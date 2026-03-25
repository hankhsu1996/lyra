#include "lyra/llvm_backend/forwarding_analysis.hpp"

#include <algorithm>
#include <cstdint>
#include <functional>
#include <unordered_set>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto ResolveSignalToGlobalSlot(const mir::SignalRef& signal, uint32_t slot_base)
    -> uint32_t {
  return (signal.scope == mir::SignalRef::Scope::kModuleLocal)
             ? slot_base + signal.id
             : signal.id;
}

// MAINTENANCE: When ConnectionKernelEntry gains any slot-bearing field,
// update this classifier in the same change.
//
// Currently classified fields:
//   - src_slot   -> downstream_src_uses (allowed downstream role)
//   - dst_slot   -> upstream_conn_indices (allowed upstream role)
//   - trigger_slot -> trigger_conn_indices, and cross-checked against
//                     src_slot to detect non-downstream trigger-only roles
//
// trigger_observation is not a slot field, but if it represents sub-slot
// observation (byte_size > 0), it sets full_slot_trigger = false for the
// relevant downstream src use.
//
// The only allowed roles for a connection-relay candidate in this cut:
//   1. dst_slot in exactly one upstream connection (the single writer)
//   2. src_slot in downstream connections (relay read)
//   3. trigger_slot in the same connection as a src_slot use (self-trigger)
// Any other appearance sets has_any_other_connection_role or
// has_non_downstream_trigger_role.
void ClassifyConnectionKernelSlotUses(
    const ConnectionKernelEntry& entry, uint32_t conn_index,
    std::vector<SlotUsageSummary>& summaries) {
  auto dst_id = entry.dst_slot.value;
  auto src_id = entry.src_slot.value;
  auto trigger_id = entry.trigger_slot.value;

  // dst_slot: this connection writes to this slot
  summaries[dst_id].upstream_conn_indices.push_back(conn_index);

  // src_slot: this connection reads from this slot
  bool self_triggered = (src_id == trigger_id);
  bool full_slot = !entry.trigger_observation.has_value() ||
                   entry.trigger_observation->byte_size == 0;

  summaries[src_id].downstream_src_uses.push_back({
      .conn_index = conn_index,
      .self_triggered = self_triggered,
      .full_slot_trigger = full_slot,
  });

  // trigger_slot: this connection triggers on this slot's changes.
  summaries[trigger_id].trigger_conn_indices.push_back(conn_index);

  // Detect non-downstream trigger-only roles: if trigger_slot != src_slot,
  // then this trigger is not a downstream self-trigger for the trigger slot.
  // The trigger slot has an independent observation role in this connection.
  if (trigger_id != src_id) {
    summaries[trigger_id].has_non_downstream_trigger_role = true;
  }
}

}  // namespace

auto CollectTriggerSlotSummary(
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena) -> TriggerSlotSummary {
  TriggerSlotSummary summary;

  // Helper: extract trigger slot IDs from Wait terminators in a process.
  auto collect_from_process = [&](const mir::Process& process,
                                  uint32_t slot_base) {
    for (const auto& block : process.blocks) {
      const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
      if (wait == nullptr) continue;
      for (const auto& trigger : wait->triggers) {
        uint32_t global_id =
            ResolveSignalToGlobalSlot(trigger.signal, slot_base);
        summary.non_connection_trigger_slots.insert(global_id);
      }
    }
  };

  // Init processes (design-level, kDesignGlobal signals, slot_base=0)
  for (const auto& proc_id : design.init_processes) {
    const auto& process = design_arena[proc_id];
    collect_from_process(process, 0);
  }

  // Module body processes
  for (const auto& plan : module_plans) {
    const auto& body = design.module_bodies.at(plan.body_id.value);
    for (const auto& proc_id : plan.body_processes) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;
      collect_from_process(process, plan.design_state_base_slot);
    }
  }

  return summary;
}

auto BuildSlotUsageSummaries(
    std::span<const ConnectionKernelEntry> connections,
    const TriggerSlotSummary& trigger_summary, uint32_t num_slots)
    -> std::vector<SlotUsageSummary> {
  std::vector<SlotUsageSummary> summaries(num_slots);

  // Classify connection kernel slot uses
  for (uint32_t ci = 0; ci < connections.size(); ++ci) {
    ClassifyConnectionKernelSlotUses(connections[ci], ci, summaries);
  }

  // Apply pre-collected non-connection trigger slot roles.
  // The combined set is represented via is_process_trigger only for the
  // first cut. The predicate rejects both is_comb_trigger and
  // is_process_trigger, so a single flag suffices. When the next cut
  // needs comb-vs-process distinction, split the summary accordingly.
  for (uint32_t slot_id : trigger_summary.non_connection_trigger_slots) {
    if (slot_id < num_slots) {
      summaries[slot_id].is_process_trigger = true;
    }
  }

  return summaries;
}

auto IsCanonicalForwardableConnectionRelay(
    mir::SlotId /*slot_id*/, const SlotUsageSummary& summary) -> bool {
  // Exactly one upstream connection writes to this slot
  if (summary.upstream_conn_indices.size() != 1) return false;

  // At least one downstream src use
  if (summary.downstream_src_uses.empty()) return false;

  // Every downstream src use is self-triggered with full-slot observation
  if (!std::ranges::all_of(
          summary.downstream_src_uses, [](const DownstreamSrcUse& use) {
            return use.self_triggered && use.full_slot_trigger;
          })) {
    return false;
  }

  // No non-downstream trigger-only roles
  if (summary.has_non_downstream_trigger_role) return false;

  // No unclassified connection roles
  if (summary.has_any_other_connection_role) return false;

  // No comb trigger or process trigger roles
  if (summary.is_comb_trigger) return false;
  if (summary.is_process_trigger) return false;

  return true;
}

auto BuildForwardingMap(
    std::span<const ConnectionKernelEntry> connections,
    const TriggerSlotSummary& trigger_summary, uint32_t num_slots)
    -> ForwardingMap {
  auto summaries =
      BuildSlotUsageSummaries(connections, trigger_summary, num_slots);

  // First pass: identify candidates and their immediate canonical source.
  struct Candidate {
    mir::SlotId slot;
    mir::SlotId immediate_canonical;
  };
  std::vector<Candidate> candidates;

  for (uint32_t slot_id = 0; slot_id < num_slots; ++slot_id) {
    auto sid = mir::SlotId{slot_id};
    if (!IsCanonicalForwardableConnectionRelay(sid, summaries[slot_id])) {
      continue;
    }
    uint32_t upstream_ci = summaries[slot_id].upstream_conn_indices[0];
    mir::SlotId upstream_src = connections[upstream_ci].src_slot;
    candidates.push_back({.slot = sid, .immediate_canonical = upstream_src});
  }

  // Second pass: flatten transitive chains with cycle detection.
  struct SlotHash {
    auto operator()(mir::SlotId id) const noexcept -> size_t {
      return std::hash<uint32_t>{}(id.value);
    }
  };
  std::unordered_map<mir::SlotId, mir::SlotId, SlotHash> immediate;
  for (const auto& c : candidates) {
    immediate[c.slot] = c.immediate_canonical;
  }

  ForwardingMap map;
  for (const auto& c : candidates) {
    mir::SlotId canonical = c.immediate_canonical;
    std::unordered_set<uint32_t> visited;
    visited.insert(c.slot.value);
    while (immediate.contains(canonical)) {
      if (visited.contains(canonical.value)) {
        throw common::InternalError(
            "BuildForwardingMap", "forwarding cycle detected");
      }
      visited.insert(canonical.value);
      canonical = immediate[canonical];
    }
    map.AddAlias(c.slot, canonical);
  }

  return map;
}

// Shared observation comparison helpers used by both merge equality and sort.

auto ResolvedObservationEqual(
    const std::optional<ResolvedObservation>& a,
    const std::optional<ResolvedObservation>& b) -> bool {
  if (a.has_value() != b.has_value()) return false;
  if (!a.has_value()) return true;
  return a->byte_offset == b->byte_offset && a->byte_size == b->byte_size &&
         a->bit_index == b->bit_index;
}

auto ResolvedObservationLess(
    const std::optional<ResolvedObservation>& a,
    const std::optional<ResolvedObservation>& b) -> bool {
  if (!a.has_value() && !b.has_value()) return false;
  if (!a.has_value()) return true;
  if (!b.has_value()) return false;
  if (a->byte_offset != b->byte_offset) return a->byte_offset < b->byte_offset;
  if (a->byte_size != b->byte_size) return a->byte_size < b->byte_size;
  return a->bit_index < b->bit_index;
}

// Full semantic comparator for sort/merge. Uses exactly the same field set
// as ConnectionKernelEntriesSemanticallyEquivalentForMerge.
auto ConnectionKernelEntrySemanticLess(
    const ConnectionKernelEntry& a, const ConnectionKernelEntry& b) -> bool {
  if (a.process_id != b.process_id) return a.process_id < b.process_id;
  if (a.src_slot != b.src_slot) return a.src_slot < b.src_slot;
  if (a.dst_slot != b.dst_slot) return a.dst_slot < b.dst_slot;
  if (a.trigger_slot != b.trigger_slot) return a.trigger_slot < b.trigger_slot;
  if (a.trigger_edge != b.trigger_edge) {
    return static_cast<uint8_t>(a.trigger_edge) <
           static_cast<uint8_t>(b.trigger_edge);
  }
  if (a.origin != b.origin) {
    return static_cast<uint8_t>(a.origin) < static_cast<uint8_t>(b.origin);
  }
  return ResolvedObservationLess(a.trigger_observation, b.trigger_observation);
}

// MAINTENANCE: Step 3 assumes the only slot-bearing fields in
// ConnectionKernelEntry are src_slot, dst_slot, and trigger_slot.
// If ConnectionKernelEntry gains another slot-bearing field, both
// canonicalization and post-condition checks must be updated in the
// same change.

auto CanonicalizeConnectionKernelEntry(
    const ConnectionKernelEntry& entry, const ForwardingMap& map)
    -> ConnectionKernelEntry {
  // This rewrite is valid because first-cut candidates guarantee that
  // downstream trigger use is same-slot self-trigger with full-slot
  // observation (ensured by IsCanonicalForwardableConnectionRelay in
  // Step 2). Resolving trigger_slot to the canonical source preserves
  // the observation semantics.
  return {
      .process_id = entry.process_id,
      .src_slot = map.Resolve(entry.src_slot),
      .dst_slot = map.Resolve(entry.dst_slot),
      .trigger_slot = map.Resolve(entry.trigger_slot),
      .trigger_edge = entry.trigger_edge,
      .trigger_observation = entry.trigger_observation,
      .origin = entry.origin,
  };
}

auto ConnectionKernelEntriesSemanticallyEquivalentForMerge(
    const ConnectionKernelEntry& a, const ConnectionKernelEntry& b) -> bool {
  if (a.process_id != b.process_id) return false;
  if (a.src_slot != b.src_slot) return false;
  if (a.dst_slot != b.dst_slot) return false;
  if (a.trigger_slot != b.trigger_slot) return false;
  if (a.trigger_edge != b.trigger_edge) return false;
  if (a.origin != b.origin) return false;
  return ResolvedObservationEqual(a.trigger_observation, b.trigger_observation);
}

auto MergeCanonicalConnectionKernelEntries(
    std::vector<ConnectionKernelEntry> entries)
    -> std::vector<ConnectionKernelEntry> {
  if (entries.size() <= 1) return entries;

  // Stable-sort by full semantic key (same fields as merge equality).
  std::ranges::stable_sort(entries, ConnectionKernelEntrySemanticLess);

  // Merge adjacent duplicates.
  std::vector<ConnectionKernelEntry> merged;
  merged.push_back(entries[0]);
  for (size_t i = 1; i < entries.size(); ++i) {
    if (!ConnectionKernelEntriesSemanticallyEquivalentForMerge(
            merged.back(), entries[i])) {
      merged.push_back(entries[i]);
    }
  }
  return merged;
}

auto RebuildCanonicalConnections(
    std::vector<ConnectionKernelEntry> connections, const ForwardingMap& map)
    -> std::vector<ConnectionKernelEntry> {
  if (map.Empty()) return connections;

  // Canonicalize every entry.
  for (auto& entry : connections) {
    entry = CanonicalizeConnectionKernelEntry(entry, map);
  }

  // Drop identity connections (src == dst after resolution).
  std::erase_if(connections, [](const ConnectionKernelEntry& e) {
    return e.src_slot == e.dst_slot;
  });

  if (connections.empty()) return connections;

  // Merge duplicates.
  connections = MergeCanonicalConnectionKernelEntries(std::move(connections));

  // Post-condition: no forwarded alias in any slot-bearing field.
  // MAINTENANCE: if ConnectionKernelEntry gains another slot-bearing field,
  // this check must be updated in the same change.
  for (const auto& entry : connections) {
    if (map.IsForwardedAlias(entry.src_slot) ||
        map.IsForwardedAlias(entry.dst_slot) ||
        map.IsForwardedAlias(entry.trigger_slot)) {
      throw common::InternalError(
          "RebuildCanonicalConnections",
          "forwarded alias survived in canonicalized connection");
    }
  }

  return connections;
}

void VerifyCanonicalizedConnectionObservations(
    const ForwardingMap& map,
    std::span<const ConnectionKernelEntry> canonicalized) {
  for (const auto& entry : canonicalized) {
    if (map.IsForwardedAlias(entry.src_slot)) {
      throw common::InternalError(
          "VerifyCanonicalizedConnectionObservations",
          std::format(
              "canonicalized connection src_slot {} is a forwarded alias",
              entry.src_slot.value));
    }
    if (map.IsForwardedAlias(entry.dst_slot)) {
      throw common::InternalError(
          "VerifyCanonicalizedConnectionObservations",
          std::format(
              "canonicalized connection dst_slot {} is a forwarded alias",
              entry.dst_slot.value));
    }
    if (map.IsForwardedAlias(entry.trigger_slot)) {
      throw common::InternalError(
          "VerifyCanonicalizedConnectionObservations",
          std::format(
              "canonicalized connection trigger_slot {} is a forwarded alias",
              entry.trigger_slot.value));
    }
  }
}

}  // namespace lyra::lowering::mir_to_llvm
