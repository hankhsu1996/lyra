#include "lyra/llvm_backend/connection_analysis.hpp"

#include <algorithm>
#include <cstdint>
#include <unordered_map>

#include "lyra/common/overloaded.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto ResolveSignalToGlobalSlot(const mir::SignalRef& signal, uint32_t slot_base)
    -> uint32_t {
  return (signal.scope == mir::SignalRef::Scope::kModuleLocal)
             ? slot_base + signal.id
             : signal.id;
}

void ClassifyConnectionKernelSlotUses(
    const ConnectionKernelEntry& entry, uint32_t conn_index,
    std::vector<SlotUsageSummary>& summaries) {
  auto dst_id = entry.dst_slot.value;
  auto src_id = entry.src_slot.value;
  auto trigger_id = entry.trigger_slot.value;

  summaries[dst_id].upstream_conn_indices.push_back(conn_index);

  bool self_triggered = (src_id == trigger_id);
  bool full_slot = !entry.trigger_observation.has_value() ||
                   entry.trigger_observation->byte_size == 0;

  summaries[src_id].downstream_src_uses.push_back({
      .conn_index = conn_index,
      .self_triggered = self_triggered,
      .full_slot_trigger = full_slot,
  });

  summaries[trigger_id].trigger_conn_indices.push_back(conn_index);

  if (trigger_id != src_id) {
    summaries[trigger_id].has_non_downstream_trigger_role = true;
  }
}

}  // namespace

auto CollectTriggerSlotSummary(
    std::span<const LayoutModulePlan> module_plans,
    std::span<const std::span<const mir::ProcessId>> module_body_processes,
    const mir::Design& design, const mir::Arena& design_arena)
    -> TriggerSlotSummary {
  TriggerSlotSummary summary;

  auto collect_triggers = [&](const mir::Process& process, uint32_t slot_base) {
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

  auto collect_reads = [&](const mir::Process& process, uint32_t slot_base,
                           const mir::Arena& arena) {
    auto record_operand = [&](const mir::Operand& op) {
      if (op.kind != mir::Operand::Kind::kUse) return;
      auto place_id = std::get<mir::PlaceId>(op.payload);
      const auto& place = arena[place_id];
      if (place.root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
        summary.process_body_read_slots.insert(
            slot_base + static_cast<uint32_t>(place.root.id));
      } else if (place.root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
        summary.process_body_read_slots.insert(
            static_cast<uint32_t>(place.root.id));
      }
    };

    auto record_format_ops = [&](const std::vector<mir::FormatOp>& ops) {
      for (const auto& fop : ops) {
        if (fop.value) record_operand(*fop.value);
      }
    };

    auto record_effect = [&](const mir::EffectOp& op) {
      std::visit(
          common::Overloaded{
              [&](const mir::DisplayEffect& d) {
                record_format_ops(d.ops);
                if (d.descriptor) record_operand(*d.descriptor);
              },
              [&](const mir::ReportEffect& r) { record_format_ops(r.ops); },
              [&](const mir::MonitorEffect& m) {
                record_format_ops(m.format_ops);
              },
              [&](const mir::SystemTfEffect& s) {
                for (const auto& a : s.args) record_operand(a);
              },
              [&](const mir::FillPackedEffect& f) {
                record_operand(f.fill_value);
              },
              [&](const mir::EnqueueDeferredAssertionEffect& e) {
                for (const auto& v : e.snapshot_values) record_operand(v);
              },
              [&](const mir::RecordDecisionObservationDynamic& r) {
                record_operand(r.match_class);
                record_operand(r.selected_kind);
                record_operand(r.selected_arm);
              },
              [&](const mir::MemIOEffect& m) {
                record_operand(m.filename.operand);
                if (m.start_addr) record_operand(*m.start_addr);
                if (m.end_addr) record_operand(*m.end_addr);
              },
              [](const auto&) {},
          },
          op);
    };

    for (const auto& block : process.blocks) {
      for (const auto& stmt : block.statements) {
        mir::ForEachOperand(stmt.data, record_operand);
        if (const auto* eff = std::get_if<mir::Effect>(&stmt.data)) {
          record_effect(eff->op);
        }
      }
      mir::ForEachLocalOperand(block.terminator, record_operand);
    }
  };

  for (const auto& proc_id : design.init_processes) {
    const auto& process = design_arena[proc_id];
    collect_triggers(process, 0);
    collect_reads(process, 0, design_arena);
  }

  for (size_t mi = 0; mi < module_plans.size(); ++mi) {
    const auto& plan = module_plans[mi];
    const auto& body = *plan.body;
    for (const auto& proc_id : module_body_processes[mi]) {
      const auto& process = body.arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;
      collect_triggers(process, plan.design_state_base_slot);
      collect_reads(process, plan.design_state_base_slot, body.arena);
    }
  }

  return summary;
}

auto BuildSlotUsageSummaries(
    std::span<const ConnectionKernelEntry> connections,
    const TriggerSlotSummary& trigger_summary, uint32_t num_slots)
    -> std::vector<SlotUsageSummary> {
  std::vector<SlotUsageSummary> summaries(num_slots);

  for (uint32_t ci = 0; ci < connections.size(); ++ci) {
    ClassifyConnectionKernelSlotUses(connections[ci], ci, summaries);
  }

  for (uint32_t slot_id : trigger_summary.non_connection_trigger_slots) {
    if (slot_id < num_slots) {
      summaries[slot_id].is_non_connection_trigger = true;
    }
  }

  for (uint32_t slot_id : trigger_summary.process_body_read_slots) {
    if (slot_id < num_slots) {
      summaries[slot_id].is_process_body_read = true;
    }
  }

  return summaries;
}

auto IsTrivialRelayCandidate(const SlotUsageSummary& summary) -> bool {
  if (summary.upstream_conn_indices.size() != 1) return false;
  if (summary.downstream_src_uses.empty()) return false;

  if (!std::ranges::all_of(
          summary.downstream_src_uses, [](const DownstreamSrcUse& use) {
            return use.self_triggered && use.full_slot_trigger;
          })) {
    return false;
  }

  if (summary.has_non_downstream_trigger_role) return false;
  if (summary.is_non_connection_trigger) return false;
  if (summary.is_process_body_read) return false;
  if (summary.is_container) return false;

  return true;
}

namespace {

// Detect identity-copy comb processes: single-block looping processes
// with exactly one Assign statement that copies one module slot to
// another, triggered by the source slot via anychange Wait.
auto CollectIdentityCopyCombs(std::span<const LayoutModulePlan> module_plans)
    -> std::vector<IdentityCopyComb> {
  std::vector<IdentityCopyComb> results;

  for (uint32_t mi = 0; mi < module_plans.size(); ++mi) {
    const auto& plan = module_plans[mi];
    const auto& body = *plan.body;
    uint32_t slot_base = plan.design_state_base_slot;

    for (uint32_t pi = 0; pi < plan.body_processes.size(); ++pi) {
      const auto& process = body.arena[plan.body_processes[pi]];
      if (process.kind != mir::ProcessKind::kLooping) continue;
      if (process.blocks.size() != 1) continue;

      const auto& block = process.blocks[0];
      if (block.statements.size() != 1) continue;

      const auto* assign = std::get_if<mir::Assign>(&block.statements[0].data);
      if (assign == nullptr) continue;

      // Destination must be a module slot.
      const auto* dst_place_id = std::get_if<mir::PlaceId>(&assign->dest);
      if (dst_place_id == nullptr) continue;
      const auto& dst_place = body.arena[*dst_place_id];
      if (dst_place.root.kind != mir::PlaceRoot::Kind::kModuleSlot) continue;
      if (!dst_place.projections.empty()) continue;

      // RHS must be a simple use of another module slot (no projections).
      const auto* rhs_op = std::get_if<mir::Operand>(&assign->rhs);
      if (rhs_op == nullptr) continue;
      if (rhs_op->kind != mir::Operand::Kind::kUse) continue;
      auto src_place_id = std::get<mir::PlaceId>(rhs_op->payload);
      const auto& src_place = body.arena[src_place_id];
      if (src_place.root.kind != mir::PlaceRoot::Kind::kModuleSlot) continue;
      if (!src_place.projections.empty()) continue;

      // Terminator must be a Wait on the source slot with kAnyChange.
      const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
      if (wait == nullptr) continue;
      if (wait->triggers.size() != 1) continue;
      const auto& trigger = wait->triggers[0];
      if (trigger.edge != common::EdgeKind::kAnyChange) continue;
      if (trigger.signal.scope != mir::SignalRef::Scope::kModuleLocal) continue;
      if (trigger.signal.id != static_cast<uint32_t>(src_place.root.id)) {
        continue;
      }

      uint32_t src_global =
          slot_base + static_cast<uint32_t>(src_place.root.id);
      uint32_t dst_global =
          slot_base + static_cast<uint32_t>(dst_place.root.id);

      results.push_back(
          IdentityCopyComb{
              .src_slot = common::SlotId{src_global},
              .dst_slot = common::SlotId{dst_global},
              .trigger_slot = common::SlotId{src_global},
          });
    }
  }

  return results;
}

}  // namespace

auto AnalyzeConnections(
    std::vector<ConnectionKernelEntry> kernel_entries,
    std::span<const LayoutModulePlan> module_plans,
    std::span<const std::span<const mir::ProcessId>> module_body_processes,
    const mir::Design& design, const mir::Arena& design_arena,
    uint32_t expanded_num_slots) -> ConnectionAnalysisResult {
  auto num_slots = expanded_num_slots > 0
                       ? expanded_num_slots
                       : static_cast<uint32_t>(design.slots.size());

  auto trigger_summary = CollectTriggerSlotSummary(
      module_plans, module_body_processes, design, design_arena);

  auto identity_combs = CollectIdentityCopyCombs(module_plans);

  auto usage_summaries =
      BuildSlotUsageSummaries(kernel_entries, trigger_summary, num_slots);

  // Mark container slots as non-eliminable. Package-level slots come from
  // design.slots; instance-local slots come from module body slots.
  auto num_package_slots = static_cast<uint32_t>(design.slots.size());
  for (uint32_t s = 0; s < num_package_slots && s < num_slots; ++s) {
    if (design.slots[s].IsOwnedContainer()) {
      usage_summaries[s].is_container = true;
    }
  }
  for (const auto& plan : module_plans) {
    const auto& body_slots = plan.body->slots;
    for (uint32_t ls = 0; ls < body_slots.size(); ++ls) {
      uint32_t global = plan.design_state_base_slot + ls;
      if (global < num_slots && body_slots[ls].IsOwnedContainer()) {
        usage_summaries[global].is_container = true;
      }
    }
  }

  // For identity-copy combs, mark the output slot as having an upstream
  // writer and the input slot as having a downstream consumer. This lets
  // IsTrivialRelayCandidate classify comb-backed pass-throughs alongside
  // connection-backed relays, without injecting synthetic connections.
  for (const auto& ic : identity_combs) {
    auto dst = ic.dst_slot.value;
    auto src = ic.src_slot.value;
    if (dst < num_slots) {
      // The comb writes to dst -- count as an upstream writer.
      // Use a sentinel index (UINT32_MAX) to distinguish from real
      // connection indices.
      usage_summaries[dst].upstream_conn_indices.push_back(UINT32_MAX);
    }
    if (src < num_slots) {
      // The comb reads from src -- this is a downstream use of src,
      // self-triggered (src == trigger) and full-slot.
      usage_summaries[src].downstream_src_uses.push_back({
          .conn_index = UINT32_MAX,
          .self_triggered = true,
          .full_slot_trigger = true,
      });
    }
  }

  std::vector<bool> relay_candidates(num_slots, false);
  for (uint32_t i = 0; i < num_slots; ++i) {
    relay_candidates[i] = IsTrivialRelayCandidate(usage_summaries[i]);
  }

  return ConnectionAnalysisResult{
      .connection_edges = std::move(kernel_entries),
      .slot_usage = std::move(usage_summaries),
      .is_relay_candidate = std::move(relay_candidates),
      .identity_copy_combs = std::move(identity_combs),
  };
}

auto EliminateRelayConnections(ConnectionAnalysisResult& analysis) -> uint32_t {
  auto& edges = analysis.connection_edges;
  const auto& slot_usage = analysis.slot_usage;
  const auto& is_relay = analysis.is_relay_candidate;

  // Build dst_slot -> identity-copy-comb lookup for comb-backed relays.
  std::unordered_map<uint32_t, const IdentityCopyComb*> comb_by_dst;
  for (const auto& ic : analysis.identity_copy_combs) {
    comb_by_dst[ic.dst_slot.value] = &ic;
  }

  std::vector<bool> edge_deleted(edges.size(), false);
  uint32_t eliminated = 0;

  for (uint32_t slot = 0; slot < is_relay.size(); ++slot) {
    if (!is_relay[slot]) continue;

    const auto& summary = slot_usage[slot];

    // Safety: exactly 1 upstream writer.
    if (summary.upstream_conn_indices.size() != 1) continue;
    uint32_t up_idx = summary.upstream_conn_indices[0];

    // Determine upstream source: either a real connection edge or an
    // identity-copy comb (sentinel UINT32_MAX).
    bool upstream_is_comb = (up_idx == UINT32_MAX);
    common::SlotId upstream_src{};
    common::SlotId upstream_trigger{};
    common::EdgeKind upstream_edge = common::EdgeKind::kAnyChange;

    if (upstream_is_comb) {
      auto it = comb_by_dst.find(slot);
      if (it == comb_by_dst.end()) continue;
      upstream_src = it->second->src_slot;
      upstream_trigger = it->second->trigger_slot;
    } else {
      if (edge_deleted[up_idx]) continue;
      const auto& up_edge = edges[up_idx];
      if (up_edge.origin != metadata::ConnectionKernelOrigin::kPortBinding) {
        continue;
      }
      if (up_edge.trigger_observation.has_value() &&
          up_edge.trigger_observation->byte_size > 0) {
        continue;
      }
      upstream_src = up_edge.src_slot;
      upstream_trigger = up_edge.trigger_slot;
      upstream_edge = up_edge.trigger_edge;
    }

    // Check all downstream edges (must be real connections, not combs).
    bool all_safe = true;
    for (const auto& down_use : summary.downstream_src_uses) {
      if (down_use.conn_index == UINT32_MAX) {
        // Downstream is another comb -- skip this relay (chained combs
        // not handled in this first cut).
        all_safe = false;
        break;
      }
      if (edge_deleted[down_use.conn_index]) {
        all_safe = false;
        break;
      }
      const auto& down_edge = edges[down_use.conn_index];
      if (down_edge.origin != metadata::ConnectionKernelOrigin::kPortBinding) {
        all_safe = false;
        break;
      }
      if (!down_use.self_triggered || !down_use.full_slot_trigger) {
        all_safe = false;
        break;
      }
    }
    if (!all_safe) continue;

    // Rewrite downstream connection edges to read from upstream source.
    for (const auto& down_use : summary.downstream_src_uses) {
      if (down_use.conn_index == UINT32_MAX) continue;
      auto& down_edge = edges[down_use.conn_index];
      down_edge.src_slot = upstream_src;
      down_edge.trigger_slot = upstream_trigger;
      down_edge.trigger_edge = upstream_edge;
      down_edge.trigger_observation = std::nullopt;
    }

    // Delete the upstream edge if it is a real connection.
    // Comb-backed upstreams continue to execute inline (cheaper than
    // full process activation) -- only the downstream routing is removed.
    if (!upstream_is_comb) {
      edge_deleted[up_idx] = true;
    }
    ++eliminated;
  }

  // Compact: remove deleted edges.
  if (eliminated > 0) {
    std::vector<ConnectionKernelEntry> surviving;
    surviving.reserve(edges.size());
    for (uint32_t i = 0; i < edges.size(); ++i) {
      if (!edge_deleted[i]) {
        surviving.push_back(std::move(edges[i]));
      }
    }
    edges = std::move(surviving);
  }

  return eliminated;
}

}  // namespace lyra::lowering::mir_to_llvm
