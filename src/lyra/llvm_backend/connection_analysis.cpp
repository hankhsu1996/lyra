#include "lyra/llvm_backend/connection_analysis.hpp"

#include <algorithm>
#include <cstdint>

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
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena) -> TriggerSlotSummary {
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

  for (const auto& plan : module_plans) {
    const auto& body = *plan.body;
    for (const auto& proc_id : plan.body_processes) {
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

  return true;
}

auto AnalyzeConnections(
    std::vector<ConnectionKernelEntry> kernel_entries,
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena, uint32_t expanded_num_slots)
    -> ConnectionAnalysisResult {
  auto num_slots = expanded_num_slots > 0
                       ? expanded_num_slots
                       : static_cast<uint32_t>(design.slots.size());

  auto trigger_summary =
      CollectTriggerSlotSummary(module_plans, design, design_arena);

  auto usage_summaries =
      BuildSlotUsageSummaries(kernel_entries, trigger_summary, num_slots);

  std::vector<bool> relay_candidates(num_slots, false);
  for (uint32_t i = 0; i < num_slots; ++i) {
    relay_candidates[i] = IsTrivialRelayCandidate(usage_summaries[i]);
  }

  return ConnectionAnalysisResult{
      .connection_edges = std::move(kernel_entries),
      .slot_usage = std::move(usage_summaries),
      .is_relay_candidate = std::move(relay_candidates),
  };
}

auto EliminateRelayConnections(ConnectionAnalysisResult& analysis) -> uint32_t {
  auto& edges = analysis.connection_edges;
  const auto& slot_usage = analysis.slot_usage;
  const auto& is_relay = analysis.is_relay_candidate;

  std::vector<bool> edge_deleted(edges.size(), false);
  uint32_t eliminated = 0;

  for (uint32_t slot = 0; slot < is_relay.size(); ++slot) {
    if (!is_relay[slot]) continue;

    const auto& summary = slot_usage[slot];

    // Safety: exactly 1 upstream writer.
    if (summary.upstream_conn_indices.size() != 1) continue;
    uint32_t up_idx = summary.upstream_conn_indices[0];
    if (edge_deleted[up_idx]) continue;
    const auto& up_edge = edges[up_idx];

    // Safety: upstream must be port-binding origin.
    if (up_edge.origin != metadata::ConnectionKernelOrigin::kPortBinding) {
      continue;
    }

    // Safety: upstream must be full-slot copy (no sub-slot observation).
    if (up_edge.trigger_observation.has_value() &&
        up_edge.trigger_observation->byte_size > 0) {
      continue;
    }

    // Check all downstream edges.
    bool all_safe = true;
    for (const auto& down_use : summary.downstream_src_uses) {
      if (edge_deleted[down_use.conn_index]) {
        all_safe = false;
        break;
      }
      const auto& down_edge = edges[down_use.conn_index];

      // Safety: port-binding origin.
      if (down_edge.origin != metadata::ConnectionKernelOrigin::kPortBinding) {
        all_safe = false;
        break;
      }

      // Safety: self-triggered, full-slot.
      if (!down_use.self_triggered || !down_use.full_slot_trigger) {
        all_safe = false;
        break;
      }
    }
    if (!all_safe) continue;

    // Rewrite downstream edges to read from the upstream source.
    for (const auto& down_use : summary.downstream_src_uses) {
      auto& down_edge = edges[down_use.conn_index];
      down_edge.src_slot = up_edge.src_slot;
      down_edge.trigger_slot = up_edge.trigger_slot;
      down_edge.trigger_edge = up_edge.trigger_edge;
      down_edge.trigger_observation = up_edge.trigger_observation;
    }

    // Delete the upstream edge (U -> R).
    edge_deleted[up_idx] = true;
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
