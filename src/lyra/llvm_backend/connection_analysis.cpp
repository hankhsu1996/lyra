#include "lyra/llvm_backend/connection_analysis.hpp"

#include <algorithm>
#include <cstdint>

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

  for (const auto& proc_id : design.init_processes) {
    const auto& process = design_arena[proc_id];
    collect_from_process(process, 0);
  }

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

  for (uint32_t ci = 0; ci < connections.size(); ++ci) {
    ClassifyConnectionKernelSlotUses(connections[ci], ci, summaries);
  }

  for (uint32_t slot_id : trigger_summary.non_connection_trigger_slots) {
    if (slot_id < num_slots) {
      summaries[slot_id].is_non_connection_trigger = true;
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

  return true;
}

auto AnalyzeConnections(
    std::vector<ConnectionKernelEntry> kernel_entries,
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena) -> ConnectionAnalysisResult {
  auto num_slots = static_cast<uint32_t>(design.slots.size());

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

}  // namespace lyra::lowering::mir_to_llvm
