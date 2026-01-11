#include "lyra/interpreter/trigger_manager.hpp"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include "lyra/common/trigger.hpp"
#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_handle.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_runner.hpp"
#include "lyra/interpreter/variable_table.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::interpreter {

void TriggerManager::RegisterWaitingProcess(
    const std::shared_ptr<lir::Process>& process,
    const std::shared_ptr<InstanceContext>& watch_instance,
    const SymbolRef& variable, common::EdgeKind edge_kind,
    std::size_t block_index, std::size_t instruction_index,
    ProcessHandle handle) {
  // Use watch_instance in the key since that's where we detect changes
  ProcessInstanceKey pi_key{.process = process, .instance = watch_instance};
  wait_map_[variable].insert(pi_key);

  // Use (process, watch_instance, variable) as key to store per-variable info
  ProcessInstanceVarKey piv_key{
      .process = process, .instance = watch_instance, .variable = variable};
  wait_set_[piv_key] = {
      .watch_instance = watch_instance,  // For reading values
      .block_index = block_index,
      .instruction_index = instruction_index,
      .edge_kind = edge_kind,
      .handle = handle};
}

auto TriggerManager::CheckTriggers(
    const std::vector<ModifiedVariable>& modified_variables)
    -> std::vector<ScheduledEvent> {
  std::vector<ScheduledEvent> events_to_trigger;
  std::unordered_set<ProcessInstanceKey, ProcessInstanceKeyHash> triggered_keys;

  for (const auto& [variable, instance] : modified_variables) {
    // Read previous and current values from the appropriate source
    RuntimeValue old_value;
    RuntimeValue new_value;

    if (instance != nullptr && instance->Exists(variable)) {
      // Per-instance storage
      old_value = instance->ReadPrevious(variable);
      new_value = instance->Read(variable);
      instance->UpdatePrevious(variable, new_value);
    } else {
      // Global storage
      old_value = context_.get().variable_table.ReadPrevious(variable);
      new_value = context_.get().variable_table.Read(variable);
      context_.get().variable_table.UpdatePrevious(variable, new_value);
    }

    // Check if any process is waiting on this variable
    auto map_it = wait_map_.find(variable);
    if (map_it == wait_map_.end()) {
      continue;
    }

    std::unordered_set<ProcessInstanceKey, ProcessInstanceKeyHash> to_remove;

    for (const auto& pi_key : map_it->second) {
      // Skip if this (process, instance) is already triggered
      if (triggered_keys.contains(pi_key)) {
        continue;
      }

      // Look up with (process, instance, variable) key
      ProcessInstanceVarKey piv_key{
          .process = pi_key.process,
          .instance = pi_key.instance,
          .variable = variable};
      auto info_it = wait_set_.find(piv_key);
      if (info_it == wait_set_.end()) {
        continue;
      }

      auto& wait_info = info_it->second;

      if (ShouldTrigger(old_value, new_value, wait_info.edge_kind)) {
        // Get process_instance from the handle (where process runs)
        events_to_trigger.push_back(
            ScheduledEvent{
                .origin =
                    {.process = pi_key.process,
                     .instance = wait_info.handle.key.instance},
                .block_index = wait_info.block_index,
                .instruction_index = wait_info.instruction_index,
                .handle = wait_info.handle});
        triggered_keys.insert(pi_key);
        to_remove.insert(pi_key);
      }
    }

    // Trace triggered processes (moved outside inner loop)
    if (!to_remove.empty()) {
      std::vector<std::string> proc_names;
      for (const auto& k : to_remove) {
        proc_names.push_back(k.process->name);
      }

      std::string trace_detail = fmt::format(
          "Trigger on variable '{}': old = {}, new = {}, triggered "
          "process(es) = {}",
          variable->name, old_value.ToString(), new_value.ToString(),
          fmt::join(proc_names, ", "));

      context_.get().tracer.Record(trace_detail);
    }

    // Remove triggered keys from wait map for this variable
    for (const auto& k : to_remove) {
      map_it->second.erase(k);
    }

    if (map_it->second.empty()) {
      vars_to_remove_.push_back(variable);
    }
  }

  // Clean up empty maps
  for (const auto& var : vars_to_remove_) {
    wait_map_.erase(var);
  }
  vars_to_remove_.clear();

  // Remove all triggered (process, instance, variable) entries from wait_set
  // When a process triggers, we need to remove ALL its wait entries
  // (e.g., for @(posedge clk or negedge rst), triggering on clk should
  // also remove the rst entry)
  std::vector<ProcessInstanceVarKey> piv_keys_to_remove;
  for (const auto& [piv_key, _] : wait_set_) {
    ProcessInstanceKey pi_key{
        .process = piv_key.process, .instance = piv_key.instance};
    if (triggered_keys.contains(pi_key)) {
      piv_keys_to_remove.push_back(piv_key);
    }
  }
  for (const auto& piv_key : piv_keys_to_remove) {
    wait_set_.erase(piv_key);
  }

  return events_to_trigger;
}

auto TriggerManager::ShouldTrigger(
    const RuntimeValue& old_value, const RuntimeValue& new_value,
    common::EdgeKind edge_kind) -> bool {
  if (edge_kind == common::EdgeKind::kAnyChange) {
    return old_value != new_value;
  }

  if (old_value.IsTwoState() && new_value.IsTwoState()) {
    // Extract bit 0 for edge detection
    auto get_bit0 = [](const RuntimeValue& v) -> uint64_t {
      if (v.IsWide()) {
        return v.AsWideBit().GetBit(0);
      }
      return v.AsNarrow().AsUInt64() & 1;
    };
    uint64_t old_bit0 = get_bit0(old_value);
    uint64_t new_bit0 = get_bit0(new_value);

    switch (edge_kind) {
      case common::EdgeKind::kPosedge:
        return (old_bit0 == 0 && new_bit0 == 1);
      case common::EdgeKind::kNegedge:
        return (old_bit0 == 1 && new_bit0 == 0);
      case common::EdgeKind::kBothEdge:
        return (old_bit0 != new_bit0);
      default:
        return false;
    }
  }

  return false;
}

}  // namespace lyra::interpreter
