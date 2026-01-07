#include "lyra/interpreter/simulation_runner.hpp"

#include <cstddef>
#include <memory>
#include <queue>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/interpreter/process_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_runner.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/module.hpp"

namespace lyra::interpreter {

// Single module constructor (backwards compatibility)
SimulationRunner::SimulationRunner(
    const lir::Module& module, SimulationContext& context)
    : delay_queue_(),
      active_queue_(),
      inactive_queue_(),
      nba_queue_(),
      postponed_queue_(),
      top_module_(module),
      module_map_(),
      simulation_context_(context),
      trigger_manager_(context) {
  // Initialize timescale for $time/$stime/$realtime scaling
  simulation_context_.get().timescale = module.timescale;
  simulation_context_.get().global_precision_power =
      module.global_precision_power;
  // Initialize module name for $printtimescale
  simulation_context_.get().module_name = module.name;
  // Add the single module to the map
  module_map_.emplace(module.name, std::cref(module));
}

// Multi-module constructor (hierarchical designs)
SimulationRunner::SimulationRunner(
    const std::vector<std::unique_ptr<lir::Module>>& modules,
    SimulationContext& context)
    : delay_queue_(),
      active_queue_(),
      inactive_queue_(),
      nba_queue_(),
      postponed_queue_(),
      top_module_(
          *modules.back()),  // Last module is the top (dependency order)
      module_map_(),
      simulation_context_(context),
      trigger_manager_(context) {
  // Initialize timescale for $time/$stime/$realtime scaling
  const auto& top = *modules.back();
  simulation_context_.get().timescale = top.timescale;
  simulation_context_.get().global_precision_power = top.global_precision_power;
  // Initialize module name for $printtimescale
  simulation_context_.get().module_name = top.name;
  // Build module map for lookup
  for (const auto& module : modules) {
    module_map_.emplace(module->name, std::cref(*module));
  }
}

void SimulationRunner::Run() {
  // Elaborate hierarchy: initialize variables and schedule processes
  ElaborateHierarchy();

  // Main simulation loop: continue until all queues are empty
  while (!active_queue_.empty() || !delay_queue_.empty()) {
    // Advance simulation time if no active events
    if (active_queue_.empty()) {
      auto it = delay_queue_.begin();
      simulation_context_.get().current_time = it->first;

      if (simulation_context_.get().current_time > kMaxSimulationTime) {
        throw DiagnosticException(
            Diagnostic::Error({}, "simulation time exceeded max limit"));
      }

      // Move delayed events scheduled for this time into the active queue
      for (const auto& event : it->second) {
        active_queue_.push(event);
      }
      delay_queue_.erase(it);
    }

    ExecuteTimeSlot();

    // Stop simulation if $finish was requested
    if (finish_requested_) {
      break;
    }
  }
}

void SimulationRunner::ExecuteTimeSlot() {
  ExecuteRegion(RegionType::kPreponed);
  ExecuteRegion(RegionType::kPreActive);

  while (HasPendingActivity()) {
    // Phase 1: Active phase group
    while (HasActivityInActiveGroup()) {
      ExecuteRegion(RegionType::kActive);

      if (!inactive_queue_.empty()) {
        MoveToActive(inactive_queue_);
      } else if (!nba_queue_.empty()) {
        ExecuteRegion(RegionType::kNba);
        // NBA commits might trigger more Active events
      }
    }

    // Phase 2: Reactive phase group
    while (HasActivityInReactiveGroup()) {
      ExecuteRegion(RegionType::kReactive);

      if (!inactive_queue_.empty()) {
        MoveToActive(inactive_queue_);
      }
    }

    // Phase 3: Pre-Postponed region (optional)
    if (IsAllRegionEmpty()) {
      ExecuteRegion(RegionType::kPrePostponed);
    }
  }

  // Final Phase: Postponed
  ExecuteRegion(RegionType::kPostponed);
}

auto SimulationRunner::HasPendingActivity() const -> bool {
  return !active_queue_.empty() || !inactive_queue_.empty() ||
         !nba_queue_.empty();
}

auto SimulationRunner::HasActivityInActiveGroup() const -> bool {
  return !active_queue_.empty() || !inactive_queue_.empty() ||
         !nba_queue_.empty();
}

auto SimulationRunner::HasActivityInReactiveGroup() const -> bool {
  return !active_queue_.empty() || !inactive_queue_.empty();
}

auto SimulationRunner::IsAllRegionEmpty() const -> bool {
  return active_queue_.empty() && inactive_queue_.empty() && nba_queue_.empty();
}

void SimulationRunner::MoveToActive(std::queue<ScheduledEvent>& source) {
  std::queue<ScheduledEvent> tmp;
  std::swap(tmp, source);
  while (!tmp.empty()) {
    active_queue_.push(tmp.front());
    tmp.pop();
  }
}

void SimulationRunner::ExecuteRegion(RegionType region) {
  switch (region) {
    case RegionType::kActive:
      while (!active_queue_.empty()) {
        ExecuteOneEvent();
      }
      break;

    case RegionType::kReactive:
      while (!active_queue_.empty()) {
        ExecuteOneEvent();
      }
      break;

    case RegionType::kNba: {
      std::vector<ModifiedVariable> modified_variables;

      while (!nba_queue_.empty()) {
        const auto& action = nba_queue_.front();
        if (action.instance != nullptr &&
            action.instance->Exists(action.variable)) {
          // Per-instance storage
          action.instance->Write(action.variable, action.value);
        } else {
          // Global storage
          simulation_context_.get().variable_table.Write(
              action.variable, action.value);
        }
        modified_variables.push_back(
            {.symbol = action.variable, .instance = action.instance});
        nba_queue_.pop();
      }

      WakeWaitingProcesses(modified_variables);
      break;
    }

    case RegionType::kPostponed:
      while (!postponed_queue_.empty()) {
        const auto& action = postponed_queue_.front();
        action.action();
        postponed_queue_.pop();
      }
      break;

    default:
      // Other regions not implemented yet
      break;
  }
}

void SimulationRunner::InitializeModuleVariables(
    const lir::Module& module,
    const std::shared_ptr<InstanceContext>& instance) {
  // Initialize module variables in the instance's storage
  for (const auto& variable : module.variables) {
    instance->InitializeVariable(variable);
  }

  // Initialize ports in the instance's storage (for input ports that need
  // storage)
  for (const auto& port : module.ports) {
    instance->InitializeVariable(port.variable);
  }
}

void SimulationRunner::ScheduleModuleProcesses(
    const lir::Module& module,
    const std::shared_ptr<InstanceContext>& instance) {
  for (const auto& process : module.processes) {
    active_queue_.push(
        {.process = process,
         .instance = instance,
         .block_index = 0,
         .instruction_index = 0});
  }
}

auto SimulationRunner::LookupModule(const std::string& name) const
    -> const lir::Module* {
  auto it = module_map_.find(name);
  if (it == module_map_.end()) {
    return nullptr;
  }
  return &it->second.get();
}

void SimulationRunner::PopulateSymbolLookup(
    const lir::Module& module,
    const std::shared_ptr<InstanceContext>& instance) {
  for (const auto& var : module.variables) {
    instance->symbol_by_name[std::string(var.symbol->name)] = var.symbol;
  }
  for (const auto& port : module.ports) {
    instance->symbol_by_name[std::string(port.variable.symbol->name)] =
        port.variable.symbol;
  }
}

void SimulationRunner::ElaborateHierarchy() {
  const auto& top = top_module_.get();

  // Create top module instance context (no port bindings)
  auto top_instance = std::make_shared<InstanceContext>(
      top.name, std::unordered_map<common::SymbolRef, PortBinding>{});

  // Build symbol lookup map for hierarchical access
  PopulateSymbolLookup(top, top_instance);

  // Initialize top module variables in per-instance storage
  InitializeModuleVariables(top, top_instance);

  // Schedule top module's processes
  ScheduleModuleProcesses(top, top_instance);

  // Store top instance for later access
  top_instance_ = top_instance;

  // Recursively elaborate submodules
  ElaborateSubmodules(top, top.name, top_instance);
}

void SimulationRunner::ElaborateSubmodules(
    const lir::Module& parent, const std::string& parent_path,
    const std::shared_ptr<InstanceContext>& parent_instance) {
  for (const auto& submod : parent.submodules) {
    const auto* child = LookupModule(submod.module_type);
    if (child == nullptr) {
      throw DiagnosticException(
          Diagnostic::Error(
              {}, fmt::format("module '{}' not found", submod.module_type)));
    }

    std::string instance_path = parent_path + "." + submod.instance_name;

    // Build port bindings for this instance
    // Output port bindings: child writes → parent signal
    // (Input ports use driver processes, no bindings needed)
    std::unordered_map<common::SymbolRef, PortBinding> bindings;
    for (const auto& output_binding : submod.output_bindings) {
      // Find the port in child module
      const lir::Port* port_ptr = nullptr;
      for (const auto& port : child->ports) {
        if (std::string(port.variable.symbol->name) ==
            output_binding.port_name) {
          port_ptr = &port;
          break;
        }
      }

      if (port_ptr == nullptr) {
        continue;
      }

      // Resolve the signal through parent's bindings (for nested hierarchy)
      // If parent has a binding, follow it; otherwise use parent instance
      auto [target_symbol, target_instance] =
          parent_instance->ResolveBinding(output_binding.signal);
      bindings[port_ptr->variable.symbol] = PortBinding{
          .target_symbol = target_symbol,
          .target_instance =
              target_instance ? target_instance : parent_instance};
    }

    auto child_instance =
        std::make_shared<InstanceContext>(instance_path, std::move(bindings));

    // Store child in parent for hierarchical access
    parent_instance->children[submod.instance_symbol] = child_instance;
    parent_instance->children_by_name[submod.instance_name] = child_instance;

    // Build symbol lookup map for hierarchical access
    PopulateSymbolLookup(*child, child_instance);

    // Initialize child module's variables in per-instance storage
    InitializeModuleVariables(*child, child_instance);

    // Schedule child module's processes with this instance context
    ScheduleModuleProcesses(*child, child_instance);

    // Recursively elaborate nested submodules
    ElaborateSubmodules(*child, instance_path, child_instance);
  }
}

void SimulationRunner::ExecuteOneEvent() {
  ScheduledEvent event = active_queue_.front();
  active_queue_.pop();

  auto& process = event.process;
  auto& instance = event.instance;
  std::size_t block_index = event.block_index;
  std::size_t instruction_index = event.instruction_index;

  simulation_context_.get().tracer.Record(
      fmt::format(
          "{} | Start at block {} instruction {}", process->name,
          process->blocks[block_index]->label, instruction_index));

  ProcessContext process_context;
  ProcessEffect process_effect;

  auto result = RunProcess(
      process, block_index, instruction_index, simulation_context_.get(),
      process_context, process_effect, instance);

  simulation_context_.get().tracer.Record(
      fmt::format("{} | {}", process->name, result.Summary()));

  WakeWaitingProcesses(process_effect.GetModifiedVariables());

  for (const auto& action : process_effect.GetNbaActions()) {
    nba_queue_.push(action);
  }
  for (const auto& action : process_effect.GetPostponedActions()) {
    postponed_queue_.push(action);
  }

  switch (result.kind) {
    case ProcessResult::Kind::kDelay: {
      auto delay_time =
          simulation_context_.get().current_time + result.delay_amount;
      ScheduledEvent delayed_event{
          .process = process,
          .instance = instance,
          .block_index = result.block_index,
          .instruction_index = result.resume_instruction_index};
      delay_queue_[delay_time].push_back(std::move(delayed_event));
      break;
    }

    case ProcessResult::Kind::kWaitEvent: {
      for (const auto& trigger : result.triggers) {
        // Unified trigger resolution:
        // 1. Traverse instance_path (empty for local triggers)
        auto watch_instance = instance;
        for (const auto& inst_sym : trigger.instance_path) {
          watch_instance = watch_instance->LookupChild(inst_sym);
          if (!watch_instance) {
            break;  // Should not happen - validated at lowering
          }
        }

        if (watch_instance) {
          // 2. Resolve through port bindings (e.g., port -> parent signal)
          auto [target_symbol, resolved_instance] =
              watch_instance->ResolveBinding(trigger.variable);
          if (resolved_instance) {
            watch_instance = resolved_instance;
          }

          // Register with both:
          // - instance: where the process runs (for resumption)
          // - watch_instance: where the variable lives (for trigger detection)
          trigger_manager_.RegisterWaitingProcess(
              process, instance, watch_instance, target_symbol,
              trigger.edge_kind, result.block_index,
              result.resume_instruction_index);
        }
      }
      break;
    }
    case ProcessResult::Kind::kFinish: {
      finish_requested_ = true;
      if (result.is_stop) {
        simulation_context_.get().stopped = true;
      }
      break;
    }

    case ProcessResult::Kind::kComplete: {
      break;
    }
  }
}

void SimulationRunner::WakeWaitingProcesses(
    const std::vector<ModifiedVariable>& modified_variables) {
  // Delegate to trigger manager
  auto events_to_schedule = trigger_manager_.CheckTriggers(modified_variables);

  // Schedule all triggered events
  for (const auto& event : events_to_schedule) {
    active_queue_.push(event);
  }
}

}  // namespace lyra::interpreter
