#include "lyra/interpreter/simulation_runner.hpp"

#include <cassert>
#include <cstddef>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/hierarchy_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/process_handle.hpp"
#include "lyra/interpreter/process_runner.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lir/process.hpp"
#include "lyra/mir/package.hpp"

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
      simulation_context_(context),
      trigger_manager_(context) {
  // Initialize timescale for $time/$stime/$realtime scaling
  simulation_context_.get().timescale = module.timescale;
  simulation_context_.get().global_precision_power =
      module.global_precision_power;
  // Initialize module name for $printtimescale
  simulation_context_.get().module_name = module.name;
}

// Multi-module constructor (hierarchical designs)
SimulationRunner::SimulationRunner(
    const std::vector<std::unique_ptr<lir::Module>>& modules,
    const std::vector<std::unique_ptr<mir::Package>>& packages,
    std::shared_ptr<lir::Process> package_init_process,
    std::shared_ptr<lir::LirContext> package_lir_context,
    std::vector<std::unique_ptr<lir::Function>> package_functions,
    SimulationContext& context)
    : delay_queue_(),
      active_queue_(),
      inactive_queue_(),
      nba_queue_(),
      postponed_queue_(),
      top_module_(
          *modules.back()),  // Last module is the top (dependency order)
      packages_(),
      package_init_process_(std::move(package_init_process)),
      package_lir_context_(std::move(package_lir_context)),
      package_functions_(std::move(package_functions)),
      simulation_context_(context),
      trigger_manager_(context) {
  // Initialize timescale for $time/$stime/$realtime scaling
  const auto& top = *modules.back();
  simulation_context_.get().timescale = top.timescale;
  simulation_context_.get().global_precision_power = top.global_precision_power;
  // Initialize module name for $printtimescale
  simulation_context_.get().module_name = top.name;
  // Store package references
  for (const auto& pkg : packages) {
    packages_.emplace_back(std::cref(*pkg));
  }
  // package_functions_ keeps the functions alive; linking was done at lowering
  // time
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
  // Preponed region: #1step sampling (stub - not implemented)
  ExecuteRegion(Region::kPreponed);

  // Main iteration loop (LRM 4.5)
  while (HasPendingActivity()) {
    // Active group iteration: Active -> Inactive -> NBA
    while (HasActivityInActiveGroup()) {
      ExecuteRegion(Region::kActive);
      ExecuteRegion(Region::kInactive);
      ExecuteRegion(Region::kNBA);
    }

    // Observed region: assertion evaluation (stub - not implemented)
    ExecuteRegion(Region::kObserved);

    // Reactive group iteration (stub - for program blocks)
    while (HasActivityInReactiveGroup()) {
      ExecuteRegion(Region::kReactive);
      ExecuteRegion(Region::kReInactive);
      ExecuteRegion(Region::kReNBA);
    }
  }

  // Postponed region: $strobe, $monitor
  ExecuteRegion(Region::kPostponed);

  // $monitor: check for value changes and print if needed
  CheckMonitor();
}

auto SimulationRunner::HasPendingActivity() const -> bool {
  return !active_queue_.empty() || !inactive_queue_.empty() ||
         !nba_queue_.empty();
}

auto SimulationRunner::HasActivityInActiveGroup() const -> bool {
  return !active_queue_.empty() || !inactive_queue_.empty() ||
         !nba_queue_.empty();
}

auto SimulationRunner::HasActivityInReactiveGroup() -> bool {
  // Stub: Reactive group not implemented yet (for program blocks)
  return false;
}

auto SimulationRunner::IsAllRegionEmpty() const -> bool {
  return active_queue_.empty() && inactive_queue_.empty() && nba_queue_.empty();
}

void SimulationRunner::ExecuteRegion(Region region) {
  switch (region) {
    case Region::kActive:
      // Invariant: Process all events until queue is empty.
      // Events may schedule more events to inactive_queue_ or nba_queue_.
      while (!active_queue_.empty()) {
        ExecuteOneEvent();
      }
      break;

    case Region::kInactive:
      // Invariant: Active queue is empty (all Active events processed).
      // Move #0 delayed events to active for next iteration.
      while (!inactive_queue_.empty()) {
        active_queue_.push(inactive_queue_.front());
        inactive_queue_.pop();
      }
      break;

    case Region::kNBA: {
      // Invariant: Active and inactive queues are empty.
      // Commit all nonblocking assignments and wake triggered processes.
      std::vector<VariableChangeRecord> modified_variables;

      while (!nba_queue_.empty()) {
        const auto& action = nba_queue_.front();

        auto& store = simulation_context_.get().variable_store;

        if (action.array_index.has_value()) {
          // Array element write: read array, modify element, write back
          auto idx = action.array_index.value();
          auto array = store.Read(action.symbol);
          store.UpdatePrevious(action.symbol);
          // Need mutable copy for modification
          RuntimeValue mutable_array = array;
          mutable_array.SetElement(idx, action.value);
          store.Write(action.symbol, mutable_array);
        } else {
          // Whole variable write
          store.Write(action.symbol, action.value);
        }

        modified_variables.push_back({.symbol = action.symbol});
        nba_queue_.pop();
      }

      WakeWaitingProcesses(modified_variables);
      break;
    }

    case Region::kPostponed:
      // Invariant: All active group activity complete for this time slot.
      // Execute $strobe, $monitor, etc. which observe final variable values.
      while (!postponed_queue_.empty()) {
        const auto& action = postponed_queue_.front();
        action.action();
        postponed_queue_.pop();
      }
      break;

    // Stub regions - not implemented yet
    case Region::kPreponed:
    case Region::kObserved:
    case Region::kReactive:
    case Region::kReInactive:
    case Region::kReNBA:
      // No-op for now
      break;
  }
}

void SimulationRunner::CheckMonitor() {
  auto& monitor = simulation_context_.get().active_monitor;
  if (!monitor || !monitor->enabled) {
    return;
  }

  // Call synthesized check process - it handles everything:
  // evaluate expressions, compare with previous, print if changed, update prev
  auto process = top_module_.get().FindProcess(monitor->check_process_name);
  if (process == nullptr) {
    return;
  }

  // Create temporary frame for process execution
  ProcessFrame frame;
  ProcessEffect effect;

  // Execute check process using standard process runner
  auto result = RunProcess(
      process, 0, 0, simulation_context_.get(), frame, effect,
      monitor->instance);

  // Monitor check should always complete (no delay/wait).
  // If this fails, the synthesized process contains delay/wait instructions.
  if (result.kind != ProcessResult::Kind::kComplete) {
    throw common::InternalError(
        "CheckMonitor", "monitor check process did not complete");
  }
}

void SimulationRunner::InitializePackageVariables() {
  // First, create storage with default values
  for (const auto& pkg_ref : packages_) {
    const auto& pkg = pkg_ref.get();
    for (const auto& var : pkg.variables) {
      RuntimeValue default_value =
          RuntimeValue::DefaultValueForType(var.variable.type);
      simulation_context_.get().variable_store.Initialize(
          var.variable.symbol, std::move(default_value));
    }
  }

  // Then execute the init process (if any) to set actual values
  if (package_init_process_) {
    ProcessFrame frame;
    ProcessEffect effect;
    RunProcess(
        package_init_process_, 0, 0, simulation_context_.get(), frame, effect,
        nullptr);  // nullptr = global storage
  }
}

void SimulationRunner::InitializeModuleVariables(const lir::Module& module) {
  // Initialize module variables in flat storage
  for (const auto& variable : module.variables) {
    auto initial = RuntimeValue::DefaultValueForType(variable.type);
    simulation_context_.get().variable_store.Initialize(
        variable.symbol, std::move(initial));
  }

  // Initialize ports in flat storage
  for (const auto& port : module.ports) {
    auto initial = RuntimeValue::DefaultValueForType(port.variable.type);
    simulation_context_.get().variable_store.Initialize(
        port.variable.symbol, std::move(initial));
  }
}

void SimulationRunner::ScheduleModuleProcesses(
    const lir::Module& module,
    const std::shared_ptr<HierarchyContext>& instance) {
  for (const auto& process : module.processes) {
    // Skip callback processes (e.g., monitor check) - they're called on demand
    if (process->is_callback) {
      continue;
    }

    // Create frame in centralized storage and get handle
    ProcessInstanceKey key{.process = process, .instance = instance};
    process_frames_.emplace(key, ProcessFrame{});
    ProcessHandle handle{key};

    active_queue_.push(
        {.origin = {.process = process, .instance = instance},
         .block_index = 0,
         .instruction_index = 0,
         .handle = handle});
  }
}

void SimulationRunner::ElaborateHierarchy() {
  const auto& top = top_module_.get();

  // Initialize package variables in global storage (before module variables)
  InitializePackageVariables();

  // Create top module instance context (no port bindings)
  auto top_instance = std::make_shared<HierarchyContext>(
      top.name, std::unordered_map<common::SymbolId, PortBinding>{});

  // Initialize top module variables in flat storage
  InitializeModuleVariables(top);

  // Schedule top module's processes
  ScheduleModuleProcesses(top, top_instance);

  // Store top instance for later access
  top_instance_ = top_instance;

  // Recursively elaborate submodules
  ElaborateSubmodules(top, top.name, top_instance);
}

void SimulationRunner::ElaborateSubmodules(
    const lir::Module& parent, const std::string& parent_path,
    const std::shared_ptr<HierarchyContext>& parent_instance) {
  for (const auto& submod : parent.submodules) {
    const auto* child = submod.child_module;  // Resolved at link time

    std::string instance_path = parent_path + "." + submod.instance_name;

    // Build port bindings for this instance
    // Output port bindings: child writes -> parent signal
    // (Input ports use driver processes, no bindings needed)
    std::unordered_map<common::SymbolId, PortBinding> bindings;
    const auto& symbol_table = simulation_context_.get().symbol_table;
    for (const auto& output_binding : submod.output_bindings) {
      // Find the port in child module
      const lir::Port* port_ptr = nullptr;
      for (const auto& port : child->ports) {
        if (symbol_table.Name(port.variable.symbol) ==
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
        std::make_shared<HierarchyContext>(instance_path, std::move(bindings));

    // Store child in parent for hierarchical access
    parent_instance->children[submod.instance_symbol] = child_instance;

    // Initialize child module's variables in flat storage
    InitializeModuleVariables(*child);

    // Schedule child module's processes with this instance context
    ScheduleModuleProcesses(*child, child_instance);

    // Recursively elaborate nested submodules
    ElaborateSubmodules(*child, instance_path, child_instance);
  }
}

void SimulationRunner::ExecuteOneEvent() {
  ScheduledEvent event = std::move(active_queue_.front());
  active_queue_.pop();

  const auto& origin = event.origin;
  std::size_t block_index = event.block_index;
  std::size_t instruction_index = event.instruction_index;

  simulation_context_.get().tracer.Record(
      fmt::format(
          "{} | Start at block {} instruction {}", origin.process->name,
          origin.process->blocks[block_index]->label, instruction_index));

  // Lookup frame from centralized storage using the handle.
  // This mirrors C++ coroutines where the frame is accessed via
  // coroutine_handle.
  ProcessFrame& frame = process_frames_.at(event.handle.key);
  ProcessEffect process_effect;

  auto result = RunProcess(
      origin.process, block_index, instruction_index, simulation_context_.get(),
      frame, process_effect, origin.instance);

  simulation_context_.get().tracer.Record(
      fmt::format("{} | {}", origin.process->name, result.Summary()));

  WakeWaitingProcesses(process_effect.GetModifiedVariables());

  for (const auto& action : process_effect.GetNbaActions()) {
    nba_queue_.push(action);
  }
  for (const auto& action : process_effect.GetPostponedActions()) {
    postponed_queue_.push(action);
  }

  switch (result.kind) {
    case ProcessResult::Kind::kDelay: {
      // Schedule for future time slot - frame stays in process_frames_
      auto delay_time =
          simulation_context_.get().current_time + result.delay_amount;
      ScheduledEvent delayed_event{
          .origin = event.origin,
          .block_index = result.block_index,
          .instruction_index = result.resume_instruction_index,
          .handle = event.handle};
      delay_queue_[delay_time].push_back(std::move(delayed_event));
      break;
    }

    case ProcessResult::Kind::kScheduleInactive: {
      // Schedule for Inactive region (same time slot, #0 delay)
      ScheduledEvent inactive_event{
          .origin = event.origin,
          .block_index = result.block_index,
          .instruction_index = result.resume_instruction_index,
          .handle = event.handle};
      inactive_queue_.push(std::move(inactive_event));
      break;
    }

    case ProcessResult::Kind::kWaitEvent: {
      // Register for all triggers with the SAME handle.
      // This is the key fix for compound triggers like @(a or b):
      // all triggers share the same frame reference, so whichever fires
      // first will resume the process with the correct frame.
      for (const auto& trigger : result.triggers) {
        // Unified trigger resolution:
        // 1. Traverse instance_path (empty for local triggers)
        auto binding_instance = origin.instance;
        for (const auto& elem : trigger.instance_path) {
          binding_instance = binding_instance->LookupChild(elem.symbol);
          if (!binding_instance) {
            break;  // Should not happen - validated at lowering
          }
        }

        if (binding_instance) {
          // 2. Resolve through port bindings (e.g., port -> parent signal)
          auto [target_symbol, resolved_instance] =
              binding_instance->ResolveBinding(trigger.variable);
          if (resolved_instance) {
            binding_instance = resolved_instance;
          }

          // Register with:
          // - event.handle: non-owning reference to frame in process_frames_
          // - binding_instance: where the variable lives (for trigger
          // detection) Same handle for ALL triggers - this fixes the compound
          // trigger bug!
          trigger_manager_.RegisterWaitingProcess(
              origin.process, binding_instance, target_symbol,
              trigger.edge_kind, result.block_index,
              result.resume_instruction_index, event.handle);
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
      // Process finished - clean up its frame from centralized storage.
      // This handles initial blocks that complete; always blocks typically
      // don't complete (they suspend on delay/event and loop back).
      process_frames_.erase(event.handle.key);
      break;
    }
  }
}

void SimulationRunner::WakeWaitingProcesses(
    const std::vector<VariableChangeRecord>& modified_variables) {
  // Delegate to trigger manager
  auto events_to_schedule = trigger_manager_.CheckTriggers(modified_variables);

  // Schedule all triggered events (move to preserve coroutine frames)
  for (auto& event : events_to_schedule) {
    active_queue_.push(std::move(event));
  }
}

}  // namespace lyra::interpreter
