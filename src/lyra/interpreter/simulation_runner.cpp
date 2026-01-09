#include "lyra/interpreter/simulation_runner.hpp"

#include <cctype>
#include <cstddef>
#include <format>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/interpreter/instruction_runner.hpp"
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
    const std::vector<std::unique_ptr<mir::Package>>& packages,
    std::shared_ptr<lir::Process> package_init_process,
    std::shared_ptr<lir::LirContext> package_lir_context,
    SimulationContext& context)
    : delay_queue_(),
      active_queue_(),
      inactive_queue_(),
      nba_queue_(),
      postponed_queue_(),
      top_module_(
          *modules.back()),  // Last module is the top (dependency order)
      module_map_(),
      packages_(),
      package_init_process_(std::move(package_init_process)),
      package_lir_context_(std::move(package_lir_context)),
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
  // Store package references
  for (const auto& pkg : packages) {
    packages_.emplace_back(std::cref(*pkg));
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

namespace {

// Format an integral value in the specified base
auto FormatIntegral(const RuntimeValue& val, char spec) -> std::string {
  if (!val.IsTwoState()) {
    return val.ToString();
  }

  if (val.IsWide()) {
    // Wide values (>64 bits)
    const auto& wb = val.AsWideBit();
    switch (spec) {
      case 'o':
        return wb.ToOctalString();
      case 'x':
      case 'h':
        return wb.ToHexString();
      case 'b': {
        // WideBit doesn't have ToBinString, convert via hex
        auto hex = wb.ToHexString();
        std::string bin;
        for (char c : hex) {
          int digit = (c >= 'a') ? (c - 'a' + 10) : (c - '0');
          for (int bit = 3; bit >= 0; --bit) {
            bool is_set = ((digit >> bit) & 1) != 0;
            bin += is_set ? '1' : '0';
          }
        }
        // Remove leading zeros
        auto first_one = bin.find('1');
        return first_one == std::string::npos ? "0" : bin.substr(first_one);
      }
      case 'd':
      default:
        // For wide values, use hex as decimal would be very long
        return wb.ToHexString();
    }
  }

  // Narrow values (<=64 bits)
  auto narrow = val.AsNarrow();
  uint64_t uval = narrow.AsUInt64();

  switch (spec) {
    case 'b': {
      if (uval == 0) {
        return "0";
      }
      std::string bin;
      while (uval > 0) {
        bin = ((uval & 1) != 0U ? '1' : '0') + bin;
        uval >>= 1;
      }
      return bin;
    }
    case 'o':
      return std::format("{:o}", uval);
    case 'x':
    case 'h':
      return std::format("{:x}", uval);
    case 'd':
    default:
      if (narrow.is_signed) {
        return std::to_string(narrow.AsInt64());
      }
      return std::to_string(uval);
  }
}

// Simple format string substitution for $monitor output.
// Handles basic %d, %h, %b, %o, %s specifiers.
auto FormatMonitorOutput(
    const std::string& fmt, const std::vector<RuntimeValue>& args,
    char default_format) -> std::string {
  std::string result;
  size_t arg_idx = 0;
  size_t i = 0;

  while (i < fmt.size()) {
    if (fmt[i] == '%' && i + 1 < fmt.size()) {
      char spec = fmt[i + 1];
      if (spec == '%') {
        result += '%';
        i += 2;
        continue;
      }

      // Skip width specifiers like %0d
      size_t spec_start = i + 1;
      while (spec_start < fmt.size() &&
             (std::isdigit(fmt[spec_start]) != 0 || fmt[spec_start] == '-')) {
        ++spec_start;
      }
      if (spec_start < fmt.size()) {
        spec = fmt[spec_start];
      }

      if (arg_idx < args.size()) {
        const auto& val = args[arg_idx++];

        if (spec == 's' && val.IsString()) {
          result += val.AsString();
        } else if (spec == 'f' && (val.IsReal() || val.IsShortReal())) {
          result +=
              std::to_string(val.IsReal() ? val.AsDouble() : val.AsFloat());
        } else if (val.IsTwoState()) {
          result += FormatIntegral(val, spec);
        } else {
          result += val.ToString();
        }
      }
      i = spec_start + 1;
    } else {
      result += fmt[i++];
    }
  }

  // If no format specifiers found but we have args, use default format
  if (result.empty() && !args.empty()) {
    for (size_t j = 0; j < args.size(); ++j) {
      if (j > 0) {
        result += " ";
      }
      const auto& val = args[j];
      if (val.IsString()) {
        result += val.AsString();
      } else if (val.IsTwoState()) {
        result += FormatIntegral(val, default_format);
      } else {
        result += val.ToString();
      }
    }
  }

  return result;
}

}  // namespace

void SimulationRunner::CheckMonitor() {
  auto& monitor = simulation_context_.get().active_monitor;
  if (!monitor || !monitor->enabled || monitor->variables.empty()) {
    return;
  }

  // Re-evaluate all monitored expressions
  std::vector<RuntimeValue> current_values;
  current_values.reserve(monitor->variables.size());

  for (const auto& var : monitor->variables) {
    const auto& block =
        top_module_.get().GetMonitorExpressionBlock(var.expression_block_index);
    current_values.push_back(
        EvaluateMonitorExpressionBlock(block, monitor->instance));
  }

  // Check if any value changed
  if (current_values == monitor->previous_values) {
    return;
  }

  // Values changed - print the monitor output using format string
  auto& out = simulation_context_.get().display_output;
  out << FormatMonitorOutput(
      monitor->format_string, current_values, monitor->default_format);

  if (monitor->append_newline) {
    out << "\n";
  }

  // Update previous values
  monitor->previous_values = std::move(current_values);
}

auto SimulationRunner::EvaluateMonitorExpressionBlock(
    const lir::MonitorExpressionBlock& block,
    const std::shared_ptr<InstanceContext>& instance) -> RuntimeValue {
  // Create temporary context for evaluation
  ProcessContext temp_context;
  ProcessEffect
      temp_effect;  // Discarded - expressions shouldn't have side effects

  // Execute each instruction in the expression block
  for (const auto& instr : block.instructions) {
    RunInstruction(
        instr, top_module_.get(), simulation_context_.get(), temp_context,
        temp_effect, instance);
  }

  // Return the result stored in the designated temp
  return temp_context.temp_table.Read(block.result);
}

void SimulationRunner::InitializePackageVariables() {
  // First, create storage with default values
  for (const auto& pkg_ref : packages_) {
    const auto& pkg = pkg_ref.get();
    for (const auto& var : pkg.variables) {
      RuntimeValue default_value =
          RuntimeValue::DefaultValueForType(var.variable.type);
      simulation_context_.get().variable_table.CreateVariable(
          var.variable.symbol, std::move(default_value));
    }
  }

  // Then execute the init process (if any) to set actual values
  if (package_init_process_) {
    ProcessContext process_context;
    ProcessEffect process_effect;
    // Use top module for function lookup (package init has no function calls)
    RunProcess(
        package_init_process_, 0, 0, top_module_.get(),
        simulation_context_.get(), process_context, process_effect,
        nullptr);  // nullptr = global storage
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

void SimulationRunner::ElaborateHierarchy() {
  const auto& top = top_module_.get();

  // Initialize package variables in global storage (before module variables)
  InitializePackageVariables();

  // Create top module instance context (no port bindings)
  auto top_instance = std::make_shared<InstanceContext>(
      top.name, std::unordered_map<common::SymbolRef, PortBinding>{});

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
      process, block_index, instruction_index, top_module_.get(),
      simulation_context_.get(), process_context, process_effect, instance);

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
      // Schedule for future time slot
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

    case ProcessResult::Kind::kScheduleInactive: {
      // Schedule for Inactive region (same time slot, #0 delay)
      ScheduledEvent inactive_event{
          .process = process,
          .instance = instance,
          .block_index = result.block_index,
          .instruction_index = result.resume_instruction_index};
      inactive_queue_.push(std::move(inactive_event));
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
