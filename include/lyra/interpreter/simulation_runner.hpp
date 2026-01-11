#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <queue>
#include <string>
#include <unordered_map>
#include <vector>

#include "lyra/common/simulation_region.hpp"
#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/process_handle.hpp"
#include "lyra/interpreter/trigger_manager.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lir/process.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::interpreter {

using common::Region;

using SimulationTime = uint64_t;

// Max simulation time in internal ticks (1 million allows ~1ms at 1ns
// precision)
constexpr SimulationTime kMaxSimulationTime = 1'000'000;

using ProcessPtr = std::shared_ptr<lir::Process>;

// Identifies where a process runs.
// Module for function lookup is accessed via instance->module.
struct ProcessOrigin {
  ProcessPtr process;
  std::shared_ptr<InstanceContext> instance;  // Like C++ 'this' pointer
};

/// ScheduledEvent represents a process scheduled to execute.
/// The `handle` field is a non-owning reference to the ProcessFrame,
/// which lives in SimulationRunner::process_frames_. This mirrors C++
/// coroutines where coroutine_handle<> is a non-owning pointer.
struct ScheduledEvent {
  ProcessOrigin origin;
  std::size_t block_index = 0;
  std::size_t instruction_index = 0;
  ProcessHandle handle;
};

using DelayQueue = std::map<SimulationTime, std::vector<ScheduledEvent>>;
using ActiveQueue = std::queue<ScheduledEvent>;
using InactiveQueue = std::queue<ScheduledEvent>;
using NbaQueue = std::queue<NbaAction>;
using PostponedQueue = std::queue<PostponedAction>;

// WaitingProcessInfo is defined in trigger_manager.hpp
// Region enum is defined in lyra/common/simulation_region.hpp

class SimulationRunner {
 public:
  // Single module constructor (for backwards compatibility)
  SimulationRunner(const lir::Module& module, SimulationContext& context);

  // Multi-module constructor (for hierarchical designs)
  SimulationRunner(
      const std::vector<std::unique_ptr<lir::Module>>& modules,
      const std::vector<std::unique_ptr<mir::Package>>& packages,
      std::shared_ptr<lir::Process> package_init_process,
      std::shared_ptr<lir::LirContext> package_lir_context,
      std::vector<std::unique_ptr<lir::Function>> package_functions,
      SimulationContext& context);

  void Run();

  // Get the top instance context (for reading final variable values)
  [[nodiscard]] auto GetTopInstance() const
      -> const std::shared_ptr<InstanceContext>& {
    return top_instance_;
  }

 private:
  void ElaborateHierarchy();
  void ElaborateSubmodules(
      const lir::Module& parent, const std::string& parent_path,
      const std::shared_ptr<InstanceContext>& parent_instance);
  void InitializePackageVariables();
  static void InitializeModuleVariables(
      const lir::Module& module,
      const std::shared_ptr<InstanceContext>& instance);
  void ScheduleModuleProcesses(
      const lir::Module& module,
      const std::shared_ptr<InstanceContext>& instance);

  void ExecuteOneEvent();
  void WakeWaitingProcesses(
      const std::vector<ModifiedVariable>& modified_variables);
  void ExecuteTimeSlot();
  void ExecuteRegion(Region region);

  // Helper functions for ExecuteTimeSlot
  auto HasPendingActivity() const -> bool;
  auto HasActivityInActiveGroup() const -> bool;
  static auto HasActivityInReactiveGroup() -> bool;
  auto IsAllRegionEmpty() const -> bool;

  // $monitor support: check for value changes at end of time slot
  void CheckMonitor();

  // Global queues
  DelayQueue delay_queue_;

  // Region-specific queues
  ActiveQueue active_queue_;
  InactiveQueue inactive_queue_;
  NbaQueue nba_queue_;
  PostponedQueue postponed_queue_;

  bool finish_requested_ = false;

  // Top module reference (submodules resolved via child_module pointers)
  std::reference_wrapper<const lir::Module> top_module_;

  // Package storage for initializing package variables
  std::vector<std::reference_wrapper<const mir::Package>> packages_;

  // Package variable init process (LIR lowered from package initializers)
  // Both process and context must be kept alive together
  std::shared_ptr<lir::Process> package_init_process_;
  std::shared_ptr<lir::LirContext> package_lir_context_;

  // Package functions storage - owns the lowered functions.
  // These must stay alive because kCall instructions hold raw pointers (callee)
  // to these functions. Ownership transferred from LowerPackages result.
  std::vector<std::unique_ptr<lir::Function>> package_functions_;

  // Top instance context (root of instance hierarchy)
  std::shared_ptr<InstanceContext> top_instance_;

  // Centralized frame storage - owns all ProcessFrames.
  // This mirrors C++ coroutines where Scheduler::tasks_ owns all frames.
  // Queues and triggers store ProcessHandle (non-owning) references.
  std::unordered_map<ProcessInstanceKey, ProcessFrame, ProcessInstanceKeyHash>
      process_frames_;

  std::reference_wrapper<SimulationContext> simulation_context_;
  TriggerManager trigger_manager_;
};

}  // namespace lyra::interpreter
