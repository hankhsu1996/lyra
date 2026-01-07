#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <queue>
#include <string>
#include <unordered_map>
#include <vector>

#include "lyra/common/trigger.hpp"
#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/trigger_manager.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::interpreter {

using SimulationTime = uint64_t;

// Max simulation time in internal ticks (1 million allows ~1ms at 1ns
// precision)
constexpr SimulationTime kMaxSimulationTime = 1'000'000;

using ProcessPtr = std::shared_ptr<lir::Process>;

struct ScheduledEvent {
  ProcessPtr process;
  std::shared_ptr<InstanceContext> instance;  // Like C++ 'this' pointer
  std::size_t block_index = 0;
  std::size_t instruction_index = 0;
};

using DelayQueue = std::map<SimulationTime, std::vector<ScheduledEvent>>;
using ActiveQueue = std::queue<ScheduledEvent>;
using InactiveQueue = std::queue<ScheduledEvent>;
using NbaQueue = std::queue<NbaAction>;
using PostponedQueue = std::queue<PostponedAction>;

// WaitingProcessInfo is defined in trigger_manager.hpp

enum class RegionType {
  kPreponed,      // Not implemented yet
  kPreActive,     // Not implemented yet
  kActive,        // Main always blocks, schedules NBA
  kInactive,      // #0 delays
  kPreNba,        // Not implemented yet
  kNba,           // Commit NBA updates
  kPostNba,       // Not implemented yet
  kPreObserved,   // Not implemented yet
  kObserved,      // Not implemented yet
  kPostObserved,  // Not implemented yet
  kReactive,      // Testbench execution
  kReInactive,    // Not implemented yet
  kPreReNba,      // Not implemented yet
  kReNba,         // Not implemented yet
  kPostReNba,     // Not implemented yet
  kPrePostponed,  // Not implemented yet
  kPostponed      // Final observation ($strobe, $monitor)
};

class SimulationRunner {
 public:
  // Single module constructor (for backwards compatibility)
  SimulationRunner(const lir::Module& module, SimulationContext& context);

  // Multi-module constructor (for hierarchical designs)
  SimulationRunner(
      const std::vector<std::unique_ptr<lir::Module>>& modules,
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
  void InitializeModuleVariables(
      const lir::Module& module,
      const std::shared_ptr<InstanceContext>& instance);
  void ScheduleModuleProcesses(
      const lir::Module& module,
      const std::shared_ptr<InstanceContext>& instance);
  auto LookupModule(const std::string& name) const -> const lir::Module*;

  void ExecuteOneEvent();
  void WakeWaitingProcesses(
      const std::vector<ModifiedVariable>& modified_variables);
  void ExecuteTimeSlot();
  void ExecuteRegion(RegionType region);

  // Helper functions for ExecuteTimeSlot
  auto HasPendingActivity() const -> bool;
  auto HasActivityInActiveGroup() const -> bool;
  auto HasActivityInReactiveGroup() const -> bool;
  auto IsAllRegionEmpty() const -> bool;
  void MoveToActive(std::queue<ScheduledEvent>& source);

  // Global queues
  DelayQueue delay_queue_;

  // Region-specific queues
  ActiveQueue active_queue_;
  InactiveQueue inactive_queue_;
  NbaQueue nba_queue_;
  PostponedQueue postponed_queue_;

  bool finish_requested_ = false;

  // Module storage - either single module reference or multi-module map
  std::reference_wrapper<const lir::Module> top_module_;
  std::unordered_map<std::string, std::reference_wrapper<const lir::Module>>
      module_map_;

  // Top instance context (root of instance hierarchy)
  std::shared_ptr<InstanceContext> top_instance_;

  std::reference_wrapper<SimulationContext> simulation_context_;
  TriggerManager trigger_manager_;
};

}  // namespace lyra::interpreter
