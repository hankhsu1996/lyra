#pragma once

#include <common/trigger.hpp>
#include <cstdint>
#include <map>
#include <memory>
#include <queue>
#include <string>
#include <vector>

#include "interpreter/actions.hpp"
#include "interpreter/process_runner.hpp"
#include "interpreter/trigger_manager.hpp"
#include "lir/module.hpp"
#include "lir/process.hpp"

namespace lyra::interpreter {

using SimulationTime = uint64_t;

constexpr SimulationTime kMaxSimulationTime = 10000;

using ProcessPtr = std::shared_ptr<lir::Process>;

struct ScheduledEvent {
  ProcessPtr process;
  std::size_t block_index = 0;
  std::size_t instruction_index = 0;
};

using DelayQueue = std::map<SimulationTime, std::vector<ScheduledEvent>>;
using ActiveQueue = std::queue<ScheduledEvent>;
using InactiveQueue = std::queue<ScheduledEvent>;
using NbaQueue = std::queue<NbaAction>;
using PostponedQueue = std::queue<PostponedAction>;

struct WaitingProcessInfo {
  std::size_t block_index;
  std::size_t instruction_index;
  common::EdgeKind edge_kind;
};

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
  SimulationRunner(const lir::Module& module, ExecutionContext& context);

  void Run();

 private:
  void InitializeVariables();
  void ScheduleInitialProcesses();
  void ScheduleAlwaysProcesses();
  void ExecuteOneEvent();
  void WakeWaitingProcesses(const std::vector<std::string>& modified_variables);
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

  std::reference_wrapper<const lir::Module> module_;
  std::reference_wrapper<ExecutionContext> context_;
  ProcessRunner process_runner_;
  TriggerManager trigger_manager_;
};

}  // namespace lyra::interpreter
