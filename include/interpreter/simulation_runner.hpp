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

class SimulationRunner {
 public:
  SimulationRunner(const lir::Module& module, ExecutionContext& context);

  auto Run() -> uint64_t;
  auto CurrentTime() const -> uint64_t {
    return simulation_time_;
  }

 private:
  void InitializeVariables();
  void ScheduleInitialProcesses();
  void ScheduleAlwaysProcesses();
  void ExecuteOneEvent();
  void WakeWaitingProcesses(const std::vector<std::string>& modified_variables);

  // Global queues
  DelayQueue delay_queue_;

  // Region-specific queues
  ActiveQueue active_queue_;
  InactiveQueue inactive_queue_;
  NbaQueue nba_queue_;
  PostponedQueue postponed_queue_;

  SimulationTime simulation_time_ = 0;
  bool finish_requested_ = false;

  std::reference_wrapper<const lir::Module> module_;
  std::reference_wrapper<ExecutionContext> context_;
  ProcessRunner process_runner_;
  TriggerManager trigger_manager_;
};

}  // namespace lyra::interpreter
