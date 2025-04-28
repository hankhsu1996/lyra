#pragma once

#include <memory>
#include <queue>
#include <unordered_map>
#include <vector>

#include "core/execution_context.hpp"
#include "lir/executor.hpp"
#include "lir/module.hpp"
#include "lir/process.hpp"

namespace lyra {

// Scheduled event: run a process
struct ScheduledEvent {
  std::shared_ptr<lir::Process> process;
};

class SimulationScheduler {
 public:
  SimulationScheduler(const lir::Module& module, ExecutionContext& ctx);

  void Run();

 private:
  void BuildVariableToTriggerMap();
  void ScheduleInitialProcesses();
  void ScheduleAlwaysProcesses();
  void ExecuteOneEvent();

  using EventQueue = std::queue<ScheduledEvent>;
  using VariableName = std::string;
  using VariableTriggerList = std::vector<
      std::pair<common::Trigger<VariableName>, std::shared_ptr<lir::Process>>>;
  using VariableTriggerMap =
      std::unordered_map<VariableName, VariableTriggerList>;

  EventQueue active_queue_;
  uint64_t current_time_ = 0;

  std::reference_wrapper<const lir::Module> module_;
  std::reference_wrapper<ExecutionContext> ctx_;
  lir::Executor executor_;

  VariableTriggerMap variable_to_triggers_;
};

}  // namespace lyra
