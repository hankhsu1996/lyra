#pragma once

#include <memory>
#include <queue>

#include "core/execution_context.hpp"
#include "core/simulation_preparation.hpp"
#include "lir/executor.hpp"
#include "lir/module.hpp"
#include "lir/process.hpp"

namespace lyra {

struct ScheduledEvent {
  std::shared_ptr<lir::Process> process;
};

class SimulationScheduler {
 public:
  SimulationScheduler(
      const lir::Module& module, ExecutionContext& ctx,
      VariableTriggerMap variable_triggers);

  void Run();

 private:
  void ScheduleInitialProcesses();
  void ScheduleAlwaysProcesses();
  void ExecuteOneEvent();

  using EventQueue = std::queue<ScheduledEvent>;

  EventQueue active_queue_;
  uint64_t current_time_ = 0;

  std::reference_wrapper<const lir::Module> module_;
  std::reference_wrapper<ExecutionContext> ctx_;
  lir::Executor executor_;

  VariableTriggerMap variable_to_triggers_;
};

}  // namespace lyra
