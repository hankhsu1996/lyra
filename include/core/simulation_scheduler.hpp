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
  size_t program_counter = 0;  // Current instruction index
};

struct DelayedEvent {
  uint64_t ready_time;
  std::shared_ptr<lir::Process> process;
  size_t program_counter;

  auto operator<(const DelayedEvent& rhs) const -> bool {
    // Note: priority_queue is a max-heap by default,
    // so we reverse the comparison to make it a min-heap.
    return ready_time > rhs.ready_time;
  }
};

class SimulationScheduler {
 public:
  SimulationScheduler(
      const lir::Module& module, ExecutionContext& context,
      VariableTriggerMap variable_triggers);

  auto Run() -> uint64_t;

  auto CurrentTime() const -> uint64_t {
    return simulation_time_;
  }

 private:
  void ScheduleInitialProcesses();
  void ScheduleAlwaysProcesses();
  void ExecuteOneEvent();

  using EventQueue = std::queue<ScheduledEvent>;
  using DelayQueue = std::priority_queue<DelayedEvent>;

  EventQueue active_queue_;
  DelayQueue delay_queue_;
  uint64_t simulation_time_ = 0;
  bool finish_requested_ = false;

  std::reference_wrapper<const lir::Module> module_;
  std::reference_wrapper<ExecutionContext> context_;
  lir::Executor executor_;
  VariableTriggerMap variable_to_triggers_;
};

}  // namespace lyra
