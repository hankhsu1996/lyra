#pragma once

#include <common/trigger.hpp>
#include <cstdint>
#include <memory>
#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "interpreter/process_runner.hpp"
#include "lir/module.hpp"
#include "lir/process.hpp"
#include "runtime/execution_context.hpp"

namespace lyra::interpreter {

struct ScheduledEvent {
  std::shared_ptr<lir::Process> process;
  std::size_t block_index = 0;
  std::size_t instruction_index = 0;
};

struct DelayedEvent {
  uint64_t ready_time;
  std::shared_ptr<lir::Process> process;
  std::size_t block_index = 0;
  std::size_t instruction_index = 0;

  auto operator<(const DelayedEvent& rhs) const -> bool {
    return ready_time > rhs.ready_time;  // min-heap
  }
};

struct WaitingProcessInfo {
  std::size_t block_index;
  std::size_t instruction_index;
  common::EdgeKind edge_kind;
};

struct ProcessPtrHash {
  auto operator()(const std::shared_ptr<lir::Process>& ptr) const
      -> std::size_t {
    return std::hash<lir::Process*>{}(ptr.get());
  }
};

struct ProcessPtrEqual {
  auto operator()(
      const std::shared_ptr<lir::Process>& lhs,
      const std::shared_ptr<lir::Process>& rhs) const -> bool {
    return lhs.get() == rhs.get();
  }
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

  using EventQueue = std::queue<ScheduledEvent>;
  using DelayQueue = std::priority_queue<DelayedEvent>;
  using WaitMap = std::unordered_map<
      std::string,
      std::unordered_set<
          std::shared_ptr<lir::Process>, ProcessPtrHash, ProcessPtrEqual>>;
  using WaitSet = std::unordered_map<
      std::shared_ptr<lir::Process>, WaitingProcessInfo, ProcessPtrHash,
      ProcessPtrEqual>;

  EventQueue active_queue_;
  DelayQueue delay_queue_;
  WaitMap wait_map_;
  WaitSet wait_set_;

  uint64_t simulation_time_ = 0;
  bool finish_requested_ = false;

  std::reference_wrapper<const lir::Module> module_;
  std::reference_wrapper<ExecutionContext> context_;
  ProcessRunner process_runner_;
};

}  // namespace lyra::interpreter
