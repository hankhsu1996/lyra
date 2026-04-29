#include "lyra/runtime/engine.hpp"

#include <cstddef>
#include <iostream>
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/bind_context.hpp"
#include "lyra/runtime/event.hpp"
#include "lyra/runtime/module.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_scope.hpp"
#include "lyra/runtime/runtime_scope_kind.hpp"
#include "lyra/runtime/runtime_traversal.hpp"
#include "lyra/runtime/wait_request.hpp"

namespace lyra::runtime {

auto DefaultEngineOptions() -> EngineOptions {
  return EngineOptions{
      .output_sink = [](std::string_view text) { std::cout << text; }};
}

Engine::Engine() : Engine(DefaultEngineOptions()) {
}

Engine::Engine(EngineOptions options)
    : output_(std::move(options.output_sink)) {
}

void Engine::BindRoot(std::string root_name, Module& top) {
  if (bound_) {
    throw InternalError("Engine::BindRoot called more than once");
  }
  bound_ = true;
  root_ = std::make_unique<RuntimeScope>(
      nullptr, std::move(root_name), RuntimeScopeKind::kModuleInstance);
  RuntimeBindContext ctx(*root_, services_);
  top.Bind(ctx);
}

auto Engine::Run() -> int {
  EnsureReadyToRun();
  EnqueueInitialProcesses();

  while (HasScheduledWork()) {
    ExecuteCurrentTimeSlot();
    if (!HasFutureTimedWork()) {
      break;
    }
    AdvanceToNextTime();
  }

  phase_ = SchedulerPhase::kIdle;
  output_.Drain();
  return 0;
}

void Engine::EnsureReadyToRun() {
  if (!bound_) {
    throw InternalError("Engine::Run called before BindRoot");
  }
  if (ran_) {
    throw InternalError("Engine::Run called more than once");
  }
  ran_ = true;
}

void Engine::EnqueueInitialProcesses() {
  WalkScopePreOrder(*root_, [this](RuntimeScope& scope) {
    scope.ForEachProcess([this](RuntimeProcess& process) {
      switch (process.Kind()) {
        case ProcessKind::kInitial:
          ScheduleActive(process);
          break;
        case ProcessKind::kAlways:
        case ProcessKind::kAlwaysComb:
        case ProcessKind::kAlwaysFf:
        case ProcessKind::kFinal:
          throw InternalError("Engine::Run: ProcessKind is not yet supported");
      }
    });
  });
}

void Engine::ExecuteCurrentTimeSlot() {
  current_delta_ = 0;
  std::size_t current_work_iterations = 0;
  while (true) {
    while (!queues_.active.empty() || !queues_.inactive.empty()) {
      if (++current_work_iterations > kMaxCurrentTimeIterations) {
        throw InternalError("Engine: current time slot did not settle");
      }
      ExecuteActiveRegion();
      ExecuteInactiveRegion();
    }
    FlushRuntimeUpdates();
    ExecuteNbaRegion();
    FlushRuntimeUpdates();
    ExecuteObservedRegion();
    ExecuteReactiveRegion();
    if (!HasNextDeltaWork()) {
      break;
    }
    AdvanceDeltaCycle();
    PromoteNextDeltaToActive();
  }
  ExecutePostponedRegion();
}

void Engine::ExecuteActiveRegion() {
  phase_ = SchedulerPhase::kActive;
  DrainRunnableQueue(queues_.active);
}

void Engine::ExecuteInactiveRegion() {
  phase_ = SchedulerPhase::kInactive;
  DrainRunnableQueue(queues_.inactive);
}

void Engine::DrainRunnableQueue(std::deque<RuntimeProcess*>& queue) {
  const std::size_t snapshot_size = queue.size();
  for (std::size_t i = 0; i < snapshot_size; ++i) {
    RuntimeProcess* process = queue.front();
    queue.pop_front();
    RunProcess(*process);
  }
}

void Engine::FlushRuntimeUpdates() {
  phase_ = SchedulerPhase::kFlushUpdates;
}

void Engine::ExecuteNbaRegion() {
  phase_ = SchedulerPhase::kCommitNba;
}

void Engine::ExecuteObservedRegion() {
  phase_ = SchedulerPhase::kObserved;
}

void Engine::ExecuteReactiveRegion() {
  phase_ = SchedulerPhase::kReactive;
}

void Engine::ExecutePostponedRegion() {
  phase_ = SchedulerPhase::kPostponed;
}

void Engine::AdvanceDeltaCycle() {
  ++current_delta_;
  if (current_delta_ > kMaxDeltaCyclesPerTimeSlot) {
    throw InternalError("Engine: delta cycle limit exceeded");
  }
}

void Engine::PromoteNextDeltaToActive() {
  for (RuntimeProcess* p : queues_.next_delta) {
    ScheduleActive(*p);
  }
  queues_.next_delta.clear();
}

void Engine::AdvanceToNextTime() {
  phase_ = SchedulerPhase::kAdvanceTime;
  auto it = queues_.delayed.begin();
  now_ = it->first;
  for (RuntimeProcess* p : it->second) {
    ScheduleActive(*p);
  }
  queues_.delayed.erase(it);
}

auto Engine::HasCurrentTimeWork() const -> bool {
  return !queues_.active.empty() || !queues_.inactive.empty() ||
         !queues_.next_delta.empty();
}

auto Engine::HasFutureTimedWork() const -> bool {
  return !queues_.delayed.empty();
}

auto Engine::HasScheduledWork() const -> bool {
  return HasCurrentTimeWork() || HasFutureTimedWork();
}

auto Engine::HasNextDeltaWork() const -> bool {
  return !queues_.next_delta.empty();
}

auto Engine::IsRunnablePhase() const -> bool {
  return phase_ == SchedulerPhase::kActive ||
         phase_ == SchedulerPhase::kInactive ||
         phase_ == SchedulerPhase::kReactive;
}

void Engine::RunProcess(RuntimeProcess& process) {
  if (!IsRunnablePhase()) {
    throw InternalError(
        "Engine::RunProcess: process resumed outside runnable phase");
  }
  auto result = process.Resume();
  if (result.IsCompleted()) {
    return;
  }
  ScheduleWait(process, result.TakeWait());
}

void Engine::ScheduleWait(RuntimeProcess& process, WaitRequest wait) {
  std::visit(
      Overloaded{
          [&](DelayWait d) { ScheduleDelayWait(process, d); },
          [&](EventWait e) { ScheduleEventWait(process, e); },
      },
      wait);
}

void Engine::ScheduleDelayWait(RuntimeProcess& process, DelayWait wait) {
  if (wait.duration == 0) {
    ScheduleInactive(process);
    return;
  }
  ScheduleDelayed(CheckedAdd(now_, wait.duration), process);
}

void Engine::ScheduleEventWait(RuntimeProcess& process, EventWait wait) {
  if (wait.event == nullptr) {
    throw InternalError("Engine::ScheduleEventWait: event pointer is null");
  }
  wait.event->AddWaiter(process);
}

void Engine::TriggerEvent(RuntimeEvent& event) {
  for (RuntimeProcess* p : event.TakeWaiters()) {
    ScheduleNextDelta(*p);
  }
}

void Engine::ScheduleActive(RuntimeProcess& process) {
  queues_.active.push_back(&process);
}

void Engine::ScheduleInactive(RuntimeProcess& process) {
  queues_.inactive.push_back(&process);
}

void Engine::ScheduleNextDelta(RuntimeProcess& process) {
  queues_.next_delta.push_back(&process);
}

void Engine::ScheduleDelayed(SimTime wake_time, RuntimeProcess& process) {
  queues_.delayed[wake_time].push_back(&process);
}

auto Engine::CheckedAdd(SimTime base, SimDuration delta) -> SimTime {
  if (delta > std::numeric_limits<SimTime>::max() - base) {
    throw InternalError("Engine::CheckedAdd: wake time overflow");
  }
  return base + delta;
}

}  // namespace lyra::runtime
