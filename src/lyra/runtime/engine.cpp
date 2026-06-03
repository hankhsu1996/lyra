#include "lyra/runtime/engine.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <limits>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_traversal.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"

namespace lyra::runtime {

auto DefaultEngineOptions() -> EngineOptions {
  return EngineOptions{
      .stream_sink = [](std::string_view text) { std::cout << text; },
      .diagnostic_sink = [](std::string_view text) { std::cerr << text; }};
}

Engine::Engine() : Engine(DefaultEngineOptions()) {
}

Engine::Engine(EngineOptions options)
    : stream_(std::move(options.stream_sink)),
      diagnostic_(std::move(options.diagnostic_sink)) {
}

void Engine::BindDesign(std::span<const TopBinding> tops) {
  if (bound_) {
    throw InternalError("Engine::BindDesign called more than once");
  }
  bound_ = true;
  root_ = std::make_unique<Scope>(nullptr, "$root");
  for (const auto& top : tops) {
    root_->AddChild(*top.scope);
    top.scope->Bind(services_);
  }
}

auto Engine::Run() -> int {
  EnsureReadyToRun();
  ResolveGlobalTimePrecision();
  RegisterProcesses();

  while (HasScheduledWork() && !finished_) {
    ExecuteCurrentTimeSlot();
    if (finished_) {
      break;
    }
    if (!HasFutureTimedWork()) {
      break;
    }
    AdvanceToNextTime();
  }

  ExecuteFinalProcesses();

  phase_ = SchedulerPhase::kIdle;
  stream_.Drain();
  return 0;
}

void Engine::EnsureReadyToRun() {
  if (!bound_) {
    throw InternalError("Engine::Run called before BindDesign");
  }
  if (ran_) {
    throw InternalError("Engine::Run called more than once");
  }
  ran_ = true;
}

void Engine::ResolveGlobalTimePrecision() {
  bool found = false;
  std::int8_t min_power = kDefaultTimePrecisionPower;
  WalkScopePreOrder(*root_, [&](Scope& scope) {
    const std::int8_t power = scope.TimePrecisionPower();
    if (power == kUnspecifiedTimePrecisionPower) {
      return;
    }
    min_power = found ? std::min(min_power, power) : power;
    found = true;
  });
  global_precision_power_ = found ? min_power : kDefaultTimePrecisionPower;
}

void Engine::RegisterProcesses() {
  WalkScopePreOrder(*root_, [this](Scope& scope) {
    scope.ForEachProcess([this](RuntimeProcess& process) {
      switch (process.Kind()) {
        case ProcessKind::kInitial:
          ScheduleActive(process.TopHandle());
          break;
        case ProcessKind::kFinal:
          queues_.finals.push_back(process.TopHandle());
          break;
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

void Engine::DrainRunnableQueue(std::deque<CoroutineHandle>& queue) {
  const std::size_t snapshot_size = queue.size();
  for (std::size_t i = 0; i < snapshot_size; ++i) {
    CoroutineHandle handle = queue.front();
    queue.pop_front();
    RunProcess(handle);
  }
}

void Engine::FlushRuntimeUpdates() {
  phase_ = SchedulerPhase::kFlushUpdates;
}

void Engine::ExecuteNbaRegion() {
  phase_ = SchedulerPhase::kCommitNba;
  // Swap-then-drain: SubmitNba called during commit (re-entrancy) is rejected
  // upstream; this swap also protects against accidental iterator invalidation
  // if any closure does run a SubmitNba via a future path.
  auto pending = std::move(queues_.nba);
  queues_.nba.clear();
  for (auto& closure : pending) {
    closure();
  }
}

void Engine::SubmitNba(std::function<void()> closure) {
  if (phase_ == SchedulerPhase::kCommitNba) {
    throw InternalError(
        "Engine::SubmitNba: re-entrant NBA submission during NBA region "
        "is not supported");
  }
  queues_.nba.push_back(std::move(closure));
}

void Engine::SubmitPostponed(std::function<void()> closure) {
  if (phase_ == SchedulerPhase::kPostponed) {
    throw InternalError(
        "Engine::SubmitPostponed: re-entrant postponed submission during "
        "postponed region is not supported");
  }
  queues_.postponed.push_back(std::move(closure));
}

void Engine::ExecuteObservedRegion() {
  phase_ = SchedulerPhase::kObserved;
  WalkScopePreOrder(*root_, [](Scope& scope) { scope.DrainObserved(); });
}

void Engine::ExecuteReactiveRegion() {
  phase_ = SchedulerPhase::kReactive;
}

void Engine::ExecutePostponedRegion() {
  phase_ = SchedulerPhase::kPostponed;
  auto pending = std::move(queues_.postponed);
  queues_.postponed.clear();
  for (auto& closure : pending) {
    closure();
  }
}

void Engine::ExecuteFinalProcesses() {
  phase_ = SchedulerPhase::kPostponed;
  for (CoroutineHandle handle : queues_.finals) {
    const bool completed = handle.promise().Process().ResumeWith(handle);
    if (completed) {
      continue;
    }
    // Suspended: only legal if `$finish` was called (sets `finished_` via
    // `RuntimeServices::RequestFinish`). LRM 9.2.3 says any `$finish` in a
    // final ends simulation immediately -- subsequent queued finals shall
    // not run. Any other suspension is a time-controlling statement, which
    // is forbidden in `final` blocks.
    if (finished_) {
      break;
    }
    throw InternalError(
        "Engine::ExecuteFinalProcesses: final block suspended; "
        "time-controlling statements are not allowed inside `final`");
  }
  queues_.finals.clear();
}

void Engine::AdvanceDeltaCycle() {
  ++current_delta_;
  if (current_delta_ > kMaxDeltaCyclesPerTimeSlot) {
    throw InternalError("Engine: delta cycle limit exceeded");
  }
}

void Engine::PromoteNextDeltaToActive() {
  for (CoroutineHandle handle : queues_.next_delta) {
    ScheduleActive(handle);
  }
  queues_.next_delta.clear();
}

void Engine::AdvanceToNextTime() {
  phase_ = SchedulerPhase::kAdvanceTime;
  auto it = queues_.delayed.begin();
  now_ = it->first;
  for (CoroutineHandle handle : it->second) {
    ScheduleActive(handle);
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

void Engine::RunProcess(CoroutineHandle handle) {
  // Sole gate for post-$finish user code; finals bypass via their own path.
  if (finished_) {
    return;
  }
  if (!IsRunnablePhase()) {
    throw InternalError(
        "Engine::RunProcess: process resumed outside runnable phase");
  }
  // No wait dispatch: each awaitable has already arranged its own wakeup path
  // during await_suspend. The engine only observes "process completed or still
  // suspended" and acts accordingly. Capture the owning process before
  // resuming, since `handle` may be an enabled task's frame that is destroyed
  // as control returns up the enable chain.
  handle.promise().Process().ResumeWith(handle);
}

void Engine::RequestFinish(int level) {
  // `level` is the LRM 20.2 verbosity argument to `$finish`. Lyra's engine
  // currently terminates regardless of level; the parameter is accepted to
  // preserve the call shape for future verbosity-aware reporting.
  (void)level;
  finished_ = true;
}

void Engine::TriggerValueChange(
    Observable& observable, const EdgeClassifier& classify) {
  for (CoroutineHandle handle : observable.TakeMatchingWaiters(classify)) {
    // Clean up sibling subscriptions so they don't leak across waits when this
    // frame re-enters the runnable queue.
    for (Observable* other : std::exchange(
             handle.promise().pending_value_change_subscriptions, {})) {
      if (other != &observable) {
        other->Unsubscribe(handle);
      }
    }
    ScheduleNextDelta(handle);
  }
}

void Engine::ScheduleActive(CoroutineHandle handle) {
  queues_.active.push_back(handle);
}

void Engine::ScheduleInactive(CoroutineHandle handle) {
  queues_.inactive.push_back(handle);
}

void Engine::ScheduleNextDelta(CoroutineHandle handle) {
  queues_.next_delta.push_back(handle);
}

void Engine::ScheduleAtTime(SimTime when, CoroutineHandle handle) {
  queues_.delayed[when].push_back(handle);
}

auto Engine::CheckedAdd(SimTime base, SimDuration delta) -> SimTime {
  if (delta > std::numeric_limits<SimTime>::max() - base) {
    throw InternalError("Engine::CheckedAdd: wake time overflow");
  }
  return base + delta;
}

}  // namespace lyra::runtime
