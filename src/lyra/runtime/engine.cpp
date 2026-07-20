#include "lyra/runtime/engine.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/registration.hpp"
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
      .diagnostic_sink = [](std::string_view text) { std::cerr << text; },
      .plusargs = {}};
}

Engine::Engine() : Engine(DefaultEngineOptions()) {
}

Engine::Engine(EngineOptions options)
    : stream_(std::move(options.stream_sink)),
      diagnostic_(std::move(options.diagnostic_sink)),
      plusargs_(std::move(options.plusargs)) {
}

void Engine::BindDesign(std::unique_ptr<Scope> root) {
  if (bound_) {
    throw InternalError("Engine::BindDesign called more than once");
  }
  bound_ = true;
  root_ = std::move(root);
  // The whole tree already exists: the generated `$root` constructor built the
  // top-level units as its owned children, and each child built its own
  // subtree. Each phase is one top-down walk from the root that recurses
  // through the owned-children relation, so the design-wide barrier holds --
  // every scope resolves before any initializes, and every scope initializes
  // before any activates (an upward climb during resolution already sees the
  // whole tree, and an initializer observes only resolved references).
  root_->Resolve();
  root_->Initialize();
  root_->Activate();
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
  return fatal_finish_ ? 1 : 0;
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
    if (power == kUnspecifiedTimePower) {
      return;
    }
    min_power = found ? std::min(min_power, power) : power;
    found = true;
  });
  global_precision_power_ = found ? min_power : kDefaultTimePrecisionPower;
  // LRM Table 20-3: the default `%t` display unit is the design-global
  // precision (the smallest across all timescale directives).
  time_format_.units_power = global_precision_power_;
}

void Engine::RegisterProcesses() {
  WalkScopePreOrder(*root_, [this](Scope& scope) {
    scope.ForEachProcess([this](RuntimeProcess& process) {
      switch (process.Kind()) {
        case ProcessKind::kInitial:
          ScheduleActive(process.TopHandle());
          break;
        case ProcessKind::kFinal:
          process.TopHandle()->Park(queues_.finals);
          break;
        case ProcessKind::kSpawned:
          throw InternalError(
              "Engine::RegisterProcesses: a spawned process must not appear in "
              "static scope registration");
      }
    });
  });
}

void Engine::ExecuteCurrentTimeSlot() {
  current_delta_ = 0;
  std::size_t current_work_iterations = 0;
  while (true) {
    while (!queues_.active.Empty() || !queues_.inactive.Empty()) {
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

void Engine::DrainRunnableQueue(RegistrationList& queue) {
  // LRM 9.3.2: work enqueued while this pass runs belongs to the next pass, so
  // the snapshot moves out of the queue and new arrivals accumulate behind it.
  queue.SpliceBackOnto(queues_.draining);
  while (Registration* queued = queues_.draining.PopFront()) {
    CoroutineHandle handle = queued->activation;
    // The activation is running, not waiting: it holds no membership and no
    // pending wait until its body parks again (the wait that led here is
    // consumed).
    handle->RevokeRegistrations();
    handle->pending_wait = nullptr;
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
    closure(Services());
  }
}

void Engine::SubmitNba(std::function<void(RuntimeServices&)> closure) {
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
  while (Registration* queued = queues_.finals.PopFront()) {
    CoroutineHandle handle = queued->activation;
    handle->RevokeRegistrations();
    // A `final` block is never an `await` target (LRM 9.7 restricts targets to
    // initial / always / fork), so its terminal transition drains no waiters;
    // the collector stays empty.
    std::vector<CoroutineHandle> woken;
    const bool completed = ResumeProcess(handle, woken);
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
  queues_.finals.Clear();
}

void Engine::AdvanceDeltaCycle() {
  ++current_delta_;
  if (current_delta_ > kMaxDeltaCyclesPerTimeSlot) {
    throw InternalError("Engine: delta cycle limit exceeded");
  }
}

void Engine::PromoteNextDeltaToActive() {
  queues_.next_delta.SpliceBackOnto(queues_.active);
}

void Engine::AdvanceToNextTime() {
  phase_ = SchedulerPhase::kAdvanceTime;
  auto it = queues_.delayed.begin();
  now_ = it->first;
  it->second.SpliceBackOnto(queues_.active);
  queues_.delayed.erase(it);
}

auto Engine::HasCurrentTimeWork() const -> bool {
  return !queues_.active.Empty() || !queues_.inactive.Empty() ||
         !queues_.next_delta.Empty();
}

auto Engine::HasFutureTimedWork() const -> bool {
  return !queues_.delayed.empty();
}

auto Engine::HasScheduledWork() const -> bool {
  return HasCurrentTimeWork() || HasFutureTimedWork();
}

auto Engine::HasNextDeltaWork() const -> bool {
  return !queues_.next_delta.Empty();
}

auto Engine::IsRunnablePhase() const -> bool {
  return phase_ == SchedulerPhase::kActive ||
         phase_ == SchedulerPhase::kInactive ||
         phase_ == SchedulerPhase::kReactive;
}

auto Engine::ResumeProcess(
    CoroutineHandle handle, std::vector<CoroutineHandle>& woken) -> bool {
  // Capture the owning process before resuming, since `handle` may be an
  // enabled task's frame that is destroyed as control returns up the enable
  // chain. On completion the terminal transition drains the process's own
  // `await` waiters into `woken` atomically.
  RuntimeProcess& process = handle->Process();
  return process.ResumeWith(execution_context_, handle, woken);
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
  // during await_suspend.
  RuntimeProcess& process = handle->Process();
  std::vector<CoroutineHandle> woken;
  if (!ResumeProcess(handle, woken)) {
    return;
  }
  // Terminal transition already settled the process and drained its own `await`
  // waiters into `woken` (LRM 9.7) atomically. Add the surviving-boundary
  // effect -- the parent's `wait fork` waiter if this was the last live child
  // (LRM 9.6.1) -- while the node is still linked, then schedule. The
  // activation layer names who became runnable; the engine schedules.
  if (RuntimeProcess* parent = process.Parent(); parent != nullptr) {
    if (CoroutineHandle waiter = parent->TakeWaitForkWaiterIfSatisfied()) {
      woken.push_back(waiter);
    }
  }
  for (CoroutineHandle waiter : woken) {
    ScheduleNextDelta(waiter);
  }
  // Releasing destroys `process` and every ancestor the release leaves with no
  // lineage to retain, so no statement may follow it here.
  RuntimeProcess::ReleaseTerminatedLineage(process);
}

void Engine::RequestFinish(
    int,  // NOLINT(readability-named-parameter)
    bool fatal) {
  // The LRM 20.2 `$finish` verbosity argument is validated at lowering and
  // threaded here for interface completeness, but the engine terminates
  // regardless of its value; acting on it is a known gap. The `fatal` flag
  // (LRM 20.10) bumps the eventual Run() return to a non-zero exit code.
  finished_ = true;
  if (fatal) fatal_finish_ = true;
}

void Engine::TriggerValueChange(
    Observable& observable, const EdgeClassifier& classify) {
  for (CoroutineHandle handle : observable.TakeMatchingWaiters(classify)) {
    ScheduleNextDelta(handle);
  }
}

void Engine::ScheduleActive(CoroutineHandle handle) {
  handle->Park(queues_.active);
}

void Engine::ScheduleInactive(CoroutineHandle handle) {
  handle->Park(queues_.inactive);
}

void Engine::ScheduleNextDelta(CoroutineHandle handle) {
  // Every satisfied wait comes back through this verb, so it is also where the
  // wait ends: the activation is runnable now, and nothing it was parked on --
  // the sibling observables of an `@(a or b)`, the event it waited for -- may
  // fire it a second time. The pending wait is consumed here too, so a suspend
  // in the woken-but-not-yet-resumed window saves a runnable disposition (a
  // re-queue on resume), not a blocked one (a re-establish).
  handle->RevokeRegistrations();
  handle->pending_wait = nullptr;
  handle->Park(queues_.next_delta);
}

void Engine::ScheduleAtTime(SimTime when, CoroutineHandle handle) {
  handle->Park(queues_.delayed[when]);
}

void Engine::Spawn(Coroutine<void> coroutine) {
  auto child = std::make_shared<RuntimeProcess>(
      ProcessKind::kSpawned, std::move(coroutine));
  const CoroutineHandle handle = child->TopHandle();
  execution_context_.CurrentProcess().AdoptChild(std::move(child));
  ScheduleActive(handle);
}

auto Engine::CurrentProcess() -> RuntimeProcess& {
  return execution_context_.CurrentProcess();
}

auto Engine::CheckedAdd(SimTime base, SimDuration delta) -> SimTime {
  if (delta > std::numeric_limits<SimTime>::max() - base) {
    throw InternalError("Engine::CheckedAdd: wake time overflow");
  }
  return base + delta;
}

}  // namespace lyra::runtime
