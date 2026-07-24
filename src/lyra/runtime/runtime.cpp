#include "lyra/runtime/runtime.hpp"

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
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"

namespace lyra::runtime {

auto DefaultRuntimeOptions() -> RuntimeOptions {
  return RuntimeOptions{
      .stream_sink = [](std::string_view text) { std::cout << text; },
      .diagnostic_sink = [](std::string_view text) { std::cerr << text; },
      .plusargs = {}};
}

Runtime::Runtime() : Runtime(DefaultRuntimeOptions()) {
}

Runtime::Runtime(RuntimeOptions options)
    : stream_(std::move(options.stream_sink)),
      diagnostic_(std::move(options.diagnostic_sink)),
      plusargs_(std::move(options.plusargs)) {
}

Runtime::~Runtime() = default;

void Runtime::BindDesign(std::unique_ptr<Design> design) {
  if (bound_) {
    throw InternalError("Runtime::BindDesign called more than once");
  }
  bound_ = true;
  design_ = std::move(design);
  // The whole tree already exists: the generated `$root` constructor built
  // the top-level units as its owned children, and each child built its
  // own subtree. Each phase is one top-down walk from the root that
  // recurses through the owned-children relation, so the design-wide
  // barrier holds -- every scope resolves before any initializes, and
  // every scope initializes before any activates.
  WalkResolve(design_->Root());
  WalkInitialize(design_->Root());
  WalkActivate(design_->Root());
}

void Runtime::WalkResolve(Scope& scope) {
  {
    ScopeExecutionGuard guard{*this, scope};
    scope.Resolve();
  }
  scope.ForEachChild([this](Scope& child) { WalkResolve(child); });
}

void Runtime::WalkInitialize(Scope& scope) {
  {
    ScopeExecutionGuard guard{*this, scope};
    scope.Initialize();
  }
  scope.ForEachChild([this](Scope& child) { WalkInitialize(child); });
}

void Runtime::WalkActivate(Scope& scope) {
  {
    ScopeExecutionGuard guard{*this, scope};
    scope.CreateProcesses();
  }
  scope.ForEachChild([this](Scope& child) { WalkActivate(child); });
}

auto Runtime::Run() -> int {
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

void Runtime::EnsureReadyToRun() {
  if (!bound_) {
    throw InternalError("Runtime::Run called before BindDesign");
  }
  if (ran_) {
    throw InternalError("Runtime::Run called more than once");
  }
  ran_ = true;
}

void Runtime::ResolveGlobalTimePrecision() {
  bool found = false;
  std::int8_t min_power = kDefaultTimePrecisionPower;
  design_->ForEachScope([&](Scope& scope) {
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

void Runtime::RegisterProcesses() {
  for (const auto& process : processes_) {
    switch (process->Kind()) {
      case ProcessKind::kInitial:
        ScheduleActive(process->TopHandle());
        break;
      case ProcessKind::kFinal:
        process->TopHandle()->Park(queues_.finals);
        break;
      case ProcessKind::kSpawned:
        throw InternalError(
            "Runtime::RegisterProcesses: a spawned process must not appear in "
            "static scope registration");
    }
  }
}

void Runtime::RegisterProcessInRegistry(
    std::shared_ptr<RuntimeProcess> process) {
  Scope* owning = process->OwningScope();
  RuntimeProcess* raw = process.get();
  processes_.push_back(std::move(process));
  if (owning != nullptr) {
    processes_by_scope_[owning].push_back(raw);
  }
}

void Runtime::EnqueueNextDelta(CoroutineHandle handle) {
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

void Runtime::ExecuteCurrentTimeSlot() {
  current_delta_ = 0;
  std::size_t current_work_iterations = 0;
  while (true) {
    while (!queues_.active.Empty() || !queues_.inactive.Empty()) {
      if (++current_work_iterations > kMaxCurrentTimeIterations) {
        throw InternalError("Runtime: current time slot did not settle");
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

void Runtime::ExecuteActiveRegion() {
  phase_ = SchedulerPhase::kActive;
  DrainRunnableQueue(queues_.active);
}

void Runtime::ExecuteInactiveRegion() {
  phase_ = SchedulerPhase::kInactive;
  DrainRunnableQueue(queues_.inactive);
}

void Runtime::DrainRunnableQueue(RegistrationList& queue) {
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

void Runtime::FlushRuntimeUpdates() {
  phase_ = SchedulerPhase::kFlushUpdates;
}

void Runtime::ExecuteNbaRegion() {
  phase_ = SchedulerPhase::kCommitNba;
  auto pending = std::move(queues_.nba);
  queues_.nba.clear();
  for (auto& closure : pending) {
    closure(*this);
  }
}

void Runtime::ExecuteObservedRegion() {
  phase_ = SchedulerPhase::kObserved;
  // Drain per-scope pending queues in tree order so a warning fires
  // consistent with the design hierarchy (LRM 4.4.3 does not fix the
  // relative order between scopes, but tree-order matches the natural
  // reading of the design and every other tree-walked runtime pass).
  // Each scope's slot is cleared as it drains so a fresh submit landing
  // during a later slot lands into an empty vector.
  design_->ForEachScope([this](Scope& scope) {
    const auto it = queues_.observed.find(&scope);
    if (it == queues_.observed.end()) return;
    for (auto& fn : it->second) {
      if (fn) fn();
    }
    queues_.observed.erase(it);
  });
}

void Runtime::ExecuteReactiveRegion() {
  phase_ = SchedulerPhase::kReactive;
}

void Runtime::ExecutePostponedRegion() {
  phase_ = SchedulerPhase::kPostponed;
  auto pending = std::move(queues_.postponed);
  queues_.postponed.clear();
  for (auto& closure : pending) {
    closure();
  }
}

void Runtime::ExecuteFinalProcesses() {
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
    // Suspended: only legal if `$finish` was called (sets `finished_`).
    // LRM 9.2.3 says any `$finish` in a final ends simulation immediately --
    // subsequent queued finals shall not run. Any other suspension is a
    // time-controlling statement, which is forbidden in `final` blocks.
    if (finished_) {
      break;
    }
    throw InternalError(
        "Runtime::ExecuteFinalProcesses: final block suspended; "
        "time-controlling statements are not allowed inside `final`");
  }
  queues_.finals.Clear();
}

void Runtime::AdvanceDeltaCycle() {
  ++current_delta_;
  if (current_delta_ > kMaxDeltaCyclesPerTimeSlot) {
    throw InternalError("Runtime: delta cycle limit exceeded");
  }
}

void Runtime::PromoteNextDeltaToActive() {
  queues_.next_delta.SpliceBackOnto(queues_.active);
}

void Runtime::AdvanceToNextTime() {
  phase_ = SchedulerPhase::kAdvanceTime;
  auto it = queues_.delayed.begin();
  now_ = it->first;
  it->second.SpliceBackOnto(queues_.active);
  queues_.delayed.erase(it);
}

auto Runtime::HasCurrentTimeWork() const -> bool {
  return !queues_.active.Empty() || !queues_.inactive.Empty() ||
         !queues_.next_delta.Empty();
}

auto Runtime::HasFutureTimedWork() const -> bool {
  return !queues_.delayed.empty();
}

auto Runtime::HasScheduledWork() const -> bool {
  return HasCurrentTimeWork() || HasFutureTimedWork();
}

auto Runtime::HasNextDeltaWork() const -> bool {
  return !queues_.next_delta.Empty();
}

auto Runtime::IsRunnablePhase() const -> bool {
  return phase_ == SchedulerPhase::kActive ||
         phase_ == SchedulerPhase::kInactive ||
         phase_ == SchedulerPhase::kReactive;
}

auto Runtime::ResumeProcess(
    CoroutineHandle handle, std::vector<CoroutineHandle>& woken) -> bool {
  // Capture the owning process before resuming, since `handle` may be an
  // enabled task's frame that is destroyed as control returns up the enable
  // chain. On completion the terminal transition drains the process's own
  // `await` waiters into `woken` atomically.
  RuntimeProcess& process = handle->Process();
  return process.ResumeWith(*this, handle, woken);
}

void Runtime::RunProcess(CoroutineHandle handle) {
  // Sole gate for post-$finish user code; finals bypass via their own path.
  if (finished_) {
    return;
  }
  if (!IsRunnablePhase()) {
    throw InternalError(
        "Runtime::RunProcess: process resumed outside runnable phase");
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
  // (LRM 9.6.1) -- while the node is still linked, then schedule.
  if (RuntimeProcess* parent = process.Parent(); parent != nullptr) {
    if (CoroutineHandle waiter = parent->TakeWaitForkWaiterIfSatisfied()) {
      woken.push_back(waiter);
    }
  }
  for (CoroutineHandle waiter : woken) {
    EnqueueNextDelta(waiter);
  }
  // Releasing destroys `process` and every ancestor the release leaves with no
  // lineage to retain, so no statement may follow it here.
  RuntimeProcess::ReleaseTerminatedLineage(process);
}

void Runtime::ScheduleActive(CoroutineHandle handle) {
  handle->Park(queues_.active);
}

auto Runtime::CheckedAdd(SimTime base, SimDuration delta) -> SimTime {
  if (delta > std::numeric_limits<SimTime>::max() - base) {
    throw InternalError("Runtime::CheckedAdd: wake time overflow");
  }
  return base + delta;
}

void RegisterInitialProcess(Scope* owning_scope, Coroutine<void> coroutine) {
  // Runtime is the sole concrete `RuntimeEffects` derived class (declared
  // `final`), so recovering it from the ambient view is safe.
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  auto& rt = static_cast<Runtime&>(current_runtime());
  rt.RegisterProcessInRegistry(
      std::make_shared<RuntimeProcess>(
          owning_scope, ProcessKind::kInitial, std::move(coroutine)));
}

void RegisterFinalProcess(Scope* owning_scope, Coroutine<void> coroutine) {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  auto& rt = static_cast<Runtime&>(current_runtime());
  rt.RegisterProcessInRegistry(
      std::make_shared<RuntimeProcess>(
          owning_scope, ProcessKind::kFinal, std::move(coroutine)));
}

}  // namespace lyra::runtime
