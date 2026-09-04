#include "lyra/runtime/runtime.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
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
  scope.Resolve();
  scope.ForEachChild([this](Scope& child) { WalkResolve(child); });
}

void Runtime::WalkInitialize(Scope& scope) {
  scope.Initialize();
  scope.ForEachChild([this](Scope& child) { WalkInitialize(child); });
}

void Runtime::WalkActivate(Scope& scope) {
  scope.CreateProcesses();
  scope.ForEachChild([this](Scope& child) { WalkActivate(child); });
}

auto Runtime::Run() -> int {
  EnsureReadyToRun();
  ResolveGlobalTimePrecision();
  RegisterProcesses();

  // LRM 4.4: slots run in time order and the simulator never goes backwards,
  // so the earliest pending slot is always the next one.
  while (!finished_) {
    auto slot = slots_.begin();
    if (slot == slots_.end()) {
      break;
    }
    now_ = slot->first;
    ExecuteTimeSlot(slot->second);
    if (finished_) {
      break;
    }
    slots_.erase(slot);
  }

  ExecuteFinalProcesses();

  ReportCoverage();
  stream_.Drain();
  return fatal_finish_ ? 1 : 0;
}

void Runtime::ReportCoverage() {
  for (const std::string& line : coverage_.Report()) {
    stream_.Append(line);
    stream_.FinishRecord(true);
  }
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
        Schedule(now_, Region::kActive, process->TopHandle());
        break;
      case ProcessKind::kFinal:
        process->TopHandle()->Park(finals_);
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

auto Runtime::SlotAt(SimTime when) -> TimeSlot& {
  if (when < now_) {
    throw InternalError(
        "Runtime::SlotAt: a time slot earlier than the current one can never "
        "run");
  }
  return slots_[when];
}

void Runtime::ExecuteTimeSlot(TimeSlot& slot) {
  RunRegion(slot, Region::kPreponed);
  std::size_t passes = 0;
  // LRM 4.5: take the earliest region that has anything, run it, and look
  // again -- what it produced lands back in the slot. The reactive group comes
  // after Observed in the order, so it is reached only once the active group is
  // empty, and work it schedules back into Active is found first on the next
  // look.
  while (std::optional<Region> region =
             slot.FirstPending(Region::kActive, Region::kReNba)) {
    if (++passes > kMaxRegionPassesPerSlot) {
      throw SimulationError(
          "the current time slot did not settle: the design keeps "
          "scheduling work without advancing time");
    }
    RunRegion(slot, *region);
  }
  RunRegion(slot, Region::kPostponed);
  if (!finished_ && !slot.Empty()) {
    throw SimulationError(
        "the postponed region scheduled work back into the time slot that "
        "ends with it (LRM 4.4.2.9)");
  }
}

void Runtime::RunRegion(TimeSlot& slot, Region region) {
  RegionQueue& queue = slot[region];
  // LRM 9.3.2: work arriving while this pass runs belongs to the next pass, so
  // both snapshots move out of the region and new arrivals accumulate behind
  // them. LRM 4.5 fixes no order between the events of one region.
  std::vector<std::function<void()>> effects = std::move(queue.effects);
  queue.effects.clear();
  queue.activations.SpliceBackOnto(draining_);
  for (const auto& effect : effects) {
    effect();
  }
  while (Registration* queued = draining_.PopFront()) {
    CoroutineHandle handle = queued->activation;
    ConsumeWait(handle);
    RunProcess(handle);
  }
}

void Runtime::ExecuteFinalProcesses() {
  while (Registration* queued = finals_.PopFront()) {
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
    throw SimulationError(
        "a final block suspended: time-controlling statements are not allowed "
        "inside `final` (LRM 9.2.3)");
  }
  finals_.Clear();
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
  // Where a `$finish` stops the design: no process resumes after it. Deferred
  // effects the slot already holds still run, and a `final` body reaches its
  // statements through its own path.
  if (finished_) {
    return;
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
    Wake(waiter);
  }
  // Releasing destroys `process` and every ancestor the release leaves with no
  // lineage to retain, so no statement may follow it here.
  RuntimeProcess::ReleaseTerminatedLineage(process);
}

void RegisterInitialProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine) {
  // Runtime is the sole concrete `RuntimeEffects` derived class (declared
  // `final`), so recovering it from the ambient view is safe.
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  auto& rt = static_cast<Runtime&>(current_runtime());
  rt.RegisterProcessInRegistry(
      std::make_shared<RuntimeProcess>(
          owning_scope, ProcessKind::kInitial, std::move(coroutine),
          unit_instance->InitializationSeeds().NextSeed()));
}

void RegisterFinalProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine) {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  auto& rt = static_cast<Runtime&>(current_runtime());
  rt.RegisterProcessInRegistry(
      std::make_shared<RuntimeProcess>(
          owning_scope, ProcessKind::kFinal, std::move(coroutine),
          unit_instance->InitializationSeeds().NextSeed()));
}

}  // namespace lyra::runtime
