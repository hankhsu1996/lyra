#include "lyra/runtime/runtime.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <ctime>
#include <format>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"

namespace lyra::runtime {

namespace {

// What the run has cost the processor so far, which is the statistic LRM 20.2
// Table 20-1's most verbose level asks a tool to report.
auto ProcessorSeconds() -> double {
  return static_cast<double>(std::clock()) /
         static_cast<double>(CLOCKS_PER_SEC);
}

}  // namespace

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
  diagnostic_.SetContextSource([this] { return ReportContext(); });
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
  // own subtree. Resolving is one top-down walk from the root that recurses
  // through the owned-children relation, so the design-wide barrier holds --
  // every scope resolves before any initializes.
  WalkResolve(design_->Root());
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
  RunSimulation();
  // Owed because the run is over, whichever way it ended: LRM 16.3 asks a tool
  // with no assertion API to report immediate cover results at the end of
  // simulation, and a record the design left open is written out rather than
  // dropped.
  ReportCoverage();
  stream_.Drain();
  return diagnostic_.ReportedFatal() || tool_failed_ ? 1 : 0;
}

void Runtime::RunSimulation() {
  try {
    // LRM 4: every scope initializes before any activates, so a time-zero
    // initializer reads sealed endpoints and no process runs before all of
    // them have.
    WalkInitialize(design_->Root());
    WalkActivate(design_->Root());
    RegisterProcesses();

    // LRM 4.4: slots run in time order and the simulator never goes backwards,
    // so the earliest pending slot is always the next one.
    while (std::holds_alternative<Running>(state_)) {
      auto slot = slots_.begin();
      if (slot == slots_.end()) {
        break;
      }
      now_ = slot->first;
      ExecuteTimeSlot(slot->second);
      if (!std::holds_alternative<Running>(state_)) {
        break;
      }
      slots_.erase(slot);
    }

    // LRM 9.2.3: a final procedure occurs at the end of simulation time, which
    // a run reaches by exhausting its work as much as by being asked to end. A
    // tool that cannot carry the run on has no such end to offer, and running
    // the design's procedures over a state already known to be wrong is not
    // one.
    const bool ends_the_simulation = std::visit(
        Overloaded{
            [](const Running&) { return true; },
            [](const SimulationEnded&) { return true; },
            [](const ToolStopped&) { return false; }},
        state_);
    if (ends_the_simulation) {
      // An evaluation attempt still in flight is one no tick will settle now,
      // so it takes the answer its statement demanded of a pending result
      // (Annex F.5.3.2) -- before the finals, which read what its statements
      // wrote.
      for (EvaluationAttempts* attempts : concurrent_assertions_) {
        attempts->SettleAtEndOfRun();
      }
      ExecuteFinalProcesses();
    }
  } catch (const std::exception&) {
    // A control effect is not derived from this hierarchy (LRM 9.6.2), so one
    // reaching here left its owner: that is a defect of the tool, and it
    // surfaces rather than being reported as something the design did.
    ReportRaisedError(*this, std::current_exception());
  }
}

void Runtime::RequestSimulationEnd() {
  ++end_requests_;
  state_ = SimulationEnded{};
}

void Runtime::RequestToolStop() {
  ++end_requests_;
  state_ = ToolStopped{};
}

void Runtime::ReportDesignError(std::string_view message) {
  diagnostic_.Report(Severity::kFatal, message);
  RequestSimulationEnd();
}

void Runtime::ReportToolFailure(std::string_view message) {
  tool_failed_ = true;
  diagnostic_.Note(std::format("lyra: {}", message));
  RequestToolStop();
}

void Runtime::ReportSimulationControl(
    std::string_view task, std::string_view origin, int level) {
  if (level <= 0) {
    return;
  }
  std::string line;
  if (!origin.empty()) {
    line += origin;
    line += ": ";
  }
  line += std::format("{} at time {}", task, now_);
  if (level >= 2) {
    line += std::format(", {:.3f}s of processor time", ProcessorSeconds());
  }
  diagnostic_.Note(line);
}

auto Runtime::ReportContext() const -> std::string {
  const Scope* scope =
      current_process_ == nullptr ? nullptr : current_process_->OwningScope();
  if (scope == nullptr) {
    return std::format("time {}", now_);
  }
  return std::format(
      "{} at time {}", std::string_view{scope->HierarchicalPath().View()},
      now_);
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
      case ProcessKind::kDetached:
        throw InternalError(
            "Runtime::RegisterProcesses: an execution created during "
            "simulation must not appear in static scope registration");
    }
  }
}

void Runtime::RegisterProcessInRegistry(
    std::shared_ptr<RuntimeProcess> process) {
  processes_.push_back(std::move(process));
}

void Runtime::EnterStaticInit(RandomSeed seed) {
  displacing_rngs_.push_back(
      DisplacingRng{.rng = DrawRng{seed}, .displaced = drawing_rng_});
  drawing_rng_ = &displacing_rngs_.back().rng;
}

void Runtime::LeaveStaticInit() {
  drawing_rng_ = displacing_rngs_.back().displaced;
  displacing_rngs_.pop_back();
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
      ReportDesignError(
          "the current time slot did not settle: the design keeps "
          "scheduling work without advancing time");
      return;
    }
    RunRegion(slot, *region);
  }
  RunRegion(slot, Region::kPostponed);
  if (std::holds_alternative<Running>(state_) && !slot.Empty()) {
    ReportDesignError(
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
  const std::uint64_t requests_before = end_requests_;
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
    // LRM 9.2.3: a `$finish` reached inside a final procedure ends the
    // simulation immediately, so the ones still queued do not run. Nothing else
    // can suspend one, because the statements a final procedure may contain are
    // those a function may, so a suspension without a further request is a
    // lowering that admitted one it cannot.
    if (end_requests_ != requests_before) {
      break;
    }
    throw InternalError(
        "Runtime::ExecuteFinalProcesses: a final procedure suspended, which "
        "the statements it may contain cannot do (LRM 9.2.3)");
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
  // Where an ending stops the design: no process resumes after one. Deferred
  // effects the slot already holds still run, and a `final` body reaches its
  // statements through its own path.
  if (!std::holds_alternative<Running>(state_)) {
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

void EnterScopeStaticInit(RuntimeEffects& runtime, Scope* unit_instance) {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  static_cast<Runtime&>(runtime).EnterStaticInit(
      unit_instance->InitializationSeeds().NextSeed());
}

void EnterNamespaceStaticInit(RuntimeEffects& runtime) {
  // A namespace is not instantiated, so nothing holds its seeds across runs of
  // anything: its initialization RNG exists for this one bring-up and hands out
  // the one seed it is ever asked for. Starting a fresh one here is that
  // generator, which is what gives every package the same starting point and
  // keeps one package's draws out of another's (LRM 18.14.1).
  InitializationRng seeds;
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  static_cast<Runtime&>(runtime).EnterStaticInit(seeds.NextSeed());
}

void LeaveStaticInit(RuntimeEffects& runtime) {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  static_cast<Runtime&>(runtime).LeaveStaticInit();
}

}  // namespace lyra::runtime
