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
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/owned_call.hpp"
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

auto Runtime::DesignRoot() -> Scope& {
  if (design_ == nullptr) {
    throw InternalError(
        "Runtime::DesignRoot: no design is bound -- please report this as a "
        "bug");
  }
  return design_->Root();
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
    //
    // Neither stretch runs in a process, so whatever leaves one lands here. An
    // initializer that ends the run -- by asking (LRM 20.2) or by an error of
    // the design (LRM 20.10) -- leaves the initialization itself, and what is
    // left of it does not run. The rest of the run still walks to its end --
    // creating a process runs none of its statements, and the final procedures
    // run because the simulation reached its end (LRM 9.2.3). Creating
    // processes is therefore left only by a failure of the tool, which is
    // reported and ends the run.
    RunAsLanding(*this, [this] { WalkInitialize(design_->Root()); });
    RunAsLanding(*this, [this] { WalkActivate(design_->Root()); });

    // LRM 4.4: slots run in time order and the simulator never goes backwards,
    // so the earliest pending slot is always the next one. Time moves only to a
    // slot that holds something (LRM 4.5): one whose every event was taken back
    // -- the delay of a process stopped or killed before its time came -- is
    // passed over where it stands.
    while (std::holds_alternative<Running>(state_)) {
      auto slot = slots_.begin();
      if (slot == slots_.end()) {
        break;
      }
      if (slot->second.Empty()) {
        slots_.erase(slot);
        continue;
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
  line += std::format("{} at time {}", task, now_.load());
  if (level >= 2) {
    line += std::format(", {:.3f}s of processor time", ProcessorSeconds());
  }
  diagnostic_.Note(line);
}

auto Runtime::ReportContext() const -> DiagnosticDispatcher::Context {
  const RuntimeProcess* const process = current_process_;
  const Scope* scope = process == nullptr ? nullptr : process->OwningScope();
  if (scope == nullptr) {
    return {
        .scope_and_time = std::format("time {}", now_.load()), .procedure = {}};
  }
  return {
      .scope_and_time = std::format(
          "{} at time {}", std::string_view{scope->HierarchicalPath().View()},
          now_.load()),
      .procedure = process->WrittenAt()};
}

void Runtime::ReportUnsettledSlot(std::span<const StillRunning> still_running) {
  ReportDesignError("the design keeps scheduling work and time never advances");
  const std::span<const StillRunning> named =
      still_running.first(std::min(still_running.size(), kMaxNamedProcedures));
  for (const StillRunning& procedure : named) {
    const std::string_view written_at = procedure.written_at;
    diagnostic_.Note(
        std::format(
            "note: {}{}this procedure in {} ran {} times in the last {} passes",
            written_at, written_at.empty() ? "" : ": ",
            std::string_view{procedure.scope->HierarchicalPath().View()},
            procedure.runs, kNotedRegionPasses));
  }
  if (named.size() < still_running.size()) {
    diagnostic_.Note(
        std::format(
            "note: and {} more procedures",
            still_running.size() - named.size()));
  }
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

void Runtime::RegisterProcessInRegistry(
    std::shared_ptr<RuntimeProcess> process) {
  processes_.push_back(std::move(process));
}

void Runtime::QueueFinal(Activation* top) {
  top->Queue(finals_);
}

auto Runtime::ClaimNamespaceInitialization(std::string_view name) -> bool {
  return initialized_namespaces_.emplace(name).second;
}

void Runtime::EnterStaticInit(RandomSeed seed) {
  displacing_.push_back(
      DisplacingState{
          .state = RunningState{.rng = DrawRng{seed}, .import_calls = {}},
          .displaced = running_});
  running_ = &displacing_.back().state;
}

void Runtime::LeaveStaticInit() {
  running_ = displacing_.back().displaced;
  displacing_.pop_back();
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
  RunRegion(slot, Region::kPreponed, nullptr);
  std::size_t passes = 0;
  std::vector<StillRunning> still_running;
  // LRM 4.5: take the earliest region that has anything, run it, and look
  // again -- what it produced lands back in the slot. The reactive group comes
  // after Observed in the order, so it is reached only once the active group is
  // empty, and work it schedules back into Active is found first on the next
  // look.
  while (std::optional<Region> region =
             slot.FirstPending(Region::kActive, Region::kReNba)) {
    if (++passes > kMaxRegionPassesPerSlot) {
      ReportUnsettledSlot(still_running);
      return;
    }
    const bool noted = passes > kMaxRegionPassesPerSlot - kNotedRegionPasses;
    RunRegion(slot, *region, noted ? &still_running : nullptr);
  }
  RunRegion(slot, Region::kPostponed, nullptr);
  if (std::holds_alternative<Running>(state_) && !slot.Empty()) {
    ReportDesignError(
        "the postponed region scheduled work back into the time slot that "
        "ends with it (LRM 4.4.2.9)");
  }
}

void Runtime::RunRegion(
    TimeSlot& slot, Region region, std::vector<StillRunning>* noted) {
  RegionQueue& queue = slot[region];
  // LRM 9.3.2: work arriving while this pass runs belongs to the next pass, so
  // both snapshots move out of the region and new arrivals accumulate behind
  // them. LRM 4.5 fixes no order between the events of one region.
  std::vector<OwnedCall> effects = std::move(queue.effects);
  queue.effects.clear();
  queue.activations.SpliceBackOnto(draining_);
  for (OwnedCall& effect : effects) {
    effect();
  }
  while (QueuePlace* queued = draining_.PopFront()) {
    Activation* activation = queued->activation;
    ConsumeWait(activation);
    if (noted != nullptr) {
      // Taken before the process runs, which may be the run that ends it.
      const RuntimeProcess& process = activation->Process();
      const auto same =
          std::ranges::find_if(*noted, [&](const StillRunning& procedure) {
            return procedure.written_at == process.WrittenAt() &&
                   procedure.scope == process.OwningScope();
          });
      if (same == noted->end()) {
        noted->push_back(
            StillRunning{
                .written_at = process.WrittenAt(),
                .scope = process.OwningScope(),
                .runs = 1});
      } else {
        ++same->runs;
      }
    }
    RunProcess(activation);
  }
}

void Runtime::ExecuteFinalProcesses() {
  const std::uint64_t requests_before = end_requests_;
  while (QueuePlace* queued = finals_.PopFront()) {
    Activation* activation = queued->activation;
    const bool completed = activation->Process().ResumeWith(*this, activation);
    // LRM 9.2.3: a `$finish` reached inside a final procedure ends the
    // simulation immediately, so the ones still queued do not run. The request
    // is what says so, however the procedure itself came to an end.
    if (end_requests_ != requests_before) {
      break;
    }
    // The statements a final procedure may contain are those a function may, so
    // one that suspended is a lowering that admitted one it cannot.
    if (!completed) {
      throw InternalError(
          "Runtime::ExecuteFinalProcesses: a final procedure suspended, which "
          "the statements it may contain cannot do (LRM 9.2.3)");
    }
  }
  finals_.Clear();
}

void Runtime::RunProcess(Activation* activation) {
  // Where an ending stops the design: no process resumes after one. Deferred
  // effects the slot already holds still run, and a `final` body reaches its
  // statements through its own path.
  if (!std::holds_alternative<Running>(state_)) {
    return;
  }
  // No wait dispatch: an execution that stopped to wait arranged its own way
  // back before it gave up control. The owning process is taken before the
  // resume, since `activation` may be an enabled task's frame that is
  // destroyed as control returns up the enable chain.
  RuntimeProcess& process = activation->Process();
  if (!process.ResumeWith(*this, activation)) {
    return;
  }
  // The terminal transition already woke what awaits this process (LRM 9.3.2,
  // 9.7). Its parent's `wait fork` may hold now that it has one live child
  // fewer (LRM 9.6.1), which is asked while the node is still linked.
  if (RuntimeProcess* parent = process.Parent(); parent != nullptr) {
    WakeParkedOn(parent->WaitForkCondition(), Change::Whole());
  }
  // Releasing destroys `process` and every ancestor the release leaves with no
  // lineage to retain, so no statement may follow it here.
  RuntimeProcess::ReleaseTerminatedLineage(process);
}

void RegisterInitialProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at) {
  Runtime& rt = AsRuntime(current_runtime());
  auto process = std::make_shared<RuntimeProcess>(
      owning_scope, std::move(coroutine),
      unit_instance->InitializationSeeds().NextSeed(), written_at);
  // LRM 9.2: an `initial` or `always` starts on the Active queue at time 0.
  rt.Schedule(rt.Now(), Region::kActive, process->TopActivation());
  rt.RegisterProcessInRegistry(std::move(process));
}

void RegisterFinalProcess(
    Scope* owning_scope, Scope* unit_instance, Coroutine<void> coroutine,
    const char* written_at) {
  Runtime& rt = AsRuntime(current_runtime());
  auto process = std::make_shared<RuntimeProcess>(
      owning_scope, std::move(coroutine),
      unit_instance->InitializationSeeds().NextSeed(), written_at);
  rt.QueueFinal(process->TopActivation());
  rt.RegisterProcessInRegistry(std::move(process));
}

void EnterScopeStaticInit(RuntimeEffects& runtime, Scope* unit_instance) {
  AsRuntime(runtime).EnterStaticInit(
      unit_instance->InitializationSeeds().NextSeed());
}

void EnterNamespaceStaticInit(RuntimeEffects& runtime) {
  // A namespace is not instantiated, so nothing holds its seeds across runs of
  // anything: its initialization RNG exists for this one bring-up and hands out
  // the one seed it is ever asked for. Starting a fresh one here is that
  // generator, which is what gives every package the same starting point and
  // keeps one package's draws out of another's (LRM 18.14.1).
  InitializationRng seeds;
  AsRuntime(runtime).EnterStaticInit(seeds.NextSeed());
}

auto ClaimNamespaceInitialization(RuntimeEffects& runtime, const char* name)
    -> std::int64_t {
  return AsRuntime(runtime).ClaimNamespaceInitialization(name) ? 1 : 0;
}

void LeaveStaticInit(RuntimeEffects& runtime) {
  AsRuntime(runtime).LeaveStaticInit();
}

}  // namespace lyra::runtime
