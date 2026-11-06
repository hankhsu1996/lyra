#include "lyra/runtime/runtime_effects.hpp"

#include <cstdint>
#include <exception>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/assertion_control.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/delay.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/loop_blocks.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/program_threads.hpp"
#include "lyra/runtime/property_wait.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/sampled_value.hpp"
#include "lyra/runtime/source_follower.hpp"
#include "lyra/runtime/timer.hpp"
#include "lyra/runtime/value_dump.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/support/assertion_control.hpp"
#include "lyra/value/format.hpp"

namespace lyra::runtime {

auto AsRuntime(RuntimeEffects& effects) -> Runtime& {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  return static_cast<Runtime&>(effects);
}

auto AsRuntime(const RuntimeEffects& effects) -> const Runtime& {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  return static_cast<const Runtime&>(effects);
}

namespace {

auto CurrentRuntimeSlot() -> RuntimeEffects*& {
  thread_local RuntimeEffects* slot = nullptr;
  return slot;
}

// Where a nonblocking assignment the running thread makes updates: NBA in the
// design, Re-NBA in a program's thread, wherever the code was declared (LRM
// 24.3.1, 24.5).
auto RunningNbaRegion(RuntimeEffects& effects) -> support::Region {
  return AsRuntime(effects).RunningContext().regions.nba;
}

// What a violation report records where it is created: the executing process's
// pass and nothing more, which is the whole of the flush-point set LRM 12.4.2.1
// lists.
auto ViolationReportValidity(RuntimeProcess& process)
    -> DeferredReportValidity {
  return DeferredReportValidity{
      .pass = process.CurrentDeferredReportEpoch(), .targets = {}};
}

// What a deferred assertion report records: the same pass, and in addition the
// disable targets that can withdraw it on their own (LRM 16.4.2, 16.4.4). The
// two constructs differ in what they record and in nothing else; both act only
// if everything they recorded still stands.
auto AssertionReportValidity(RuntimeProcess& process)
    -> DeferredReportValidity {
  return DeferredReportValidity{
      .pass = process.CurrentDeferredReportEpoch(),
      .targets = process.DeferredReportTargets()};
}

// A deferred assertion's action, started once it is due, as an execution of no
// lineage in the context of whatever reached the statement. No thread creates
// it, so its generator is the next of that context's initialization RNG (LRM
// 18.14.1), and the draws of whatever reached the statement do not move.
auto StartWhenDue(RuntimeEffects& effects, Coroutine<void> action) {
  Runtime& runtime = AsRuntime(effects);
  const ExecutionContext context = runtime.RunningContext().WithoutLineage();
  return [&runtime, context, rng = context.SplitInitializationRng(),
          action = std::move(action)]() mutable {
    runtime.RunAction(context, std::move(action), rng);
  };
}

}  // namespace

auto current_runtime() -> RuntimeEffects& {
  RuntimeEffects* handle = CurrentRuntimeSlot();
  if (handle == nullptr) {
    throw InternalError(
        "current_runtime(): called on a thread with no attached Runtime");
  }
  return *handle;
}

auto ProcedureMakingEffects() -> const ProcedureContext& {
  const RuntimeEffects* attached = CurrentRuntimeSlot();
  return attached == nullptr ? kNoProcedure
                             : AsRuntime(*attached).RunningProcedure();
}

void HandOverExpiry(IntrusiveList<WaitMembership>& expiry) noexcept {
  RuntimeEffects* attached = CurrentRuntimeSlot();
  if (attached == nullptr) return;
  AsRuntime(*attached).expired_.Append(expiry);
}

CurrentRuntimeGuard::CurrentRuntimeGuard(RuntimeEffects& effects)
    : previous_(CurrentRuntimeSlot()) {
  CurrentRuntimeSlot() = &effects;
}

CurrentRuntimeGuard::~CurrentRuntimeGuard() {
  CurrentRuntimeSlot() = previous_;
}

ProcessExecutionGuard::ProcessExecutionGuard(
    RuntimeEffects& effects, RuntimeProcess& process)
    : effects_(&effects),
      previous_process_(
          AsRuntime(effects).current_process_.load(std::memory_order_relaxed)),
      previous_running_(
          std::exchange(AsRuntime(effects).running_, &process.Running())) {
  AsRuntime(effects).current_process_.store(
      &process, std::memory_order_relaxed);
}

ProcessExecutionGuard::~ProcessExecutionGuard() {
  AsRuntime(*effects_).current_process_.store(
      previous_process_, std::memory_order_relaxed);
  AsRuntime(*effects_).running_ = previous_running_;
}

auto RuntimeEffects::Diagnostic() -> DiagnosticDispatcher& {
  return AsRuntime(*this).diagnostic_;
}

auto RuntimeEffects::Files() -> FileTable& {
  return AsRuntime(*this).files_;
}

auto RuntimeEffects::Dumps() -> ValueDumps& {
  return AsRuntime(*this).dumps_;
}

auto RuntimeEffects::Randomize() -> RandomizeSettings& {
  return AsRuntime(*this).randomize_settings_;
}

auto RuntimeEffects::PlusArgs() -> PlusArgsSource& {
  return AsRuntime(*this).plusargs_;
}

void RuntimeEffects::RecordCoverage(base::SourceLocation site, bool succeeded) {
  AsRuntime(*this).coverage_.Record(site, succeeded);
}

void RuntimeEffects::Schedule(
    SimTime when, support::Region region, Activation* activation) {
  activation->Queue(AsRuntime(*this).SlotAt(when)[region].activations);
}

void RuntimeEffects::Schedule(
    SimTime when, support::Region region, Timer& timer) {
  AsRuntime(*this).SlotAt(when)[region].timers.PushBack(timer.Place());
}

void RuntimeEffects::Wake(Activation* activation) {
  ConsumeWait(activation);
  const ExecutionContext& context = activation->Process().Context();
  switch (context.wake) {
    case WakePlacement::kActiveRegion:
      Schedule(Now(), context.regions.active, activation);
      return;
    case WakePlacement::kAtTheOccurrence:
      AsRuntime(*this).woken_at_the_occurrence_.push_back(activation);
      RunWokenAtTheOccurrence();
      return;
  }
  throw InternalError("RuntimeEffects::Wake: unknown wake placement");
}

auto RuntimeEffects::EffectsAt(SimTime when, support::Region region)
    -> DeferredEffects& {
  return AsRuntime(*this).SlotAt(when)[region].effects;
}

auto RuntimeEffects::NbaEffects() -> DeferredEffects* {
  return &EffectsAt(Now(), RunningNbaRegion(*this));
}

auto RuntimeEffects::NbaEffectsAfter(
    const value::ConstIntegralView& duration, std::int64_t unit_power,
    std::int64_t precision_power) -> DeferredEffects* {
  return &EffectsAt(
      DeadlineAfterAmount(*this, duration, unit_power, precision_power),
      RunningNbaRegion(*this));
}

auto RuntimeEffects::NbaEffectsAfterReal(
    const value::Real& duration, std::int64_t unit_power,
    std::int64_t precision_power) -> DeferredEffects* {
  return &EffectsAt(
      DeadlineAfterRealAmount(*this, duration, unit_power, precision_power),
      RunningNbaRegion(*this));
}

auto RuntimeEffects::RegionEffectsAfter(
    std::int64_t region, const value::ConstIntegralView& duration,
    std::int64_t unit_power, std::int64_t precision_power) -> DeferredEffects* {
  return &EffectsAt(
      DeadlineAfterAmount(*this, duration, unit_power, precision_power),
      RegionOperand(region));
}

auto RuntimeEffects::RegionEffectsAfterReal(
    std::int64_t region, const value::Real& duration, std::int64_t unit_power,
    std::int64_t precision_power) -> DeferredEffects* {
  return &EffectsAt(
      DeadlineAfterRealAmount(*this, duration, unit_power, precision_power),
      RegionOperand(region));
}

auto RuntimeEffects::PassEndEffects() -> DeferredEffects* {
  return &AsRuntime(*this).pass_end_effects_;
}

auto RuntimeEffects::PostponedEffects() -> DeferredEffects* {
  return &EffectsAt(Now(), support::Region::kPostponed);
}

void RuntimeEffects::RegisterConcurrentAssertion(EvaluationAttempts& attempts) {
  AsRuntime(*this).concurrent_assertions_.push_back(&attempts);
}

auto RuntimeEffects::ObservedEffects() -> DeferredEffects* {
  return &EffectsAt(Now(), support::Region::kObserved);
}

void RuntimeEffects::KeepViolationReport(
    void* made, const AssertionControl& control) {
  DeferredEffects& observed = *ObservedEffects();
  const CapturedControl queued_under{
      .control = &control, .generation = control.Generation()};
  RuntimeProcess* process = TryCurrentProcess();
  // The condition kept on the report holds it, so the two end together,
  // whether or not the condition lets it run; the report's own place in the
  // order is never committed, since the condition stands in it.
  if (process == nullptr) {
    // A check that fires with no process running -- in a static variable's
    // initializer (LRM 6.8), or in a function a deferred effect calls, as a
    // `$strobe` argument does -- belongs to no deferred report queue, so no
    // flush point clears it, and only a Kill of its control withdraws it
    // (LRM 12.4.2.1, 20.11).
    observed.Emplace([queued_under, report = observed.Take(made)] {
      if (queued_under.Holds()) report.Call();
    });
    return;
  }
  observed.Emplace([validity = ViolationReportValidity(*process), queued_under,
                    report = observed.Take(made)] {
    // LRM 12.4.2.1: a pass that no longer stands is a flush point the
    // process reached before this report could mature.
    if (validity.Holds() && queued_under.Holds()) {
      report.Call();
    }
  });
}

void RuntimeEffects::SubmitDeferredObserved(
    Coroutine<void> body, const AssertionControl& control) {
  auto action = StartWhenDue(*this, std::move(body));
  const CapturedControl queued_under{
      .control = &control, .generation = control.Generation()};
  RuntimeProcess* process = AsRuntime(*this).current_process_;
  if (process == nullptr) {
    // A deferred assertion outside any process (a static variable's
    // initializer, LRM 6.8) belongs to no deferred report queue, so no flush
    // point can clear it, and only a Kill of its control withdraws it.
    Submit(
        Now(), support::Region::kObserved,
        [this, queued_under, action = std::move(action)]() mutable {
          if (!queued_under.Holds()) return;
          Submit(Now(), support::Region::kReactive, std::move(action));
        });
    return;
  }
  // LRM 16.4.1: the report is queued now and matures in Observed only if every
  // source it recorded still stands, then runs its action in the Reactive
  // region. Maturing discards the record, so the committed Reactive effect is
  // beyond the reach of any later flush, `disable` or Kill.
  Submit(
      Now(), support::Region::kObserved,
      [this, validity = AssertionReportValidity(*process), queued_under,
       action = std::move(action)]() mutable {
        if (!validity.Holds() || !queued_under.Holds()) return;
        Submit(Now(), support::Region::kReactive, std::move(action));
      });
}

void RuntimeEffects::SubmitDeferredFinal(
    Coroutine<void> body, const AssertionControl& control) {
  auto action = StartWhenDue(*this, std::move(body));
  const CapturedControl queued_under{
      .control = &control, .generation = control.Generation()};
  RuntimeProcess* process = AsRuntime(*this).current_process_;
  if (process == nullptr) {
    Submit(
        Now(), support::Region::kPostponed,
        [queued_under, action = std::move(action)]() mutable {
          if (queued_under.Holds()) action();
        });
    return;
  }
  // LRM 16.4.1: a final deferred assertion matures and runs in the Postponed
  // region, still withdrawn by anything that invalidated a source it recorded
  // before then. Postponed is non-iterative, so maturing and running are the
  // one region.
  Submit(
      Now(), support::Region::kPostponed,
      [validity = AssertionReportValidity(*process), queued_under,
       action = std::move(action)]() mutable {
        if (validity.Holds() && queued_under.Holds()) action();
      });
}

namespace {

// The control a task's `control_type` asks for (LRM Table 20-5), none where
// the table gives none, which the run is told of and goes on past.
auto ControlTypeOf(RuntimeEffects& effects, std::int64_t control_type)
    -> std::optional<support::AssertionControlType> {
  if (control_type <
          static_cast<std::int64_t>(support::AssertionControlType::kLock) ||
      control_type > static_cast<std::int64_t>(
                         support::AssertionControlType::kVacuousOff)) {
    effects.Diagnostic().Report(
        Severity::kWarning,
        base::DiagMessage(
            "the assertion control type {} is none the standard defines; the "
            "call is ignored",
            control_type));
    return std::nullopt;
  }
  return static_cast<support::AssertionControlType>(control_type);
}

// A selection mask, as the bits of its low byte: Tables 20-6 and 20-7 give no
// meaning to a higher one.
auto MaskOf(std::int64_t mask) -> std::uint64_t {
  return static_cast<std::uint64_t>(mask) & 0xFFU;
}

}  // namespace

auto RuntimeEffects::AssertionControlsOfTheRun() -> AssertionControls& {
  return AsRuntime(*this).assertion_controls_;
}

void RuntimeEffects::AssertControlOfDesign(
    std::int64_t control_type, std::int64_t types, std::int64_t directives,
    std::int64_t levels, base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  const auto control = ControlTypeOf(*this, control_type);
  if (!control.has_value()) return;
  Runtime& rt = AsRuntime(*this);
  rt.assertion_controls_.ApplyToDesign(
      rt.DesignRoot(), *control, MaskOf(types), MaskOf(directives), levels);
}

void RuntimeEffects::AssertControlOfScope(
    Scope* scope, std::int64_t control_type, std::int64_t types,
    std::int64_t directives, std::int64_t levels, base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  const auto control = ControlTypeOf(*this, control_type);
  if (!control.has_value()) return;
  AsRuntime(*this).assertion_controls_.ApplyToScope(
      *scope, *control, MaskOf(types), MaskOf(directives), levels);
}

void RuntimeEffects::AssertControlOfBlock(
    const CancellationTarget* block, std::int64_t control_type,
    std::int64_t types, std::int64_t directives, base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  const auto control = ControlTypeOf(*this, control_type);
  if (!control.has_value()) return;
  AsRuntime(*this).assertion_controls_.ApplyToBlock(
      *block, *control, MaskOf(types), MaskOf(directives));
}

void RuntimeEffects::AssertControlOfLoop(
    const LoopBlocks* blocks, std::int64_t control_type, std::int64_t types,
    std::int64_t directives, std::int64_t levels, base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  const auto control = ControlTypeOf(*this, control_type);
  if (!control.has_value()) return;
  AssertionControls& controls = AsRuntime(*this).assertion_controls_;
  blocks->ForEach([&](Scope& block) {
    controls.ApplyToScope(
        block, *control, MaskOf(types), MaskOf(directives), levels);
  });
}

// What an occurrence woke to run at it runs in the order it was woken, which is
// the order its waits were reached in, and what waits again while running
// waits behind every wait already reached; so executions that began to wait on
// one event in some order run in that order at each occurrence they wait out.
// A clocking block's drives land in the order they were made by this (LRM
// 14.16.2).
void RuntimeEffects::RunWokenAtTheOccurrence() {
  Runtime& rt = AsRuntime(*this);
  if (rt.occurrences_being_told_ != 0) return;
  while (!rt.woken_at_the_occurrence_.empty()) {
    std::vector<Activation*> woken;
    woken.swap(rt.woken_at_the_occurrence_);
    for (Activation* activation : woken) {
      RuntimeProcess::RunUndispatched(*this, activation);
    }
  }
}

namespace {

// The extent of telling one occurrence to the waits it reaches, which ends
// however the telling does. What it wakes to run at it runs once the outermost
// such extent has ended.
class TellingAnOccurrence {
 public:
  explicit TellingAnOccurrence(int& depth) : depth_(depth) {
    ++depth_;
  }
  TellingAnOccurrence(const TellingAnOccurrence&) = delete;
  auto operator=(const TellingAnOccurrence&) -> TellingAnOccurrence& = delete;
  TellingAnOccurrence(TellingAnOccurrence&&) = delete;
  auto operator=(TellingAnOccurrence&&) -> TellingAnOccurrence& = delete;
  ~TellingAnOccurrence() {
    --depth_;
  }

 private:
  int& depth_;
};

}  // namespace

void RuntimeEffects::WakeParkedOn(
    IntrusiveList<WaitMembership>& members, const Change& change) {
  const auto told = [&] {
    const TellingAnOccurrence telling(AsRuntime(*this).occurrences_being_told_);
    members.ForEach([&](WaitMembership& membership) {
      if (change.KnownUnchanged(membership.reads)) {
        return;
      }
      std::visit(
          Overloaded{
              [&](Awaiter* awaiter) {
                if (Activation* woken = awaiter->WokenBy(membership)) {
                  Wake(woken);
                }
              },
              [](SampledValue* sampled) { sampled->Reached(); },
              [](SourceFollower* follower) { follower->Reached(); },
              [this](ProgramThreads* program) { program->RootEnded(*this); },
              [&](PropertyEvaluation* evaluation) {
                evaluation->Reached(membership);
              },
              [](DumpWatch* watch) { watch->Reached(); }},
          membership.observer);
    });
  };
  told();
  RunWokenAtTheOccurrence();
}

void RuntimeEffects::WakeParkedOn(
    Observable& place, const PartPath& written, const Change& change) {
  const auto told = [&] {
    const TellingAnOccurrence telling(AsRuntime(*this).occurrences_being_told_);
    place.WakeAffectedBy(*this, written, change);
  };
  told();
  RunWokenAtTheOccurrence();
}

auto RuntimeEffects::Bound() -> BoundMembers& {
  return AsRuntime(*this).bound_members_;
}

void RuntimeEffects::DriveContinuously(const VariableCell& variable) {
  AsRuntime(*this).driven_continuously_.insert(&variable);
}

auto RuntimeEffects::DrivenContinuously(const VariableCell& variable) const
    -> bool {
  return AsRuntime(*this).driven_continuously_.contains(&variable);
}

void RuntimeEffects::EndRun(std::string_view task, std::int64_t finish_number) {
  Runtime& rt = AsRuntime(*this);
  rt.ReportSimulationControl(task, static_cast<int>(finish_number));
  rt.RequestSimulationEnd();
}

void ReportRaisedError(
    RuntimeEffects& effects, const std::exception_ptr& raised) {
  Runtime& rt = AsRuntime(effects);
  // Asked rather than looked at: an exception put away as an `exception_ptr`
  // answers what it is only by being raised again into a handler. A control
  // effect is not derived from this hierarchy (LRM 9.6.2), so one arriving here
  // left its owner: that is a defect of the tool, and it surfaces rather than
  // being reported as something the design did.
  try {
    std::rethrow_exception(raised);
  } catch (const SimulationError& error) {
    rt.ReportDesignError(error.At(), base::DiagMessage("{}", error.what()));
  } catch (const InternalError& error) {
    rt.ReportToolFailure(std::format("internal error: {}", error.what()));
  } catch (const std::exception& error) {
    rt.ReportToolFailure(
        std::format(
            "internal error: an exception nothing handles ({})\n{}",
            error.what(), BugReportRequest()));
  }
}

void RequireCallerMayScheduleAfterReturn(
    RuntimeEffects& effects, base::SourceLocation at) {
  if (!CreatedByInitialOrAlways(AsRuntime(effects).RunningContext().creator)) {
    const base::SourceLocationScope bound(at);
    throw SimulationError(
        "a function cannot schedule what takes effect after it returns "
        "unless the thread calling it was created by an initial or always "
        "procedure or a fork of one");
  }
}

auto RuntimeEffects::Spawn(Coroutine<void> coroutine)
    -> std::shared_ptr<RuntimeProcess> {
  Runtime& rt = AsRuntime(*this);
  if (rt.current_process_ == nullptr) {
    throw InternalError(
        "RuntimeEffects::Spawn: no ambient process to parent the branch to");
  }
  RuntimeProcess& parent = *rt.current_process_.load();
  // Hierarchical seeding (LRM 18.14.1): the branch's generator is taken out of
  // the spawner's, so a whole subtree of threads follows from the seed of the
  // one at its root and the order the branches then run in does not move any
  // of it.
  auto child = RuntimeProcess::Create(
      parent.Context().ForStarted(), std::move(coroutine),
      parent.Generator().Split());
  Activation* const activation = child->TopActivation();
  // The spawned activity is enabled within whatever disable targets the spawner
  // is inside (LRM 9.6.2), so it takes that membership here rather than
  // rebuilding it once it starts running: it is spawned already enclosed, and a
  // `disable` landing before its first resumption still reaches it.
  child->InheritEnclosingTargets(parent);
  parent.Children().Adopt(child);
  Schedule(Now(), child->Context().regions.active, activation);
  return child;
}

void RuntimeEffects::RunDetached(Coroutine<void> coroutine) {
  StartDetached(
      std::move(coroutine), AsRuntime(*this).RunningContext().WithoutLineage());
}

void RuntimeEffects::RunObserving(Coroutine<void> coroutine) {
  StartDetached(
      std::move(coroutine),
      AsRuntime(*this).RunningContext().WokenAtTheOccurrence());
}

void RuntimeEffects::StartDetached(
    Coroutine<void> coroutine, const ExecutionContext& context) {
  Runtime& rt = AsRuntime(*this);
  // What it waits for is evaluated there and again at each candidacy, so it
  // draws from a generator of its own. No thread creates it, so that generator
  // is the next of the context's initialization RNG (LRM 18.14.1), and the
  // draws of whatever reached the statement do not move.
  auto execution = RuntimeProcess::Create(
      context, std::move(coroutine), context.SplitInitializationRng());
  Activation* const activation = execution->TopActivation();
  // No lineage and no disable membership: the standard makes a process of
  // neither an update it carries out nor a covergroup's sampling, so nothing
  // that names processes may find it (LRM 9.6.1, 9.6.3). What keeps it alive
  // until it ends is therefore the runtime itself.
  rt.AdoptProcess(execution);
  // Reached where nothing executes -- a static initialization making a
  // covergroup sampled on its event (LRM 19.3) -- it starts with the
  // processes, since nothing it waits for can happen before they run.
  if (rt.current_process_ == nullptr) {
    Schedule(Now(), execution->Context().regions.active, activation);
    return;
  }
  // It runs now, as the statement that reached it, until it first waits: the
  // statement executes the control it carries (LRM 9.4.5). So the object an
  // event names and the value a change is measured from are those of that
  // moment, a change the process makes after the statement is one the wait
  // sees, and an error evaluating the control is the statement's.
  RuntimeProcess::RunUndispatched(*this, activation);
}

void RuntimeEffects::SubmitAtRunEnd(
    OwnedClosure<support::EffectSignature> closure) {
  Runtime& rt = AsRuntime(*this);
  rt.at_run_end_.push_back(
      OwedAtRunEnd{
          .procedure = rt.RunningProcedure(), .call = std::move(closure)});
}

auto RuntimeEffects::CurrentProcess() -> RuntimeProcess& {
  RuntimeProcess* p = AsRuntime(*this).current_process_;
  if (p == nullptr) {
    throw InternalError(
        "RuntimeEffects::CurrentProcess: no process is currently executing");
  }
  return *p;
}

auto RuntimeEffects::HasCurrentProcess() const -> bool {
  return AsRuntime(*this).current_process_ != nullptr;
}

auto RuntimeEffects::TryCurrentProcess() -> RuntimeProcess* {
  return AsRuntime(*this).current_process_;
}

auto RuntimeEffects::Running() -> RunningState& {
  RunningState* running = AsRuntime(*this).running_;
  if (running == nullptr) {
    throw InternalError(
        "RuntimeEffects::Running: a generated body reached the runtime with no "
        "process and no static initialization running -- please report this as "
        "a bug");
  }
  return *running;
}

auto RuntimeEffects::TryRunning() -> RunningState* {
  return AsRuntime(*this).running_;
}

auto RuntimeEffects::UnseededRandomSeed() -> std::int32_t& {
  return AsRuntime(*this).unseeded_random_seed_;
}

void RuntimeEffects::ReportDesignFailure(const base::DiagMessage& message) {
  AsRuntime(*this).ReportDesignError(
      base::BoundSourceLocation(message.Text()), message);
}

void RuntimeEffects::ReportDesignFailure(
    base::NoSourceLocation no_location, const base::DiagMessage& message) {
  AsRuntime(*this).ReportDesignError(
      static_cast<base::SourceLocation>(no_location), message);
}

auto RuntimeEffects::Now() const -> SimTime {
  return AsRuntime(*this).now_;
}

auto RuntimeEffects::GlobalPrecisionPower() const -> std::int8_t {
  return AsRuntime(*this).global_precision_power_;
}

auto RuntimeEffects::TimeFormat() const -> const value::TimeFormat& {
  return AsRuntime(*this).time_format_;
}

void RuntimeEffects::SetTimeFormat(
    std::int64_t units_power, std::int64_t precision,
    const value::String& suffix, std::int64_t min_width) {
  AsRuntime(*this).time_format_ = value::TimeFormat{
      .units_power = static_cast<std::int8_t>(units_power),
      .precision = static_cast<std::int32_t>(precision),
      .suffix = std::string(suffix.View()),
      .min_width = static_cast<std::int32_t>(min_width)};
}

void RuntimeEffects::ResetTimeFormat() {
  Runtime& rt = AsRuntime(*this);
  rt.time_format_ = value::TimeFormat{};
  rt.time_format_.units_power = rt.global_precision_power_;
}

}  // namespace lyra::runtime
