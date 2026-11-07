#pragma once

#include <cstdint>
#include <exception>
#include <memory>
#include <string_view>
#include <utility>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/deferred_effects.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/region.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class AssertionControl;
class AssertionControls;
class CancellationTarget;
class DiagnosticDispatcher;
class FileTable;
class LoopBlocks;
class PlusArgsSource;
class BoundMembers;
class VariableCell;
class EvaluationAttempts;
class Observable;
class RuntimeProcess;
class Scope;
class Timer;
class ValueDumps;
class RandomizeSettings;
class RuntimeEffects;

// The runtime capability surface reachable from generated code. Every side
// effect an emitted body performs -- I/O, scheduler verbs, time queries,
// deferred check submission, fork spawn -- lands on a method here. The host
// boundary (BindDesign, Run, elaboration walks) is added by the concrete
// Runtime; a caller holding a `RuntimeEffects&` cannot reach it.
//
// Non-polymorphic base: Runtime is the sole concrete implementation and every
// entry resolves at compile time. Inheritance carries the type-level access
// split without a vtable.
class RuntimeEffects {
 public:
  auto Diagnostic() -> DiagnosticDispatcher&;
  auto Files() -> FileTable&;
  auto PlusArgs() -> PlusArgsSource&;
  // LRM 21.7: the run's value dump, which the design's `$dump*` tasks act on.
  auto Dumps() -> ValueDumps&;
  // LRM 18.6: how the run's `randomize()` calls draw.
  auto Randomize() -> RandomizeSettings&;

  // LRM 16.3: record one evaluation of an immediate cover statement, and
  // whether that evaluation succeeded. `site` is the statement's own source
  // location, which is what separates one coverage goal from another.
  void RecordCoverage(base::SourceLocation site, bool succeeded);

  // LRM 4.4: place `activation` in `region` of the time slot at `when`, to be
  // resumed when that region runs. Naming the placement is the whole of what a
  // suspending construct tells the engine; nothing here says what it waits for.
  void Schedule(SimTime when, support::Region region, Activation* activation);
  // The same for a timer, which fires when that region runs.
  void Schedule(SimTime when, support::Region region, Timer& timer);

  // The wait that parked `activation` is satisfied, so it is placed where its
  // context says a wake places it: runnable in the current slot, in the
  // Active region of its thread's region set -- Reactive for a program's
  // (LRM 4.5, 24.3.1) -- or run at the occurrence itself, once the waits it
  // reached have all been asked and before whatever made it occur goes on
  // (LRM 19.3). Ending the wait belongs to
  // the same step: nothing it was parked on -- the sibling observables of an
  // `@(a or b)`, the event it waited for -- may fire it a second time, and a
  // suspend in the woken-but-not-yet-resumed window then saves a runnable
  // disposition rather than a blocked one.
  void Wake(Activation* activation);

  // Something has happened where the waits enrolled on `members` watch -- a
  // cell took a new value, a named event was triggered, a process terminated.
  // `change` bounds which bits the occurrence could have reached, and bounds
  // nothing where it has no bits to speak of. A membership whose bits it shows
  // untouched is passed over, and every other one asks its awaiter what is
  // parked there now and whether this was an event for it: an event control
  // whose evaluation only reads storage by what its expression is worth now, a
  // wait for a state by whether that state holds, and every other wait by
  // having been reached at all, its process deciding once it runs (LRM 4.5,
  // 9.2.2.2.1, 9.4.2, 15.5.1). Waking only queues and leaves every membership
  // where it is, so each activation is woken as it is found; what is woken to
  // run at the occurrence runs once every membership has been asked, so no
  // code of the design runs while the list is walked.
  void WakeParkedOn(
      IntrusiveList<WaitMembership>& members, const Change& change);

  // The same for a write to the part of `place` at `written`, `change` being
  // what the write did to that part: the waits on the part, on each part
  // holding it and on each part inside it are asked, and no other wait of the
  // place is visited.
  void WakeParkedOn(
      Observable& place, const PartPath& written, const Change& change);

  // The members of scopes that references were bound into, which a force on
  // one acts through (LRM 10.6.2).
  [[nodiscard]] auto Bound() -> BoundMembers&;

  // States that a continuous assignment drives `variable` (LRM 10.3). A
  // variable does not say so itself: the fact is asked only as a procedural
  // continuous assignment comes to cover one (LRM 10.6.2), so it is kept here
  // and a write to a variable costs nothing for it.
  void DriveContinuously(const VariableCell& variable);
  [[nodiscard]] auto DrivenContinuously(const VariableCell& variable) const
      -> bool;

  // LRM 4.4: the deferred effects of `region` in the time slot at `when`, which
  // an effect the library defers is made in and kept by. A deferred effect
  // runs where it is placed and never suspends whoever submitted it.
  [[nodiscard]] auto EffectsAt(SimTime when, support::Region region)
      -> DeferredEffects&;
  template <typename Effect>
  void Submit(SimTime when, support::Region region, Effect&& effect) {
    EffectsAt(when, region).Emplace(std::forward<Effect>(effect));
  }

  // The deferred effects of the regions the program writes an effect into, one
  // entry each: the construct that writes it fixes the region within the
  // writing thread's region set, and the slot is the current one unless the
  // entry takes a delay. The program makes its closure in the storage they
  // hand out and keeps it there, and it runs once, where it lands, unless
  // something withdraws it first.
  [[nodiscard]] auto NbaEffects() -> DeferredEffects*;
  // LRM 9.4.5: a nonblocking effect carrying a delay control schedules its
  // update into the NBA region of its thread's region set -- Re-NBA for a
  // program's -- in the slot that delay names (LRM 4.4.2.4, 4.4.2.8).
  // `duration` is an amount in the scope's time unit, read exactly as a delay
  // control reads one; an integral amount of any type is read through its
  // planes.
  [[nodiscard]] auto NbaEffectsAfter(
      const value::ConstIntegralView& duration, std::int64_t unit_power,
      std::int64_t precision_power) -> DeferredEffects*;
  template <value::IntegralValue T>
  [[nodiscard]] auto NbaEffectsAfter(
      const T& duration, std::int64_t unit_power, std::int64_t precision_power)
      -> DeferredEffects* {
    return NbaEffectsAfter(
        duration.AsWords().View(), unit_power, precision_power);
  }
  [[nodiscard]] auto NbaEffectsAfterReal(
      const value::Real& duration, std::int64_t unit_power,
      std::int64_t precision_power) -> DeferredEffects*;
  // The effects of `region` itself, a position of `support::Region`, in the
  // slot a delay names, whichever region set the writing thread runs in: where
  // a construct the standard names the region of places its effect, as a
  // clocking block keeps in NBA an input's value that skew behind its signal
  // (LRM 14.13).
  [[nodiscard]] auto RegionEffectsAfter(
      std::int64_t region, const value::ConstIntegralView& duration,
      std::int64_t unit_power, std::int64_t precision_power)
      -> DeferredEffects*;
  template <value::IntegralValue T>
  [[nodiscard]] auto RegionEffectsAfter(
      std::int64_t region, const T& duration, std::int64_t unit_power,
      std::int64_t precision_power) -> DeferredEffects* {
    return RegionEffectsAfter(
        region, duration.AsWords().View(), unit_power, precision_power);
  }
  [[nodiscard]] auto RegionEffectsAfterReal(
      std::int64_t region, const value::Real& duration, std::int64_t unit_power,
      std::int64_t precision_power) -> DeferredEffects*;
  // The effects run once the pass of the region running now has run
  // everything it held, and before anything that pass placed: where what the
  // pass did is settled as one, as a clocking block puts onto its output's
  // signal only the last value the pass's drives gave it (LRM 14.16.2).
  [[nodiscard]] auto PassEndEffects() -> DeferredEffects*;
  [[nodiscard]] auto PostponedEffects() -> DeferredEffects*;
  // LRM 16.5: the deferred effects of the Observed region of the current
  // slot, where a concurrent assertion is evaluated once every region in
  // which that step settles the values it reads has run.
  [[nodiscard]] auto ObservedEffects() -> DeferredEffects*;
  // LRM 12.4.2.1: the effects a violation report made now is made in and
  // matures in -- the Observed region's of the current slot, or, once the slot
  // has reached its Postponed region, after which no earlier region runs again,
  // the Postponed region's. The slot has no Observed region left and its
  // active set can glitch no more, so maturing where it was made reports what
  // the Observed region would have, at the same time.
  [[nodiscard]] auto ViolationReportEffects() -> DeferredEffects*;
  // Keeps the violation report made at `made` in those effects, where it
  // matures unless the process that raised it reaches a flush point, or
  // `control` is killed, first (LRM 20.11).
  void KeepViolationReport(void* made, const AssertionControl& control);
  // LRM 16.4: a deferred immediate assertion's action is queued where the
  // statement is reached and acts later only if every source that could
  // withdraw it still stands -- the executing process's pass, the disable
  // targets that can name the assertion or flush its queue (16.4.2, 16.4.4),
  // and the assertion's own control, which a Kill withdraws it through (LRM
  // 20.11). An observed (`#0`) report matures in the Observed region and acts
  // in Reactive; a final one matures and acts in Postponed. Maturing discards
  // what it validated, which is why a matured report can no longer be
  // withdrawn (16.4.1). The action is a subroutine call, which can be a task
  // (16.4), so `body` is what it runs as, built where the statement is with
  // the arguments it was handed there. An observed one reached once the slot
  // is in its Postponed region has no Observed or Reactive region left to
  // mature and act in, and running its action in Postponed would run code that
  // may write what the region forbids writing (LRM 4.4.2.9), so it is the
  // design's error, at `at`.
  void SubmitDeferredObserved(
      Coroutine<void> body, const AssertionControl& control,
      base::SourceLocation at);
  void SubmitDeferredFinal(
      Coroutine<void> body, const AssertionControl& control);

  // LRM 20.11: every control the design registered, which a task selects
  // from.
  [[nodiscard]] auto AssertionControlsOfTheRun() -> AssertionControls&;
  // LRM 20.11: an assertion control task, its `control_type`,
  // `assertion_type`, `directive_type` and `levels` as the arguments evaluated
  // to, applied to what it names -- nothing, which is the whole design; an
  // instance or a generate block, `levels` from it down; or a block or a task,
  // an assertion's label among the blocks. A call naming several applies once
  // for each, which is the same as once for all, since applying any control
  // twice leaves a statement as applying it once does. A control type Table
  // 20-5 does not give selects nothing, and the call is ignored with a
  // warning placed at `at`, where the task was written, as a dump task's
  // argument it cannot use is.
  void AssertControlOfDesign(
      std::int64_t control_type, std::int64_t types, std::int64_t directives,
      std::int64_t levels, base::SourceLocation at);
  void AssertControlOfScope(
      Scope* scope, std::int64_t control_type, std::int64_t types,
      std::int64_t directives, std::int64_t levels, base::SourceLocation at);
  void AssertControlOfBlock(
      const CancellationTarget* block, std::int64_t control_type,
      std::int64_t types, std::int64_t directives, base::SourceLocation at);
  void AssertControlOfLoop(
      const LoopBlocks* blocks, std::int64_t control_type, std::int64_t types,
      std::int64_t directives, std::int64_t levels, base::SourceLocation at);

  // LRM 16.14.5: a concurrent assertion's attempts outlive the tick that
  // started them, so an attempt can still be in flight when the run ends. What
  // it comes to then is what the finished trace says of it rather than anything
  // a tick settles (Annex F.5.3.1), and the run's own tail is what supplies it
  // -- ahead of the design's `final` procedures, which read what the statements
  // it runs wrote.
  void RegisterConcurrentAssertion(EvaluationAttempts& attempts);

  // LRM 20.2: a simulation control task ends the run, and its finish number
  // selects what the tool prints about it (Table 20-1). `task` is the one the
  // design called, which is the whole of what a run nothing can resume has to
  // tell `$stop` and `$finish` apart by.
  void EndRun(std::string_view task, std::int64_t finish_number);
  // Adopts `coroutine` as a spawned child of the executing process's
  // lineage (LRM 9.5), schedules it, and answers the process it now is.
  auto Spawn(Coroutine<void> coroutine) -> std::shared_ptr<RuntimeProcess>;

  // Takes `coroutine` on as an execution of no lineage and runs it up to its
  // first wait: a deferred effect the standard makes no process of, so nothing
  // that names processes finds it (LRM 9.4.5 and 15.5.1 against 9.6.1, 9.6.3),
  // whose control the statement reaching it executes.
  void RunDetached(Coroutine<void> coroutine);

  // The same for an execution that observes what it waits for: each time it
  // is woken it runs inside the occurrence that woke it, as though the
  // execution that made it occur had called it -- a covergroup sampled on its
  // clocking event (LRM 19.3).
  void RunObserving(Coroutine<void> coroutine);

  // Holds `closure` until the simulation has ended, and runs it after the final
  // procedures (LRM 9.2.3): what writes the coverage database (LRM 19.9).
  void SubmitAtRunEnd(OwnedClosure<support::EffectSignature> closure);

  // The process whose body is executing right now (LRM 9.5); the observation
  // target `wait fork` reads and a fork spawn parents its branch to. Throws
  // if no process is executing.
  [[nodiscard]] auto CurrentProcess() -> RuntimeProcess&;
  [[nodiscard]] auto HasCurrentProcess() const -> bool;
  // Nullable form of `CurrentProcess()`: returns the executing process, or
  // `nullptr` when no process is running -- a scope-agnostic foreign query
  // (e.g. `svGetScope` outside an imported subroutine, LRM 35.5.3) uses this
  // to report a null scope rather than fault.
  [[nodiscard]] auto TryCurrentProcess() -> RuntimeProcess*;

  // What is running: the generator a randomization system call draws from (LRM
  // 18.13, 18.14) and the scope chain a DPI-C foreign call reports through (LRM
  // 35.5.3). A process installs one, and so does code that runs on no thread:
  // a static initialization, and a deferred body that evaluates what the
  // source wrote, which enters the context it took where it was made. A call
  // reaching this with neither running is a defect of the tool.
  [[nodiscard]] auto Running() -> RunningState&;
  // Nullable form of `Running()`, for a query that can arrive from foreign code
  // while nothing of the design is executing -- `svGetScope` outside an
  // imported subroutine (LRM 35.5.3) reports a null scope rather than faulting.
  [[nodiscard]] auto TryRunning() -> RunningState*;

  // The seed every `$random` called with no seed advances (LRM 20.14.1).
  [[nodiscard]] auto UnseededRandomSeed() -> std::int32_t&;

  // A fatal report Lyra writes for the design, placed where the operation being
  // carried out was written, followed by the end of the run (LRM 20.10). It is
  // the composition a `$fatal` makes, minus the departure -- for a condition
  // found where nothing may leave by unwinding, which is any frame reached from
  // foreign code. The second is that of a condition of the run's own.
  void ReportDesignFailure(const base::DiagMessage& message);
  void ReportDesignFailure(
      base::NoSourceLocation no_location, const base::DiagMessage& message);

  [[nodiscard]] auto Now() const -> SimTime;
  [[nodiscard]] auto GlobalPrecisionPower() const -> std::int8_t;

  // LRM 20.4.3: the design-wide `$timeformat` state.
  [[nodiscard]] auto TimeFormat() const -> const value::TimeFormat&;
  void SetTimeFormat(
      std::int64_t units_power, std::int64_t precision,
      const value::String& suffix, std::int64_t min_width);
  void ResetTimeFormat();

  RuntimeEffects(const RuntimeEffects&) = delete;
  auto operator=(const RuntimeEffects&) -> RuntimeEffects& = delete;
  RuntimeEffects(RuntimeEffects&&) = delete;
  auto operator=(RuntimeEffects&&) -> RuntimeEffects& = delete;

 protected:
  RuntimeEffects() = default;
  ~RuntimeEffects() = default;

 private:
  void StartDetached(
      Coroutine<void> coroutine, const ExecutionContext& context);
  // Runs what occurrences woke to run at them, once no occurrence is still
  // being told to the waits it reaches.
  void RunWokenAtTheOccurrence();
};

// Reports a run-time error that left a body or a time-zero phase, and asks the
// run to end. A design's error is the fatal report LRM 20.10 asks a tool to
// make for it and ends the simulation as `$fatal` does, so every `final`
// procedure still runs (LRM 9.2.3); a failure of the tool itself is not a
// language event and ends the run without them.
void ReportRaisedError(
    RuntimeEffects& effects, const std::exception_ptr& raised);

// LRM 13.4.4: a function may schedule what cannot become active until it has
// returned -- a nonblocking assignment or event trigger, a fork-join_none --
// only where the thread calling it was created by an initial or always
// procedure, or a fork of one, and it is the design's error otherwise. Asked
// where such a statement runs during a function call, which is known where the
// statement is written; the running thread is what only the run knows.
void RequireCallerMayScheduleAfterReturn(
    RuntimeEffects& effects, base::SourceLocation at);

// What work handed over to run where no thread runs it takes where it is made:
// the context an execution of no lineage started here would run in, and a
// generator what is running hands over to it (LRM 18.14). A deferred body that
// evaluates what the source wrote -- a `$strobe`'s arguments (LRM 21.2.2), a
// concurrent assertion's tick (LRM 16.5), a clocking block's sample (LRM
// 14.13) and the settling of its output at the end of a pass (LRM 14.16.2), a
// covergroup sampled under `strobe` (LRM 19.7), what starts a concurrent
// assertion's action (LRM 16.14.1) -- runs in it on no thread once its region
// runs it, and a deferred assertion's action is started from it as an
// execution of no lineage (LRM 16.4).
[[nodiscard]] auto TakeNoThreadContext(RuntimeEffects& effects)
    -> NoThreadContext;

// The `RuntimeEffects` view of the Runtime attached to this thread. Reached
// without a receiver, so every body kind -- module process, class method,
// package function, class static method -- takes one uniform runtime-access
// path.
//
// Lifetime is Runtime attachment: valid for the full lifetime of the owning
// Runtime on this thread, including elaboration phases, scheduler-dispatch
// windows, and any span in which the Runtime exists but no generated code is
// currently executing. Throws when read on a thread with no attached Runtime
// so a stray access surfaces as a located error rather than reading a stale
// handle.
[[nodiscard]] auto current_runtime() -> RuntimeEffects&;

// The procedure an effect made now is made for: the one running on the runtime
// attached to this thread, or none with no runtime attached.
[[nodiscard]] auto ProcedureMakingEffects() -> const ProcedureContext&;

// An object that waits reached through a weak reference has expired (LRM
// 8.30.1): the waits on `expiry` move to the runtime attached to this thread,
// which tells them once the execution running now has handed control back, so
// nothing of the design runs while the object is being destroyed. With no
// runtime attached nothing waits, and the list is left to end with its object.
void HandOverExpiry(IntrusiveList<WaitMembership>& expiry) noexcept;

// RAII publication of a `RuntimeEffects` view as the current thread's ambient
// runtime. Save-and-restore of any previously-published handle, so a nested
// construction (a test spawning a second Runtime while the first is still
// torn down) leaves the outer handle in place until the outer's own guard
// destructs.
class CurrentRuntimeGuard {
 public:
  explicit CurrentRuntimeGuard(RuntimeEffects& effects);
  ~CurrentRuntimeGuard();

  CurrentRuntimeGuard(const CurrentRuntimeGuard&) = delete;
  auto operator=(const CurrentRuntimeGuard&) -> CurrentRuntimeGuard& = delete;
  CurrentRuntimeGuard(CurrentRuntimeGuard&&) = delete;
  auto operator=(CurrentRuntimeGuard&&) -> CurrentRuntimeGuard& = delete;

 private:
  RuntimeEffects* previous_;
};

// Publication of `process` as this runtime's ambient execution identity for
// the extent of a resume (LRM 9.5), and of its generator as what a
// randomization call made in that extent draws from (LRM 18.14.2).
// Save-and-restore, so a nested foreign call that re-enters generated code
// cannot lose either on the way back.
class ProcessExecutionGuard {
 public:
  ProcessExecutionGuard(RuntimeEffects& effects, RuntimeProcess& process);
  ~ProcessExecutionGuard();

  ProcessExecutionGuard(const ProcessExecutionGuard&) = delete;
  auto operator=(const ProcessExecutionGuard&)
      -> ProcessExecutionGuard& = delete;
  ProcessExecutionGuard(ProcessExecutionGuard&&) = delete;
  auto operator=(ProcessExecutionGuard&&) -> ProcessExecutionGuard& = delete;

 private:
  RuntimeEffects* effects_;
  RuntimeProcess* previous_process_;
  RunningState* previous_running_;
};

}  // namespace lyra::runtime
