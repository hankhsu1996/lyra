#include "lyra/runtime/runtime_effects.hpp"

#include <cstdint>
#include <exception>
#include <format>
#include <functional>
#include <new>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/delay.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/value/format.hpp"

namespace lyra::runtime {

namespace {

// Recovers the concrete Runtime from its capability view. Safe because
// Runtime is the sole derived class (declared `final`), so every
// `RuntimeEffects` object is a `Runtime` object.
auto AsRuntime(RuntimeEffects& effects) -> Runtime& {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  return static_cast<Runtime&>(effects);
}

auto AsRuntime(const RuntimeEffects& effects) -> const Runtime& {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-static-cast-downcast)
  return static_cast<const Runtime&>(effects);
}

auto CurrentRuntimeSlot() -> RuntimeEffects*& {
  thread_local RuntimeEffects* slot = nullptr;
  return slot;
}

// What a violation report records where it is created: the executing process's
// pass and nothing more, which is the whole of the flush-point set LRM 12.4.2.1
// lists.
auto ViolationReportValidity(RuntimeProcess& process)
    -> DeferredReportValidity {
  return DeferredReportValidity{
      .pass = std::weak_ptr(process.CurrentDeferredReportEpoch()),
      .targets = {}};
}

// What a deferred assertion report records: the same pass, and in addition the
// disable targets that can withdraw it on their own (LRM 16.4.2, 16.4.4). The
// two constructs differ in what they record and in nothing else; both act only
// if everything they recorded still stands.
auto AssertionReportValidity(RuntimeProcess& process)
    -> DeferredReportValidity {
  return DeferredReportValidity{
      .pass = std::weak_ptr(process.CurrentDeferredReportEpoch()),
      .targets = process.DeferredReportTargets()};
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
          std::exchange(AsRuntime(effects).current_process_, &process)) {
}

ProcessExecutionGuard::~ProcessExecutionGuard() {
  AsRuntime(*effects_).current_process_ = previous_process_;
}

auto RuntimeEffects::Stream() -> StreamDispatcher& {
  return AsRuntime(*this).stream_;
}

auto RuntimeEffects::Diagnostic() -> DiagnosticDispatcher& {
  return AsRuntime(*this).diagnostic_;
}

auto RuntimeEffects::Files() -> FileTable& {
  return AsRuntime(*this).files_;
}

auto RuntimeEffects::PlusArgs() -> PlusArgsSource& {
  return AsRuntime(*this).plusargs_;
}

void RuntimeEffects::RecordCoverage(const value::String& site, bool succeeded) {
  AsRuntime(*this).coverage_.Record(site.View(), succeeded);
}

void RuntimeEffects::Schedule(
    SimTime when, Region region, CoroutineHandle activation) {
  activation->Park(AsRuntime(*this).SlotAt(when)[region].activations);
}

void RuntimeEffects::Wake(CoroutineHandle activation) {
  ConsumeWait(activation);
  Schedule(Now(), Region::kActive, activation);
}

void RuntimeEffects::Submit(
    SimTime when, Region region, std::function<void()> effect) {
  AsRuntime(*this).SlotAt(when)[region].effects.push_back(std::move(effect));
}

void RuntimeEffects::SubmitNba(std::function<void()> closure) {
  Submit(Now(), Region::kNba, std::move(closure));
}

void RuntimeEffects::SubmitNbaAfter(
    const value::PackedArray& duration, const value::PackedArray& unit_power,
    const value::PackedArray& precision_power, std::function<void()> closure) {
  const auto unit = static_cast<std::int8_t>(unit_power.ToInt64());
  const auto precision = static_cast<std::int8_t>(precision_power.ToInt64());
  Submit(
      DelayDeadline(*this, DelayTicks(duration, unit, precision), precision),
      Region::kNba, std::move(closure));
}

void RuntimeEffects::SubmitNbaAfterReal(
    const value::Real& duration, const value::PackedArray& unit_power,
    const value::PackedArray& precision_power, std::function<void()> closure) {
  const auto unit = static_cast<std::int8_t>(unit_power.ToInt64());
  const auto precision = static_cast<std::int8_t>(precision_power.ToInt64());
  Submit(
      DelayDeadline(
          *this, DelayTicksReal(duration, unit, precision), precision),
      Region::kNba, std::move(closure));
}

void RuntimeEffects::SubmitPostponed(std::function<void()> closure) {
  Submit(Now(), Region::kPostponed, std::move(closure));
}

void RuntimeEffects::SubmitObserved(std::function<void()> report) {
  RuntimeProcess* process = AsRuntime(*this).current_process_;
  if (process == nullptr) {
    // A check that fires before any procedure runs -- a static variable's
    // initializer (LRM 6.8) -- belongs to no process, so there is no deferred
    // report queue for a flush point to clear and it always matures (LRM
    // 12.4.2.1).
    Submit(Now(), Region::kObserved, std::move(report));
    return;
  }
  Submit(
      Now(), Region::kObserved,
      [validity = ViolationReportValidity(*process),
       report = std::move(report)] {
        // LRM 12.4.2.1: a pass that no longer stands is a flush point the
        // process reached before this report could mature.
        if (validity.Holds()) {
          report();
        }
      });
}

void RuntimeEffects::SubmitDeferredObserved(std::function<void()> action) {
  RuntimeProcess* process = AsRuntime(*this).current_process_;
  if (process == nullptr) {
    // A deferred assertion outside any process (a static variable's
    // initializer, LRM 6.8) belongs to no deferred report queue, so no flush
    // point can clear it and it always matures.
    Submit(Now(), Region::kReactive, std::move(action));
    return;
  }
  // LRM 16.4.1: the report is queued now and matures in Observed only if every
  // source it recorded still stands, then runs its action in the Reactive
  // region. Maturing discards the record, so the committed Reactive effect is
  // beyond the reach of any later flush or `disable`.
  Submit(
      Now(), Region::kObserved,
      [this, validity = AssertionReportValidity(*process),
       action = std::move(action)]() mutable {
        if (!validity.Holds()) return;
        Submit(Now(), Region::kReactive, std::move(action));
      });
}

void RuntimeEffects::SubmitDeferredFinal(std::function<void()> action) {
  RuntimeProcess* process = AsRuntime(*this).current_process_;
  if (process == nullptr) {
    Submit(Now(), Region::kPostponed, std::move(action));
    return;
  }
  // LRM 16.4.1: a final deferred assertion matures and runs in the Postponed
  // region, still withdrawn by anything that invalidated a source it recorded
  // before then. Postponed is non-iterative, so maturing and running are the
  // one region.
  Submit(
      Now(), Region::kPostponed,
      [validity = AssertionReportValidity(*process),
       action = std::move(action)] {
        if (validity.Holds()) action();
      });
}

void RuntimeEffects::WakeWaitersOf(
    Observable& observable, const ProjectionUnchanged& unchanged) {
  for (CoroutineHandle handle : observable.TakeFiringWaiters(unchanged)) {
    Wake(handle);
  }
}

void RuntimeEffects::EndRun(
    std::string_view task, const value::String& origin,
    const value::PackedArray& level) {
  Runtime& rt = AsRuntime(*this);
  rt.ReportSimulationControl(
      task, origin.View(), static_cast<int>(level.ToInt64()));
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
    rt.ReportDesignError(error.what());
  } catch (const InternalError& error) {
    rt.ReportToolFailure(std::format("internal error: {}", error.what()));
  } catch (const std::bad_alloc&) {
    rt.ReportToolFailure("out of memory");
  } catch (const std::exception& error) {
    rt.ReportToolFailure(std::format("unexpected error: {}", error.what()));
  }
}

void RuntimeEffects::Spawn(Coroutine<void> coroutine) {
  Runtime& rt = AsRuntime(*this);
  if (rt.current_process_ == nullptr) {
    throw InternalError(
        "RuntimeEffects::Spawn: no ambient process to parent the branch to");
  }
  RuntimeProcess& parent = *rt.current_process_;
  // Hierarchical seeding (LRM 18.14.1): the branch starts from the spawner's
  // next value, so a whole subtree of threads follows from the seed of the one
  // at its root and the order the branches then run in does not move any of it.
  auto child = std::make_shared<RuntimeProcess>(
      parent.OwningScope(), ProcessKind::kSpawned, std::move(coroutine),
      parent.Rng().NextSeed());
  const CoroutineHandle handle = child->TopHandle();
  // The spawned activity is enabled within whatever disable targets the spawner
  // is inside (LRM 9.6.2), so it takes that membership here rather than
  // rebuilding it once it starts running: it is spawned already enclosed, and a
  // `disable` landing before its first resumption still reaches it.
  child->InheritEnclosingTargets(parent);
  parent.AdoptChild(child);
  rt.RegisterProcessInRegistry(child);
  Schedule(Now(), Region::kActive, handle);
}

void RuntimeEffects::RunDetached(Coroutine<void> coroutine) {
  Runtime& rt = AsRuntime(*this);
  if (rt.current_process_ == nullptr) {
    throw InternalError(
        "RuntimeEffects::RunDetached: no ambient process to take the owning "
        "scope from");
  }
  // The carrier evaluates nothing of the design -- what it writes and where it
  // writes it were both settled where the statement was reached -- so it draws
  // no random values, and taking a seed from the process that reached the
  // statement would move that process's own stream (LRM 18.14.1).
  auto carrier = std::make_shared<RuntimeProcess>(
      rt.current_process_->OwningScope(), ProcessKind::kDetached,
      std::move(coroutine), RandomSeed{0});
  const CoroutineHandle handle = carrier->TopHandle();
  // No lineage and no disable membership: the standard makes no process of the
  // update this carries out, so nothing that names processes may find it. What
  // keeps it alive is therefore the runtime's own registry, which every
  // execution created during simulation is held by.
  rt.RegisterProcessInRegistry(std::move(carrier));
  Schedule(Now(), Region::kActive, handle);
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
    const value::PackedArray& units_power, const value::PackedArray& precision,
    const value::String& suffix, const value::PackedArray& min_width) {
  AsRuntime(*this).time_format_ = value::TimeFormat{
      .units_power = static_cast<std::int8_t>(units_power.ToInt64()),
      .precision = static_cast<std::int32_t>(precision.ToInt64()),
      .suffix = std::string(suffix.View()),
      .min_width = static_cast<std::int32_t>(min_width.ToInt64())};
}

void RuntimeEffects::ResetTimeFormat() {
  Runtime& rt = AsRuntime(*this);
  rt.time_format_ = value::TimeFormat{};
  rt.time_format_.units_power = rt.global_precision_power_;
}

}  // namespace lyra::runtime
