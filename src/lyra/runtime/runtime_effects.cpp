#include "lyra/runtime/runtime_effects.hpp"

#include <cstdint>
#include <functional>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/delay.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/var.hpp"
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
    // initializer (LRM 6.8) -- belongs to no process, so LRM 12.4.2.1 has no
    // violation report queue for a flush point to clear and it always matures.
    Submit(Now(), Region::kObserved, std::move(report));
    return;
  }
  Submit(
      Now(), Region::kObserved,
      [epoch = std::weak_ptr(process->CurrentViolationReportEpoch()),
       report = std::move(report)] {
        // LRM 12.4.2.1: an expired epoch is a flush point the process reached
        // before this report could mature.
        if (!epoch.expired()) {
          report();
        }
      });
}

void RuntimeEffects::TriggerValueChange(
    Observable& observable, const EdgeClassifier& classify) {
  for (CoroutineHandle handle : observable.TakeMatchingWaiters(classify)) {
    Wake(handle);
  }
}

void RuntimeEffects::RequestFinish(
    int,  // NOLINT(readability-named-parameter)
    bool fatal) {
  Runtime& rt = AsRuntime(*this);
  rt.finished_ = true;
  if (fatal) rt.fatal_finish_ = true;
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
