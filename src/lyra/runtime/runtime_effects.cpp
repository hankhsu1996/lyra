#include "lyra/runtime/runtime_effects.hpp"

#include <cstddef>
#include <cstdint>
#include <functional>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/scope.hpp"
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
          std::exchange(AsRuntime(effects).current_process_, &process)),
      previous_scope_(
          std::exchange(
              AsRuntime(effects).current_scope_, process.OwningScope())) {
}

ProcessExecutionGuard::~ProcessExecutionGuard() {
  AsRuntime(*effects_).current_process_ = previous_process_;
  AsRuntime(*effects_).current_scope_ = previous_scope_;
}

ScopeExecutionGuard::ScopeExecutionGuard(RuntimeEffects& effects, Scope& scope)
    : effects_(&effects),
      previous_scope_(
          std::exchange(AsRuntime(effects).current_scope_, &scope)) {
}

ScopeExecutionGuard::~ScopeExecutionGuard() {
  AsRuntime(*effects_).current_scope_ = previous_scope_;
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

void RuntimeEffects::SubmitNba(std::function<void(RuntimeEffects&)> closure) {
  Runtime& rt = AsRuntime(*this);
  if (rt.phase_ == SchedulerPhase::kCommitNba) {
    throw InternalError(
        "RuntimeEffects::SubmitNba: re-entrant NBA submission during NBA "
        "region is not supported");
  }
  rt.queues_.nba.push_back(std::move(closure));
}

void RuntimeEffects::SubmitPostponed(std::function<void()> closure) {
  Runtime& rt = AsRuntime(*this);
  if (rt.phase_ == SchedulerPhase::kPostponed) {
    throw InternalError(
        "RuntimeEffects::SubmitPostponed: re-entrant postponed submission "
        "during postponed region is not supported");
  }
  rt.queues_.postponed.push_back(std::move(closure));
}

void RuntimeEffects::SubmitObserved(
    const lyra::value::PackedArray& site_id, std::function<void()> fn) {
  Runtime& rt = AsRuntime(*this);
  if (rt.current_scope_ == nullptr) {
    throw InternalError("RuntimeEffects::SubmitObserved: no ambient scope");
  }
  const auto slot = static_cast<std::size_t>(site_id.ToInt64());
  auto& queue = rt.queues_.observed[rt.current_scope_];
  if (slot >= queue.size()) {
    queue.resize(slot + 1);
  }
  queue[slot] = std::move(fn);
}

void RuntimeEffects::TriggerValueChange(
    Observable& observable, const EdgeClassifier& classify) {
  Runtime& rt = AsRuntime(*this);
  for (CoroutineHandle handle : observable.TakeMatchingWaiters(classify)) {
    rt.EnqueueNextDelta(handle);
  }
}

void RuntimeEffects::ScheduleNextDelta(CoroutineHandle handle) {
  AsRuntime(*this).EnqueueNextDelta(handle);
}

void RuntimeEffects::ScheduleInactive(CoroutineHandle handle) {
  Runtime& rt = AsRuntime(*this);
  handle->Park(rt.queues_.inactive);
}

void RuntimeEffects::ScheduleAtTime(SimTime when, CoroutineHandle handle) {
  Runtime& rt = AsRuntime(*this);
  handle->Park(rt.queues_.delayed[when]);
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
  auto child = std::make_shared<RuntimeProcess>(
      parent.OwningScope(), ProcessKind::kSpawned, std::move(coroutine));
  const CoroutineHandle handle = child->TopHandle();
  parent.AdoptChild(child);
  rt.RegisterProcessInRegistry(child);
  handle->Park(rt.queues_.active);
}

auto RuntimeEffects::CurrentProcess() -> RuntimeProcess& {
  RuntimeProcess* p = AsRuntime(*this).current_process_;
  if (p == nullptr) {
    throw InternalError(
        "RuntimeEffects::CurrentProcess: no process is currently executing");
  }
  return *p;
}

auto RuntimeEffects::CurrentScope() -> Scope& {
  Scope* s = AsRuntime(*this).current_scope_;
  if (s == nullptr) {
    throw InternalError(
        "RuntimeEffects::CurrentScope: no scope is currently executing");
  }
  return *s;
}

auto RuntimeEffects::HasCurrentProcess() const -> bool {
  return AsRuntime(*this).current_process_ != nullptr;
}

auto RuntimeEffects::TryCurrentProcess() -> RuntimeProcess* {
  return AsRuntime(*this).current_process_;
}

auto RuntimeEffects::HasCurrentScope() const -> bool {
  return AsRuntime(*this).current_scope_ != nullptr;
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
