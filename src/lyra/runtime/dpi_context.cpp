#include "lyra/runtime/dpi_context.hpp"

#include <cstdint>
#include <variant>

#include "lyra/abi/records.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/dpi_scope_handles.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/sim_time.hpp"

namespace lyra::runtime {

void EnterDpiScope(RuntimeEffects& effects, Scope* decl_scope) {
  effects.Running().import_calls.Enter(decl_scope);
}

void LeaveDpiScope(RuntimeEffects& effects) {
  // The extent is left on every way out of it, and one of those ways runs with
  // nothing of the design executing: the frame of an execution still suspended
  // when the run ends is released where it stands, which runs the cleanups it
  // was holding open. The chain that entry belonged to is going with it.
  if (RunningState* running = effects.TryRunning(); running != nullptr) {
    running->import_calls.Leave();
  }
}

auto CurrentDpiScope() -> CurrentDpiHandle {
  RunningState* running = AmbientRunContext::Current().Effects().TryRunning();
  if (running == nullptr) {
    return static_cast<Scope*>(nullptr);
  }
  return running->import_calls.Current();
}

auto ReplaceDpiScope(CurrentDpiHandle handle) -> CurrentDpiHandle {
  RunningState* running = AmbientRunContext::Current().Effects().TryRunning();
  if (running == nullptr) {
    return static_cast<Scope*>(nullptr);
  }
  return running->import_calls.Replace(handle);
}

auto HandStringToForeignCaller(
    RuntimeEffects& effects, const value::String& text) -> const char* {
  return effects.Running().import_calls.HandToForeignCaller(text);
}

namespace {

// Whether this execution is in the disabled state of LRM 35.9. Kept apart from
// the int the boundary answers with, because what the checks need is the
// predicate and what crosses the boundary is the standard's own encoding of it.
auto IsDisabled(RuntimeEffects& effects) -> bool {
  const RuntimeProcess* process = effects.TryCurrentProcess();
  return process != nullptr && process->DepartureIsDue();
}

}  // namespace

auto DisableIsActive(RuntimeEffects& effects) -> std::int32_t {
  return IsDisabled(effects) ? 1 : 0;
}

void AcknowledgeStop(RuntimeEffects& effects) {
  if (RunningState* running = effects.TryRunning(); running != nullptr) {
    running->import_calls.AcknowledgeStop();
  }
}

void CheckImportTaskAcknowledged(
    RuntimeEffects& effects, std::int32_t returned) {
  if (!IsDisabled(effects) || returned == 1) {
    return;
  }
  // LRM 35.9 item b.
  effects.ReportDesignFailure(
      base::DiagMessage(
          "an imported task returned while its execution thread was in the "
          "disabled state without returning 1"));
}

void CheckImportFunctionAcknowledged(RuntimeEffects& effects) {
  if (!IsDisabled(effects)) {
    return;
  }
  RunningState* running = effects.TryRunning();
  if (running != nullptr && running->import_calls.InnermostAcknowledgedStop()) {
    return;
  }
  // LRM 35.9 item c.
  effects.ReportDesignFailure(
      base::DiagMessage(
          "an imported function returned while its execution thread was in "
          "the disabled state without calling 'svAckDisabledState'"));
}

void CheckExportReachable(RuntimeEffects& effects) {
  if (!IsDisabled(effects)) {
    RequireAContextImportCall(effects);
    return;
  }
  // LRM 35.9 item d.
  effects.ReportDesignFailure(
      base::DiagMessage(
          "foreign code called an exported subroutine after its execution "
          "thread entered the disabled state"));
}

void RequireAContextImportCall(RuntimeEffects& effects) {
  if (const RunningState* running = effects.TryRunning();
      running != nullptr && running->import_calls.InsideACall()) {
    return;
  }
  throw SimulationError(
      "foreign code called an exported subroutine from an imported "
      "subroutine that is not declared 'context'");
}

}  // namespace lyra::runtime

namespace {

// The svdpi time value (Annex H `svTimeVal` == `s_vpi_time`), laid out here so
// the runtime need not include the vendored header. `svGetTime` fills the
// integer simulation-time form.
struct SvTimeVal {
  std::int32_t type;
  std::uint32_t high;
  std::uint32_t low;
  double real;
};
constexpr std::int32_t kVpiSimTime = 2;

// The scope handles this run has given foreign code, which answer the name and
// user-data queries.
auto Handles() -> lyra::runtime::DpiScopeHandles& {
  return lyra::runtime::AmbientRunContext::Current().ScopeHandles();
}

// What foreign code is given for the handle an import call has current: a
// scope, which it may pass back from then on, or the address it set, which
// stays no scope.
auto HandOut(const lyra::runtime::CurrentDpiHandle& handle) -> void* {
  return std::visit(
      lyra::Overloaded{
          [](lyra::runtime::Scope* scope) -> void* {
            return Handles().Issue(scope);
          },
          [](const lyra::runtime::UnissuedHandle& unissued) {
            return unissued.address;
          }},
      handle);
}

// The run a time query reads its clock and its precision from.
auto Effects() -> lyra::runtime::RuntimeEffects& {
  return lyra::runtime::AmbientRunContext::Current().Effects();
}

// Resolves a time-query scope handle: a null handle is legal (the query is at
// the simulation level), a non-null one must have been handed out. Returns
// false for any other, which the query reports as an error.
auto ResolveTimeScope(void* scope, const lyra::runtime::Scope** resolved)
    -> bool {
  if (scope == nullptr) {
    *resolved = nullptr;
    return true;
  }
  const auto* handle = static_cast<const lyra::runtime::Scope*>(scope);
  if (!Handles().IsIssued(handle)) {
    return false;
  }
  *resolved = handle;
  return true;
}

struct TimePowers {
  std::int8_t unit;
  std::int8_t precision;
};

// The powers of ten a time query answers in. A scope carrying no timescale of
// its own reports the unspecified sentinel rather than a power (LRM 3.14.2.3),
// and a query naming no scope is at the simulation level; both are the same
// answer, which is the simulation's own precision.
auto EffectiveTimePowers(
    const lyra::runtime::Scope* scope, std::int8_t global_power) -> TimePowers {
  const auto effective = [global_power](std::int8_t declared) {
    return declared == lyra::abi::kUnspecifiedTimePower ? global_power
                                                        : declared;
  };
  if (scope == nullptr) {
    return TimePowers{.unit = global_power, .precision = global_power};
  }
  return TimePowers{
      .unit = effective(scope->TimeUnitPower()),
      .precision = effective(scope->TimePrecisionPower())};
}

}  // namespace

// The Annex H context, time and disable surface, linked into the simulation
// binary and resolved against the user's C by name. `svScope` is `void*`; a
// handle is a `runtime::Scope*` the C layer handed out. The current scope is
// the innermost entry of the running import call chain; the handles handed out
// answer the name and user-data queries; a time query reports the scope's
// effective unit or precision, or the simulation-level value for a null scope;
// and the two disable entries are how foreign code reads and answers the
// protocol of LRM 35.9. Every entry that answers a scope hands it out, which is
// what makes it one foreign code may pass back. Errors follow the svdpi
// contract -- a null handle, null out slot, or invalid handle yields the
// documented null / -1 -- and nothing here throws across the C boundary.
extern "C" {

auto svGetScope() -> void* {
  return HandOut(lyra::runtime::CurrentDpiScope());
}

// Whether the address is a scope is decided here, where it arrives, because
// only a scope this run handed out is one (LRM Annex H.9.3).
auto svSetScope(void* scope) -> void* {
  auto* given = static_cast<lyra::runtime::Scope*>(scope);
  const lyra::runtime::CurrentDpiHandle installed =
      given == nullptr || Handles().IsIssued(given)
          ? lyra::runtime::CurrentDpiHandle{given}
          : lyra::runtime::CurrentDpiHandle{
                lyra::runtime::UnissuedHandle{.address = scope}};
  return HandOut(lyra::runtime::ReplaceDpiScope(installed));
}

auto svGetNameFromScope(void* scope) -> const char* {
  return Handles().NameOf(static_cast<const lyra::runtime::Scope*>(scope));
}

auto svGetScopeFromName(const char* name) -> void* {
  return name == nullptr ? nullptr
                         : Handles().Issue(Handles().ScopeOfName(name));
}

auto svPutUserData(void* scope, void* key, void* data) -> int {
  return Handles().PutUserData(
      static_cast<const lyra::runtime::Scope*>(scope), key, data);
}

auto svGetUserData(void* scope, void* key) -> void* {
  return Handles().GetUserData(
      static_cast<const lyra::runtime::Scope*>(scope), key);
}

auto svGetTime(void* scope, void* time) -> int {
  if (time == nullptr) {
    return -1;
  }
  const lyra::runtime::Scope* resolved = nullptr;
  if (!ResolveTimeScope(scope, &resolved)) {
    return -1;
  }
  const std::int8_t global = Effects().GlobalPrecisionPower();
  const lyra::SimDuration divisor = lyra::runtime::TimeUnitDivisor(
      EffectiveTimePowers(resolved, global).unit, global);
  const std::uint64_t scaled = Effects().Now() / divisor;
  auto* out = static_cast<SvTimeVal*>(time);
  out->type = kVpiSimTime;
  out->high = static_cast<std::uint32_t>(scaled >> 32U);
  out->low = static_cast<std::uint32_t>(scaled & 0xFFFFFFFFU);
  out->real = 0.0;
  return 0;
}

auto svGetTimeUnit(void* scope, void* time_unit) -> int {
  if (time_unit == nullptr) {
    return -1;
  }
  const lyra::runtime::Scope* resolved = nullptr;
  if (!ResolveTimeScope(scope, &resolved)) {
    return -1;
  }
  const std::int8_t unit_power =
      EffectiveTimePowers(resolved, Effects().GlobalPrecisionPower()).unit;
  // A time power is a small signed number (LRM 3.14): -9 is ns, so widening it
  // with sign extension is the meaning, not the byte read this check looks for.
  // NOLINTNEXTLINE(bugprone-signed-char-misuse)
  *static_cast<std::int32_t*>(time_unit) = unit_power;
  return 0;
}

auto svGetTimePrecision(void* scope, void* time_precision) -> int {
  if (time_precision == nullptr) {
    return -1;
  }
  const lyra::runtime::Scope* resolved = nullptr;
  if (!ResolveTimeScope(scope, &resolved)) {
    return -1;
  }
  const std::int8_t precision =
      EffectiveTimePowers(resolved, Effects().GlobalPrecisionPower()).precision;
  // A time power is a small signed number (LRM 3.14): -9 is ns, so widening it
  // with sign extension is the meaning, not the byte read this check looks for.
  // NOLINTNEXTLINE(bugprone-signed-char-misuse)
  *static_cast<std::int32_t*>(time_precision) = precision;
  return 0;
}

auto svIsDisabledState() -> int {
  return lyra::runtime::DisableIsActive(Effects());
}

void svAckDisabledState() {
  lyra::runtime::AcknowledgeStop(Effects());
}

}  // extern "C"
