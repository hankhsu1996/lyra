#include "lyra/runtime/dpi_context.hpp"

#include <cstdint>

#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/dpi_scope_registry.hpp"
#include "lyra/runtime/running_state.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/sim_time.hpp"

namespace lyra::runtime {

void EnterDpiScope(RuntimeEffects& effects, Scope* decl_scope) {
  effects.Running().dpi_scopes.Enter(decl_scope);
}

void LeaveDpiScope(RuntimeEffects& effects) {
  effects.Running().dpi_scopes.Leave();
}

auto CurrentDpiScope() -> Scope* {
  RunningState* running = AmbientRunContext::Current().Effects().TryRunning();
  return running == nullptr ? nullptr : running->dpi_scopes.Current();
}

auto ReplaceDpiScope(Scope* scope) -> Scope* {
  RunningState* running = AmbientRunContext::Current().Effects().TryRunning();
  return running == nullptr ? nullptr : running->dpi_scopes.Replace(scope);
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

// The scope directory answering the name and user-data queries.
auto Directory() -> lyra::runtime::DpiScopeRegistry& {
  return lyra::runtime::AmbientRunContext::Current().ScopeRegistry();
}

// The run a time query reads its clock and its precision from.
auto Effects() -> lyra::runtime::RuntimeEffects& {
  return lyra::runtime::AmbientRunContext::Current().Effects();
}

// Resolves a time-query scope handle: a null handle is legal (the query is at
// the simulation level), a non-null handle must be registered. Returns false
// for an unregistered non-null handle, which the query reports as an error.
auto ResolveTimeScope(void* scope, const lyra::runtime::Scope** resolved)
    -> bool {
  if (scope == nullptr) {
    *resolved = nullptr;
    return true;
  }
  const auto* handle = static_cast<const lyra::runtime::Scope*>(scope);
  if (!Directory().IsValidHandle(handle)) {
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
    return declared == lyra::runtime::kUnspecifiedTimePower ? global_power
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

// The Annex H context and time surface, linked into the simulation binary and
// resolved against the user's C by name. `svScope` is `void*`; a handle is a
// `runtime::Scope*`. The current scope is the top of the running DPI scope
// chain; the directory answers the name and user-data queries; a time query
// reports the scope's effective unit or precision, or the simulation-level
// value for a null scope. Errors follow the svdpi contract -- a null handle,
// null out slot, or invalid handle yields the documented null / -1 -- and never
// throw across the C boundary.
extern "C" {

auto svGetScope() -> void* {
  return lyra::runtime::CurrentDpiScope();
}

auto svSetScope(void* scope) -> void* {
  return lyra::runtime::ReplaceDpiScope(
      static_cast<lyra::runtime::Scope*>(scope));
}

auto svGetNameFromScope(void* scope) -> const char* {
  return Directory().NameOf(static_cast<const lyra::runtime::Scope*>(scope));
}

auto svGetScopeFromName(const char* name) -> void* {
  return name == nullptr ? nullptr : Directory().ScopeOfName(name);
}

auto svPutUserData(void* scope, void* key, void* data) -> int {
  return Directory().PutUserData(
      static_cast<const lyra::runtime::Scope*>(scope), key, data);
}

auto svGetUserData(void* scope, void* key) -> void* {
  return Directory().GetUserData(
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

}  // extern "C"
