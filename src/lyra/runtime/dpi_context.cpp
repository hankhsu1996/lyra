#include "lyra/runtime/dpi_context.hpp"

#include <cstdint>

#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/dpi_scope_registry.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/sim_time.hpp"

namespace lyra::runtime {

DpiScopeGuard::DpiScopeGuard(RuntimeServices& services, Scope* decl_scope)
    : process_(&services.CurrentProcess()) {
  process_->PushDpiScope(decl_scope);
}

DpiScopeGuard::~DpiScopeGuard() {
  process_->PopDpiScope();
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

// The process whose foreign call is running, or null when none is executing --
// a query from C that is not an imported function, which svGetScope reports as
// a null scope rather than a fault.
auto ForeignProcess() -> lyra::runtime::RuntimeProcess* {
  return lyra::runtime::AmbientRunContext::Current()
      .Services()
      .TryCurrentProcess();
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

}  // namespace

// The Annex H context and time surface, linked into the simulation binary and
// resolved against the user's C by name. `svScope` is `void*`; a handle is a
// `runtime::Scope*`. The current scope is the top of the executing process's
// DPI scope chain; the directory answers the name and user-data queries; a time
// query reports the scope's effective unit or precision, or the
// simulation-level value for a null scope. Errors follow the svdpi contract --
// a null handle, null out slot, or invalid handle yields the documented null /
// -1 -- and never throw across the C boundary.
extern "C" {

auto svGetScope() -> void* {
  lyra::runtime::RuntimeProcess* process = ForeignProcess();
  return process == nullptr ? nullptr : process->CurrentDpiScope();
}

auto svSetScope(void* scope) -> void* {
  lyra::runtime::RuntimeProcess* process = ForeignProcess();
  return process == nullptr ? nullptr
                            : process->ReplaceDpiScope(
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
  lyra::runtime::RuntimeServices& services =
      lyra::runtime::AmbientRunContext::Current().Services();
  const std::int8_t unit = resolved != nullptr
                               ? resolved->TimeUnitPower()
                               : services.GlobalPrecisionPower();
  const lyra::SimDuration divisor =
      lyra::runtime::TimeUnitDivisor(unit, services.GlobalPrecisionPower());
  const std::uint64_t scaled = services.Now() / divisor;
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
  lyra::runtime::RuntimeServices& services =
      lyra::runtime::AmbientRunContext::Current().Services();
  *static_cast<std::int32_t*>(time_unit) =
      resolved != nullptr ? resolved->TimeUnitPower()
                          : services.GlobalPrecisionPower();
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
  lyra::runtime::RuntimeServices& services =
      lyra::runtime::AmbientRunContext::Current().Services();
  *static_cast<std::int32_t*>(time_precision) =
      resolved != nullptr ? resolved->TimePrecisionPower()
                          : services.GlobalPrecisionPower();
  return 0;
}

}  // extern "C"
