#include "lyra/runtime/hierarchy_levels.hpp"

#include <algorithm>
#include <cstdint>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/support/scope_kind.hpp"

namespace lyra::runtime {

auto LevelsNamed(std::int64_t levels) -> std::int64_t {
  return levels == 0 ? kEveryLevel : std::max<std::int64_t>(levels, 0);
}

auto CountsAsLevel(const Scope& scope) -> bool {
  switch (scope.Segment().Declaration().Kind()) {
    case support::ElaboratedScopeKind::kModule:
    case support::ElaboratedScopeKind::kInterface:
    case support::ElaboratedScopeKind::kProgram:
    case support::ElaboratedScopeKind::kModuleArray:
    case support::ElaboratedScopeKind::kInterfaceArray:
    case support::ElaboratedScopeKind::kProgramArray:
      return true;
    case support::ElaboratedScopeKind::kGenerateBlock:
    case support::ElaboratedScopeKind::kGenerateLoop:
    case support::ElaboratedScopeKind::kDesignRoot:
      return false;
  }
  throw InternalError("CountsAsLevel: unknown kind");
}

auto LevelsBelow(const Scope& child, std::int64_t remaining) -> std::int64_t {
  if (!CountsAsLevel(child) || remaining == kEveryLevel) return remaining;
  return std::max<std::int64_t>(remaining - 1, 0);
}

}  // namespace lyra::runtime
