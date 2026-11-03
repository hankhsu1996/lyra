#include "lyra/runtime/hierarchy_levels.hpp"

#include <algorithm>
#include <cstdint>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/support/dump_kind.hpp"

namespace lyra::runtime {

auto LevelsNamed(std::int64_t levels) -> std::int64_t {
  return levels == 0 ? kEveryLevel : std::max<std::int64_t>(levels, 0);
}

auto CountsAsLevel(const Scope& scope) -> bool {
  switch (scope.Info().metadata.DumpedAs()) {
    case support::DumpScopeKind::kModule:
      return true;
    case support::DumpScopeKind::kTask:
    case support::DumpScopeKind::kFunction:
    case support::DumpScopeKind::kBegin:
    case support::DumpScopeKind::kFork:
      return false;
  }
  throw InternalError("CountsAsLevel: unknown scope kind");
}

auto LevelsBelow(const Scope& child, std::int64_t remaining) -> std::int64_t {
  if (!CountsAsLevel(child) || remaining == kEveryLevel) return remaining;
  return std::max<std::int64_t>(remaining - 1, 0);
}

}  // namespace lyra::runtime
