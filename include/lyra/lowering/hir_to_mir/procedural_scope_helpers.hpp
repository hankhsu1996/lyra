#pragma once

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

inline auto AddChildProceduralScope(
    std::vector<mir::ProceduralScope>& scopes, mir::ProceduralScope scope)
    -> mir::ProceduralScopeId {
  const mir::ProceduralScopeId id{static_cast<std::uint32_t>(scopes.size())};
  scopes.push_back(std::move(scope));
  return id;
}

}  // namespace lyra::lowering::hir_to_mir
