#pragma once

#include <cstdint>
#include <vector>

#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct DeferredCheckSite {};

struct CompilationUnit {
  std::vector<Type> types;
  StructuralScope structural_scope;
  std::vector<DeferredCheckSite> deferred_check_sites;

  [[nodiscard]] auto GetType(TypeId id) const -> const Type& {
    return types.at(id.value);
  }

  // Backing-vector position is the id, matching TypeId / ProceduralVarId.
  auto AllocateDeferredCheckSiteId() -> DeferredCheckSiteId {
    const auto id = static_cast<std::uint32_t>(deferred_check_sites.size());
    deferred_check_sites.push_back({});
    return DeferredCheckSiteId{id};
  }
};

}  // namespace lyra::mir
