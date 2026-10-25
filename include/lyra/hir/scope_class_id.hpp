#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

// The class a scope of the design hierarchy is, under the identity the context
// holding it gives it: a unit's scope class registry, or a signature's list.
struct ScopeClassId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ScopeClassId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::hir
