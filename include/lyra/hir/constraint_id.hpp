#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

// Identity of one constraint (LRM 18.5) in the arena of the body that wrote it.
struct ConstraintId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ConstraintId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::hir
