#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// A block's position in its enclosing block's child-scope arena. It sits in a
// header of its own because both a statement and an expression can name a
// block, and the two live in headers that cannot include each other.
struct BlockId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const BlockId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
