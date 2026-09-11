#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

struct PropertyCoordinateId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PropertyCoordinateId&) const
      -> std::strong_ordering = default;
};

struct BehaviorCoordinateId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const BehaviorCoordinateId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::hir
