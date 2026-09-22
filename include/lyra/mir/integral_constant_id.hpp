#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of a constant integral value within a compilation unit. Derived from
// the value rather than conferred on it: two occurrences that wrote the same
// bits at the same type are one entity, so equal ids mean equal values. The
// counterpart of `TypeId` on the value axis -- a type descriptor and a constant
// are both settled before the program runs and shared by every use naming one.
struct IntegralConstantId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const IntegralConstantId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
