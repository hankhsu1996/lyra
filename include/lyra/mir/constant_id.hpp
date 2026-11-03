#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of a value a compilation unit settled before the program runs.
// Derived from the value and its type rather than conferred on them: two
// occurrences stating the same value at the same type are one entity, so equal
// ids mean equal values. The counterpart of `TypeId` on the value axis.
struct ConstantId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ConstantId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
