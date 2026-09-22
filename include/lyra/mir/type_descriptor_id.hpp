#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of one run-time description within a compilation unit. Derived from
// what the description says rather than conferred on it: two uses that need the
// same thing of a declaration reach one entry, so equal ids mean one
// description. The counterpart of `IntegralConstantId` over what a declaration
// says rather than over what a value holds.
struct TypeDescriptorId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const TypeDescriptorId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
