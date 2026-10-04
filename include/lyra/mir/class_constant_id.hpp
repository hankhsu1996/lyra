#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Identity of a constant a class holds: plain machine data fixed where the
// class is compiled, which no instance carries and nothing writes. Scoped to
// the class that holds it.
struct ClassConstantId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ClassConstantId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
