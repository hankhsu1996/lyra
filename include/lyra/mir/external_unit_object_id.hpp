#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

struct ExternalUnitObjectId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ExternalUnitObjectId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
