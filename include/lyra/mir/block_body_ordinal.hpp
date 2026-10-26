#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Where one block body stands among the block bodies of the unit body it was
// compiled in (LRM 27): what whoever assembles the program names the entry
// making a block of that body by, beside the unit and the body.
struct BlockBodyOrdinal {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const BlockBodyOrdinal&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
