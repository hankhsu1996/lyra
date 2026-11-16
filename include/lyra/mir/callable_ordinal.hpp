#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::mir {

// Where a callable stands among the callables its owner declares -- a class's,
// or the unit's namespace's -- counted in the order the owner declares them, as
// rustc's disambiguator tells apart the definitions one parent holds.
// Meaningless alone, since ordinal 0 of one owner is unrelated to ordinal 0 of
// another; it is what spells a body nothing names, which only its owner's own
// code reaches.
struct CallableOrdinal {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const CallableOrdinal&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
