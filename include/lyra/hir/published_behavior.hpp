#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

// Which of a class's introductions a behavior is. The position is the
// signature's own order, so the class that introduces it and the unit that
// dispatches on it both count it out of the same list and neither states it to
// the other.
struct PublishedBehaviorId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PublishedBehaviorId&) const
      -> std::strong_ordering = default;
};

// One behavior a class introduces (LRM 8.20), as the unit declaring the class
// published it. The name is what a caller writes and what a target language
// resolving by name spells the call as; which body answers it is a property of
// whatever class a value turns out to be, so nothing about a body is here.
struct PublishedBehavior {
  std::string name;
};

}  // namespace lyra::hir
