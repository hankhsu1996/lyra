#pragma once

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::mir {

// Computes the type of a place by applying all projections to the root type.
// This is the single source of truth for place types - Place stores only
// structure (root + projections), not cached types.
auto TypeOfPlace(const TypeArena& types, const Place& place) -> TypeId;

}  // namespace lyra::mir
