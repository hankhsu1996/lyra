#pragma once

// Shared process-local place collection helper.
// Used by both PlaceCollector (process.cpp) and layout place collection
// (layout.cpp) to ensure the collection rule is defined once.

#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::mir {

// Insert a place into a container if its root is process-local.
// Container must support .insert(PlaceId).
template <typename Container>
void CollectIfProcessLocal(
    PlaceId place_id, const Arena& arena, Container& out) {
  const auto& place = arena[place_id];
  if (IsProcessLocalRoot(place.root.kind)) {
    out.insert(place_id);
  }
}

}  // namespace lyra::mir
