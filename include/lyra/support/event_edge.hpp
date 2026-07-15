#pragma once

#include <cstdint>

namespace lyra::support {

// LRM 9.4.2 edge specifier on `@(...)`. Shared vocabulary: the compiler encodes
// each observed leaf's polarity, and the runtime decides which subscribed
// waiters a value change wakes. `kAnyChange` is the polarity of an implicit
// sensitivity (an `always_comb` / `always_latch` body, an `@*`, a `wait
// (cond)`, a continuous assignment), which no edge keyword names.
enum class EventEdge : std::uint8_t {
  kAnyChange,
  kPosedge,
  kNegedge,
  kBothEdges,
};

}  // namespace lyra::support
