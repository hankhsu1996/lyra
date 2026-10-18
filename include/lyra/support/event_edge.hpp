#pragma once

#include <cstdint>

namespace lyra::support {

// LRM 9.4.2 edge specifier on `@(...)`: what an event control was written
// with, which the watch built for the control compares by to decide whether a
// change in the expression's value is the event that control waits for.
// `kAnyChange` is what an event control carrying no edge keyword waits for --
// a change anywhere in the value, rather than a direction its least
// significant bit took.
enum class EventEdge : std::uint8_t {
  kAnyChange,
  kPosedge,
  kNegedge,
  kBothEdges,
};

}  // namespace lyra::support
