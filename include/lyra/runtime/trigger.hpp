#pragma once

#include <cstdint>

namespace lyra::runtime {

class Observable;

// LRM 9.4.2 edge specifier on `@(...)`. Stored per-trigger on the wait so the
// observable can decide which subscribed waiters to wake on a value change.
enum class Edge : std::uint8_t {
  kAnyChange,
  kPosedge,
  kNegedge,
  kBothEdges,
};

// One observable + the edge polarity the waiter cares about. Multiple
// Triggers in a single `EventControlAwaitable` model an event list
// `@(a or posedge b)`; the process resumes when any matches.
struct Trigger {
  Observable* observable = nullptr;
  Edge edge = Edge::kAnyChange;
};

}  // namespace lyra::runtime
