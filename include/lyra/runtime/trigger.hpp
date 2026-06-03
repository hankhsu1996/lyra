#pragma once

#include <cstdint>
#include <functional>

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

// One leaf subscription on a single `Observable`. The projection is described
// by `(lsb_bit_offset, bit_width)` in the observable's flat-bit address space.
// `bit_width == 0` is a "whole var" sentinel: any-change subscriptions fire on
// any write, and edge subscriptions sample bit 0 (LRM 9.4.2 LSB rule on a
// whole-var expression). For explicit projections (bit-select, range-select,
// LRM 9.4.2 LSB-reduced edge), `bit_width` is the projection's actual width.
//
// Multiple Triggers in a single `EventControlAwaitable` model an event-list
// `@(a or posedge b[3])` or a multi-leaf expression `@({clk_a, clk_b})`; the
// process resumes when any leaf passes its per-leaf classifier.
struct Trigger {
  Observable* observable = nullptr;
  Edge edge = Edge::kAnyChange;
  std::uint64_t lsb_bit_offset = 0;
  std::uint64_t bit_width = 0;
};

// Per-leaf fire decision: receives a waiter's `(lsb_bit_offset, bit_width,
// edge)` and returns whether to wake it. The caller (the producer of the
// value change -- typically `Var<PackedArray>::Set`) captures the
// value context (old/new) so the `Observable` stays value-type agnostic.
using EdgeClassifier =
    std::function<bool(std::uint64_t lsb, std::uint64_t width, Edge edge)>;

}  // namespace lyra::runtime
