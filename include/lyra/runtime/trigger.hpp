#pragma once

#include <cstdint>
#include <functional>

#include "lyra/support/event_edge.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

class Observable;

// One leaf subscription on a single `Observable`. The projection is described
// by `(lsb_bit_offset, bit_width)` in the observable's flat-bit address space.
// `bit_width == 0` is a "whole var" sentinel: any-change subscriptions fire on
// any write, and edge subscriptions sample bit 0 (LRM 9.4.2 LSB rule on a
// whole-var expression). For explicit projections (bit-select, range-select,
// LRM 9.4.2 LSB-reduced edge), `bit_width` is the projection's actual width.
//
// Multiple Triggers in a single wait model an event-list `@(a or posedge b[3])`
// or a multi-leaf expression `@({clk_a, clk_b})`; the process resumes when any
// leaf passes its per-leaf classifier.
struct Trigger {
  Observable* observable = nullptr;
  support::EventEdge edge = support::EventEdge::kAnyChange;
  std::uint64_t lsb_bit_offset = 0;
  std::uint64_t bit_width = 0;

  Trigger() = default;

  // Each field beside the cell arrives as a PackedArray literal -- the value
  // model routes compile-time scalars as SV values, the same way the runtime
  // effect entries take their int args -- and converts to its native field type
  // here.
  Trigger(
      Observable* observable, const value::PackedArray& edge,
      const value::PackedArray& lsb_bit_offset,
      const value::PackedArray& bit_width);
};

// Per-leaf fire decision: receives a waiter's `(lsb_bit_offset, bit_width,
// edge)` and returns whether to wake it. The caller (the producer of the
// value change -- typically `Var<PackedArray>::Set`) captures the
// value context (old/new) so the `Observable` stays value-type agnostic.
using EdgeClassifier = std::function<bool(
    std::uint64_t lsb, std::uint64_t width, support::EventEdge edge)>;

}  // namespace lyra::runtime
