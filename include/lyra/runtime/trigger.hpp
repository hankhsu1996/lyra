#pragma once

#include <cstdint>
#include <functional>

#include "lyra/runtime/observation.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

class Observable;

// One leaf of a wait: a place it watches, which bits of that place's flat-bit
// encoding it reads, as `(lsb_bit_offset, bit_width)`, and what decides whether
// what happens there is an event. A width of zero reads the whole of it, which
// is what a place with no bit-addressed parts produces -- a named event among
// them, since the event itself carries no bits.
//
// Several leaves in one wait are an event list `@(a or posedge b[3])` or one
// expression over several variables `@({clk_a, clk_b})`; the wait resumes when
// any of them leads to an event. Leaves of one event expression name one
// observation between them, because the value being watched is the
// expression's and there is one of it.
struct Trigger {
  Observable* observable = nullptr;
  Observation observation;
  std::uint64_t lsb_bit_offset = 0;
  std::uint64_t bit_width = 0;

  Trigger() = default;

  // Each field beside the cell and the observation arrives as a PackedArray
  // literal -- the value model routes compile-time scalars as SV values, the
  // same way the runtime effect entries take their int args -- and converts to
  // its native field type here.
  Trigger(
      Observable* observable, Observation observation,
      const value::PackedArray& lsb_bit_offset,
      const value::PackedArray& bit_width);
};

// Whether a change left one leaf's bits exactly as they were. The producer of
// the change captures the values it moved between, so an observable stays
// agnostic to what it holds.
//
// It answers only in the negative direction: `true` means this leaf cannot have
// been affected and the wait need not be consulted, while `false` means it may
// have been. Where a value has no bit projection to speak of, answering `false`
// throughout is correct and simply consults every wait.
using ProjectionUnchanged =
    std::function<bool(std::uint64_t lsb, std::uint64_t width)>;

}  // namespace lyra::runtime
