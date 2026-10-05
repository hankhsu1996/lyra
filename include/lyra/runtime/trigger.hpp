#pragma once

#include <cstdint>
#include <span>

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
//
// Every member is defined in the library: a unit stating a wait builds, copies
// and destroys these, and a definition written here would be compiled again by
// each such unit.
struct Trigger {
  Observable* observable = nullptr;
  Observation observation;
  std::uint64_t lsb_bit_offset = 0;
  std::uint64_t bit_width = 0;

  Trigger();
  Trigger(const Trigger&);
  auto operator=(const Trigger&) -> Trigger&;
  Trigger(Trigger&&) noexcept;
  auto operator=(Trigger&&) noexcept -> Trigger&;
  ~Trigger();

  // Each field beside the cell and the observation arrives as a PackedArray
  // literal -- the value model routes compile-time scalars as SV values, the
  // same way the runtime effect entries take their int args -- and converts to
  // its native field type here.
  Trigger(
      Observable* observable, Observation observation,
      const value::PackedArray& lsb_bit_offset,
      const value::PackedArray& bit_width);
};

// What a write that changed an observable did to it, as far as a wait on some
// of its bits can be told. Where the observable is a packed value, the change
// borrows its planes from before and after the write, for as long as the change
// is being told, and a leaf's bits are compared where they lie; where it is
// not, nothing about its bits can be shown untouched and every wait is asked.
//
// It is only ever made for a write that changed the observable, so a leaf
// covering every bit is moved without being compared.
class Change {
 public:
  // A change whose parts are not bit runs.
  [[nodiscard]] static auto Whole() -> Change;

  // A change between two values of one packed shape.
  [[nodiscard]] static auto Between(
      const value::PackedArray& before, const value::PackedArray& after)
      -> Change;

  // Whether the `bit_width` bits from `lsb_bit_offset` are as they were, a
  // width of zero reading the whole of it as a leaf's does. It answers only in
  // the negative direction: `true` means a wait reading only those bits need
  // not be asked, and `false` that it may have been affected.
  [[nodiscard]] auto LeftAlone(
      std::uint64_t lsb_bit_offset, std::uint64_t bit_width) const -> bool;

 private:
  Change();

  std::span<const std::uint64_t> before_value_;
  std::span<const std::uint64_t> before_unknown_;
  std::span<const std::uint64_t> after_value_;
  std::span<const std::uint64_t> after_unknown_;
  // Zero for a change whose parts are not bit runs.
  std::uint64_t bit_width_ = 0;
};

}  // namespace lyra::runtime
