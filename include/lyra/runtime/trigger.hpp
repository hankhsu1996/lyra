#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/runtime/observation.hpp"
#include "lyra/value/packed.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

class Observable;

// One leaf of a wait: a place it watches, which bits of that place's flat-bit
// encoding it reads, and what decides whether what happens there is an event.
// A width of zero reads the whole of it, which is what a place with no
// bit-addressed parts produces -- a named event among them, since the event
// itself carries no bits.
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
  value::BitPositions reads;

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
// is which bits the write reached and what they held before it, and the
// storage after the write is read where it lies while the change is told: a
// leaf reading none of those bits is passed over without a comparison, and a
// leaf reading some compares only the bits it shares with the write. Where the
// observable is not a packed value, nothing about its bits can be shown
// unchanged and every wait is asked.
//
// It is only ever told for a write that changed the bits it reached, so a leaf
// reading all of them is moved without being compared.
class Change {
 public:
  // Defined in the library: a unit's write builds and copies these, and a
  // definition written here would be compiled again by each such unit.
  Change(const Change&);
  Change(Change&&) noexcept;
  auto operator=(const Change&) -> Change&;
  auto operator=(Change&&) noexcept -> Change&;
  ~Change();

  // A change whose parts are not bits.
  [[nodiscard]] static auto Whole() -> Change;

  // The bits of `storage` at `reached`, kept as they stand before a write that
  // reaches them. The words kept are the storage's own words those bits lie
  // in, so the bits cost the words they span and no value is built.
  [[nodiscard]] static auto Reaching(
      const value::PackedArray& storage, value::BitPositions reached) -> Change;

  // The same storage once the write has landed, borrowed while the change is
  // told.
  void SetAfter(const value::PackedArray& storage);

  // Whether the bits reached hold what they held before. Asked before the
  // change is told, so a write that moved nothing is no change.
  [[nodiscard]] auto Unmoved() const -> bool;

  // Whether the bits at `reads` are known to be as they were, a width of zero
  // reading the whole of it as a leaf's does. `true` means a wait reading only
  // those bits need not be asked; `false` only that it may have been affected,
  // which is all a change whose parts are not bits can say.
  [[nodiscard]] auto KnownUnchanged(value::BitPositions reads) const -> bool;

 private:
  Change();

  // Whether the bits at `at` read the same before and after.
  [[nodiscard]] auto BitsUnmoved(value::BitPositions at) const -> bool;

  // The bits reached, a width of zero for a change whose parts are not bits.
  value::BitPositions reached_;
  // The storage's words from `first_word_` on that the bits reached lie in, as
  // they stood before the write, and the same words after it, where they lie.
  std::size_t first_word_ = 0;
  value::PackedWordArray before_value_;
  value::PackedWordArray before_unknown_;
  std::span<const std::uint64_t> after_value_;
  std::span<const std::uint64_t> after_unknown_;
};

}  // namespace lyra::runtime
