#pragma once

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/base/fixed_array.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

class Observable;
struct ErasedReference;

// A place a wait watches, as the one word it crosses to the library in: where
// occurrences are reported, or the member of a scope a reference was bound into
// (LRM 23.3.3) where the place was reached through one. A member is named
// rather than what it is bound to now, because a force binds it to other
// storage (LRM 10.6.2) and a wait built on it has to follow.
class WatchedPlace {
 public:
  WatchedPlace() = default;
  explicit WatchedPlace(Observable* reported_to);

  [[nodiscard]] static auto Through(const ErasedReference& reference)
      -> WatchedPlace;
  [[nodiscard]] static auto FromWord(void* word) -> WatchedPlace;
  [[nodiscard]] auto Word() const -> void*;

  // Where occurrences at the place are reported now, none for storage nothing
  // is told about.
  [[nodiscard]] auto ReportedTo() const -> Observable*;
  // The member the place was reached through, none where it was reached by
  // itself.
  [[nodiscard]] auto Member() const -> const ErasedReference*;
  // The part of what occurrences are reported to that `steps` reach from the
  // place: from the part a member names, where it was reached through one.
  [[nodiscard]] auto PathOf(std::span<const std::int64_t> steps) const
      -> PartPath;

 private:
  std::uintptr_t word_ = 0;
};

// One leaf of a wait: a place it watches, which part of the place, which
// positions of that part it reads -- bits where the part is a packed value,
// elements where it is an array read as a slice (LRM 7.4.6) -- and what
// decides whether what happens there is an event. A width of zero reads the
// whole of the part, which is what a part with no bits to address produces -- a
// named event among them, since the event itself carries no bits.
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
  // The member the place was reached through, which is only ever compared.
  const ErasedReference* through = nullptr;
  // Which part of the place it watches, the whole of it where the path is
  // empty.
  PartPath part;
  Observation observation;
  // Which bits of that part it reads where the part is a packed value, which
  // elements where it is an array.
  value::BitPositions reads;

  Trigger();
  Trigger(const Trigger&);
  auto operator=(const Trigger&) -> Trigger&;
  Trigger(Trigger&&) noexcept;
  auto operator=(Trigger&&) noexcept -> Trigger&;
  ~Trigger();

  // `steps` lead from the place to the part watched, in order: an element's
  // ordinal, a member's index.
  Trigger(
      Observable* observable, Observation observation,
      std::span<const std::int64_t> steps, std::int64_t lsb_bit_offset,
      std::int64_t bit_width);
  Trigger(
      WatchedPlace place, Observation observation,
      std::span<const std::int64_t> steps, std::int64_t lsb_bit_offset,
      std::int64_t bit_width);
};

// What a write that changed a part of an observable did to that part, as far
// as a wait on some of the part's bits can be told. Where the part is a packed
// value, the change is which bits the write reached and the words they lie in
// before and after it: a leaf reading none of those bits is passed over
// without a comparison, and a leaf reading some compares only the bits it
// shares with the write. Where the part is not a packed value, nothing about
// its bits can be shown unchanged and every wait on it is asked. Which part it
// was is told beside the change, by the part's path.
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

  // A change of the elements `reached` of an array, by their ordinals (LRM
  // 7.6 has a slice assigned as one), which says nothing of what became of any
  // one of them: a wait on other elements is passed over, and one on any of
  // these is asked. Of no elements in particular where the run is empty.
  [[nodiscard]] static auto Elements(value::BitPositions reached) -> Change;

  // The bits of `storage` at `reached`, kept as they stand before a write that
  // reaches them. The words kept are the storage's own words those bits lie
  // in, so the bits cost the words they span and no value is built.
  [[nodiscard]] static auto Reaching(
      const value::ConstIntegralView& storage, value::BitPositions reached)
      -> Change;

  // The same words of the storage once the write has landed.
  void SetAfter(const value::ConstIntegralView& storage);

  // The same two for storage that is one word in each plane, which is handed
  // over as those words; a plane a two-state value does not carry is clear.
  [[nodiscard]] static auto ReachingInWord(
      std::uint64_t value, std::uint64_t unknown, value::BitPositions reached)
      -> Change;
  void SetAfterInWord(std::uint64_t value, std::uint64_t unknown);

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
  // The storage's words the bits reached lie in, `count_` of each plane from
  // `first_word_` on, as four runs one after another: the value plane and the
  // unknown plane as they stood before the write, then both as they stand
  // after it. A word a plane does not reach is clear. Bits lying in one word
  // need no buffer.
  static constexpr std::size_t kBeforeValue = 0;
  static constexpr std::size_t kBeforeUnknown = 1;
  static constexpr std::size_t kAfterValue = 2;
  static constexpr std::size_t kAfterUnknown = 3;
  [[nodiscard]] auto WordsOf(std::size_t run) -> std::span<std::uint64_t>;
  [[nodiscard]] auto WordsOf(std::size_t run) const
      -> std::span<const std::uint64_t>;

  std::size_t first_word_ = 0;
  std::size_t count_ = 0;
  base::FixedArray<std::uint64_t, 4> words_;
};

}  // namespace lyra::runtime
