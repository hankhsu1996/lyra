#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <variant>

#include "lyra/base/fixed_array.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"

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
  explicit WatchedPlace(const Observable* reported_to);

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

// The positions of a part a leaf or a report is told of, from its first
// position and how many there are, none meaning the whole part. A select over
// a name a view joins several declarations under names the same positions in
// each, so in the less significant ones it may begin below the part, as a
// write of it does and lands only on what lies within (LRM 11.5.1): only the
// positions at or above the part's first are named. One lying wholly below the
// part keeps a first position past every one it has, so nothing written there
// reaches it.
[[nodiscard]] auto PositionsInPart(std::int64_t first, std::int64_t count)
    -> value::BitPositions;

// One leaf of a wait: a place it watches, which part of the place, which
// positions of that part it reads -- bits where the part is a packed value,
// elements where it is an array read as a slice (LRM 7.4.5) -- and what
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
      WatchedPlace place, Observation observation,
      std::span<const std::int64_t> steps, std::int64_t lsb_bit_offset,
      std::int64_t bit_width);
};

// What a write that changed a part of an observable did to that part, as far
// as a wait on the part, or on a part inside it, can be told (LRM 4.3). The
// write is what holds the value it replaced and the value it left, so it
// answers for every wait at once. Where the part is a packed value, the change
// is which bits the write reached and the
// words they lie in before and after it: a leaf reading none of those bits is
// passed over without a comparison, and a leaf reading some compares only the
// bits it shares with the write. Where the part is any other value whose type
// names its parts, the change is the value before and after the write, read
// where each lies while the waits are told, so a wait on a part inside it is
// told whether that part changed. Where neither is known, every wait on it is
// asked. Which part it was is told beside the change, by the part's path.
//
// It is only ever told where what it reached moved -- by a write, or by an
// interval, measured from the part's original -- so a leaf reading all of it
// is moved without being compared. What it says of any part is whether a wait
// there may be affected, never that its value is another: an observer that
// answers for a change of value itself -- a foreign callback on a value or a
// size change (LRM 38.36.1) -- compares what it reads with what it last read.
class Change {
 public:
  // Defined in the library: a unit's write builds and copies these, and a
  // definition written here would be compiled again by each such unit.
  Change(const Change&);
  Change(Change&&) noexcept;
  auto operator=(const Change&) -> Change&;
  auto operator=(Change&&) noexcept -> Change&;
  ~Change();

  // A change of which nothing more is known than that it may have happened.
  [[nodiscard]] static auto Whole() -> Change;

  // A change of a value of `type` from the one at `before` to the one at
  // `after`, which are no longer equal. Both have to stay where they are until
  // the change has been told.
  [[nodiscard]] static auto Replacing(
      const value::ValueType& type, const void* before, const void* after)
      -> Change;

  // What became of a value of `type` that held what lies at `before` and now
  // holds what lies at `after`: none where the two are the same bits, and
  // otherwise its change -- of the bits, read as the type lays them out, where
  // the value is integral, and of the value, read where both lie, where it is
  // not.
  [[nodiscard]] static auto Between(
      const value::ValueType& type, const void* before, const void* after)
      -> std::optional<Change>;

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

  // Whether the bits, or the elements, at `reads` are known to be as they
  // were, a width of zero reading the whole of it as a leaf's does. `true`
  // means a wait reading only those need not be asked; `false` only that it
  // may have been affected, which is all a change known only to have happened
  // can say.
  [[nodiscard]] auto KnownUnchanged(value::BitPositions reads) const -> bool;

  // What became of the part `step` names inside the part this change is of
  // (LRM 7.2, 7.4): nothing where it is known to be as it was, and otherwise
  // what can be said of it.
  [[nodiscard]] auto Inside(std::uint32_t step) const -> std::optional<Change>;

 private:
  // Nothing known but that the part may have changed.
  struct Unknown {};
  // A run of elements reached, a width of zero naming none in particular.
  struct ElementRun {
    value::BitPositions reached;
  };
  // The storage's words the bits reached lie in, `count` of each plane from
  // `first_word` on, as four runs one after another: the value plane and the
  // unknown plane as they stood before the write, then both as they stand
  // after it. A word a plane does not reach is clear. Bits lying in one word
  // need no buffer.
  struct BitWords {
    static constexpr std::size_t kBeforeValue = 0;
    static constexpr std::size_t kBeforeUnknown = 1;
    static constexpr std::size_t kAfterValue = 2;
    static constexpr std::size_t kAfterUnknown = 3;

    [[nodiscard]] auto WordsOf(std::size_t run) -> std::span<std::uint64_t>;
    [[nodiscard]] auto WordsOf(std::size_t run) const
        -> std::span<const std::uint64_t>;
    // Whether the bits at `at` read the same before and after.
    [[nodiscard]] auto Unmoved(value::BitPositions at) const -> bool;

    value::BitPositions reached;
    std::size_t first_word = 0;
    std::size_t count = 0;
    base::FixedArray<std::uint64_t, 4> words;
  };
  // A value before and after, each where it lies.
  struct ReplacedValue {
    const value::ValueType* type;
    const void* before;
    const void* after;
  };
  using Shape = std::variant<Unknown, ElementRun, BitWords, ReplacedValue>;

  explicit Change(Shape shape);

  Shape shape_;
};

}  // namespace lyra::runtime
