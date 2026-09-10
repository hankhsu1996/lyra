#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>

#include "lyra/support/takeover_level.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// Which level a takeover entry acts on. It arrives as a PackedArray literal,
// the way every compile-time scalar crosses into a runtime entry.
[[nodiscard]] inline auto TakeoverLevelOf(const value::PackedArray& level)
    -> support::TakeoverLevel {
  return static_cast<support::TakeoverLevel>(level.ToInt64());
}

// The width and domain a takeover's generation crosses in. It is a counter the
// evaluation carries and hands back, never a value the design can see, so what
// decides its shape is only that both ends agree on one.
inline constexpr std::uint64_t kTakeoverGenerationBits = 32;

[[nodiscard]] inline auto TakeoverGenerationValue(std::uint32_t generation)
    -> value::PackedArray {
  return value::PackedArray::FromInt(
      static_cast<std::int64_t>(generation), kTakeoverGenerationBits, false,
      false);
}

[[nodiscard]] inline auto TakeoverGenerationOf(
    const value::PackedArray& generation) -> std::uint32_t {
  return static_cast<std::uint32_t>(generation.ToInt64());
}

// The procedural continuous assignments in effect on one cell, each holding
// the value it last evaluated to (LRM 10.6). What the cell shows is the
// highest level in effect, so a value arriving from any level below that one
// is recorded and goes no further -- which is the whole of how an `assign`
// overrides a procedural write and a `force` overrides an `assign`.
//
// Ending a level hands the cell to the highest one still in effect. Where none
// is left the cell is told nothing, because it already holds the right value:
// the writes a takeover displaced were discarded rather than saved, so a
// released variable keeps what it was last given (LRM 10.6.1, 10.6.2).
//
// A level carries a generation beside its value. Whatever evaluates a takeover
// carries the generation it started under, and stops as soon as the two
// disagree -- so a second `assign` ends the first (LRM 10.6.1) and a `release`
// ends what it released, without either having to reach the evaluation and
// stop it directly.
template <class T>
class Takeovers {
 public:
  // Starts a takeover at `level`, superseding whatever was driving it, and
  // answers with the generation the new evaluation carries.
  auto Begin(support::TakeoverLevel level) -> std::uint32_t {
    return ++GenerationOf(level);
  }

  // Records what a level evaluated to, unless the evaluation offering it has
  // been superseded. Answers whether that evaluation is still the one driving
  // this level, which is what tells it to stop.
  //
  // A level records even while a higher one covers it, so the value it would
  // show is current the moment that higher level ends -- which is what lets a
  // release reestablish the assignment underneath it with nothing recomputed.
  auto Drive(
      support::TakeoverLevel level, std::uint32_t generation, const T& value)
      -> bool {
    if (GenerationOf(level) != generation) {
      return false;
    }
    Slot(level) = value;
    return true;
  }

  // Ends the takeover at `level`, and stops whatever was evaluating it.
  void End(support::TakeoverLevel level) {
    Slot(level).reset();
    ++GenerationOf(level);
  }

  // The value the cell should be showing, or nothing where no level is in
  // effect and the cell's own storage is what shows. The levels are an ordered
  // scale, so what shows is the first one found descending them.
  [[nodiscard]] auto Highest() const -> const T* {
    for (std::size_t above = slots_.size(); above > 0; --above) {
      if (slots_[above - 1].has_value()) {
        return &*slots_[above - 1];
      }
    }
    return nullptr;
  }

 private:
  [[nodiscard]] static auto IndexOf(support::TakeoverLevel level)
      -> std::size_t {
    return static_cast<std::size_t>(level);
  }

  [[nodiscard]] auto Slot(support::TakeoverLevel level) -> std::optional<T>& {
    return slots_[IndexOf(level)];
  }

  [[nodiscard]] auto GenerationOf(support::TakeoverLevel level)
      -> std::uint32_t& {
    return generations_[IndexOf(level)];
  }

  std::array<std::optional<T>, support::kTakeoverLevelCount> slots_;
  std::array<std::uint32_t, support::kTakeoverLevelCount> generations_{};
};

}  // namespace lyra::runtime
