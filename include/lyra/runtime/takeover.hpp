#pragma once

#include <array>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>

#include "lyra/support/takeover_level.hpp"

namespace lyra::runtime {

// Which level a takeover entry acts on. It arrives as a machine integer, the
// way every runtime scalar crosses into a runtime entry.
[[nodiscard]] inline auto TakeoverLevelOf(std::int64_t level)
    -> support::TakeoverLevel {
  return static_cast<support::TakeoverLevel>(level);
}

// A takeover's generation is a counter the evaluation carries and hands back,
// never a value the design can see, so it crosses a runtime entry in both
// directions as the machine integer every runtime scalar is.
[[nodiscard]] inline auto TakeoverGenerationOf(std::int64_t generation)
    -> std::uint32_t {
  return static_cast<std::uint32_t>(generation);
}

// The procedural continuous assignments in effect on one cell, each holding
// the value it last evaluated to (LRM 10.6). What the cell shows is the
// highest level in effect, so a value arriving from any level below that one
// is recorded and goes no further -- which is the whole of how an `assign`
// overrides a procedural write and a `force` overrides an `assign`.
//
// Ending a level hands the cell to the highest one still in effect. Where none
// is left the two kinds of variable part ways, as the standard has them (LRM
// 10.6.2). One that something drives continuously shows what its driver last
// produced, so that value is kept beneath the levels for as long as one covers
// the cell and the driver's writes go there. Any other keeps what it was last
// given, and a write a takeover displaced is dropped.
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
    return Drive(
        level, generation, [&](std::optional<T>& held) { held = value; });
  }

  // The same where the value arrives in a form of the caller's own: `record`
  // is handed what the level holds, which is nothing until the first value
  // after the level began, and leaves the value there.
  template <std::invocable<std::optional<T>&> Record>
  auto Drive(
      support::TakeoverLevel level, std::uint32_t generation, Record record)
      -> bool {
    if (GenerationOf(level) != generation) {
      return false;
    }
    record(Slot(level));
    return true;
  }

  // Ends the takeover at `level`, and stops whatever was evaluating it.
  void End(support::TakeoverLevel level) {
    Slot(level).reset();
    ++GenerationOf(level);
  }

  // Starts keeping what drives the cell continuously beneath the levels, from
  // `shown`, the value the driver gave it last. Asked as the first level comes
  // to cover the cell.
  void KeepBeneath(const T& shown) {
    beneath_ = shown;
  }

  // What a write the levels turned away lands in, where the cell is driven
  // continuously, and nothing where it is not.
  [[nodiscard]] auto Beneath() -> T* {
    return beneath_.has_value() ? &*beneath_ : nullptr;
  }

  // Gives up what was kept beneath, once no level covers the cell.
  [[nodiscard]] auto TakeBeneath() -> std::optional<T> {
    return std::exchange(beneath_, std::nullopt);
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
  std::optional<T> beneath_;
};

}  // namespace lyra::runtime
