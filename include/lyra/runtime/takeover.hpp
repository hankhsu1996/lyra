#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/support/takeover_level.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/positions.hpp"
#include "lyra/value/value_type.hpp"

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

// The positions a takeover entry names of a value: `count` of them from
// `first`, numbered as the value's type numbers them (LRM 10.6.2) -- an
// integral value's bits from its least significant, an aggregate's parts' one
// part after another, and the one position of any other value.
[[nodiscard]] inline auto PositionsNamed(std::int64_t first, std::int64_t count)
    -> value::BitPositions {
  return {
      .lsb = static_cast<std::uint64_t>(first),
      .width = static_cast<std::uint64_t>(count)};
}

// A value a takeover may hold some of the bits of in place (LRM 10.6.2): one
// written position by position.
template <class T>
concept TakenOverByBits = requires(T& value, const T& other) {
  value.MutableView();
  other.View();
};

// How a takeover shows what it holds at some positions of a value of `T`. A
// type the library is compiled with takes them itself (LRM 10.6.2).
template <class T>
struct TakePositionsOf {
  [[nodiscard]] auto Taken(
      const T& into, const T& from, std::int64_t first,
      std::int64_t count) const -> T {
    return value::PositionsTaken(into, from, first, count);
  }
};

// A type the library was compiled without takes them by its own function, which
// the takeover is handed where it begins, since nothing else asks for it.
template <value::LaidOutByType T>
struct TakePositionsOf<T> {
  value::TakePositionsWitness take = nullptr;

  [[nodiscard]] auto Taken(
      const T& into, const T& from, std::int64_t first,
      std::int64_t count) const -> T {
    return T::Built(into.Type(), [&](void* out) {
      take(into.Type(), into.Bytes(), from.Bytes(), &first, &count, out);
    });
  }
};

// The positions a takeover still holds: runs of positions, none overlapping
// another. It starts with the one run it was begun over and only ever loses
// positions, to a later takeover at its level or to a release, which leaves
// none, one or two pieces of each run it reaches.
//
// Every member function is defined in the library: a unit taking a variable
// over builds and copies these, and a definition written here would be
// compiled again by each such unit.
class HeldPositions {
 public:
  explicit HeldPositions(value::BitPositions run);
  HeldPositions(const HeldPositions&);
  HeldPositions(HeldPositions&&) noexcept;
  auto operator=(const HeldPositions&) -> HeldPositions&;
  auto operator=(HeldPositions&&) noexcept -> HeldPositions&;
  ~HeldPositions();

  void Remove(value::BitPositions removed);

  [[nodiscard]] auto Empty() const -> bool;

  [[nodiscard]] auto Runs() const -> std::span<const value::BitPositions>;

 private:
  std::vector<value::BitPositions> runs_;
};

// The procedural continuous assignments in effect on one value (LRM 10.6).
// Each is a takeover: begun at one of two levels over some positions of the
// value, holding those it has not lost, and holding the value its evaluation
// last produced, once it has produced one.
//
// At one level no two takeovers hold a position in common, since beginning one
// takes its positions from whatever held them there (LRM 10.6.1), so each
// position shows the value of the takeover holding it at the highest level
// that has one -- which is the whole of how an `assign` overrides a procedural
// write and a `force` overrides an `assign` -- and what was written there
// where none does. A takeover goes on recording while a higher one covers it,
// so ending the higher one has its value ready with nothing recomputed (LRM
// 10.6.2).
//
// Where no takeover shows at any position the two kinds of variable part ways,
// as the standard has them. One that something drives continuously shows what
// its driver last produced, so that value is kept beneath the takeovers for as
// long as one shows. Any other keeps what it was last given.
//
// Ending a takeover that something lies beneath -- a lower one, or the
// continuous driver -- reestablishes that and schedules its reevaluation (LRM
// 10.6.2), so the positions released go on showing what they showed until the
// reevaluation runs. The driver's value stays kept beneath until then, so what
// it produces meanwhile lands there, and shows at once wherever no takeover
// holds.
//
// An evaluation carries the generation its takeover began under, and stops as
// soon as no takeover at its level carries that generation -- so a second
// `assign` ends the first and a `release` ends what it released, without
// either having to reach the evaluation and stop it directly.
template <class T>
class Takeovers {
 public:
  // Begins a takeover at `level` over `positions`, which every other takeover
  // at that level stops holding, and answers with its generation. A value held
  // whole is always named whole, as its one position. `take_positions` is how
  // what a takeover holds is shown, the same at every beginning.
  auto Begin(
      support::TakeoverLevel level, value::BitPositions positions,
      TakePositionsOf<T> take_positions) -> std::uint32_t {
    take_positions_ = take_positions;
    Remove(level, positions);
    const std::uint32_t generation = ++GenerationOf(level);
    // Built where it is kept, since its value may be larger than a frame keeps.
    takeovers_.emplace_back(level, generation, HeldPositions(positions));
    return generation;
  }

  // Records what the takeover of `generation` evaluated to. Answers whether it
  // still holds any position, which is what tells its evaluation to stop.
  auto Drive(
      support::TakeoverLevel level, std::uint32_t generation, const T& value)
      -> bool {
    const auto found = std::ranges::find_if(takeovers_, [&](const Takeover& t) {
      return t.level == level && t.generation == generation;
    });
    if (found == takeovers_.end()) {
      return false;
    }
    found->value = value;
    return true;
  }

  // Ends whatever holds `positions` at `level`.
  void End(support::TakeoverLevel level, value::BitPositions positions) {
    Remove(level, positions);
  }

  // Whether any takeover holds a position, whether or not it has produced a
  // value yet.
  [[nodiscard]] auto Holds() const -> bool {
    return !takeovers_.empty();
  }

  // Whether any takeover shows at any position: one whose evaluation has
  // produced a value. Between its beginning and its first evaluation a
  // takeover holds positions and shows nothing there.
  [[nodiscard]] auto Shows() const -> bool {
    return std::ranges::any_of(
        takeovers_, [](const Takeover& t) { return t.value.has_value(); });
  }

  // Makes every position of `written` a takeover shows at show what it holds
  // there, the higher level over the lower. It is rewritten where it lies,
  // since a value may be larger than a frame keeps.
  void Overlay(T& written) const {
    for (std::size_t level = 0; level < support::kTakeoverLevelCount; ++level) {
      for (const Takeover& t : takeovers_) {
        if (static_cast<std::size_t>(t.level) != level ||
            !t.value.has_value()) {
          continue;
        }
        for (const value::BitPositions run : t.held.Runs()) {
          OverlayRun(written, *t.value, run);
        }
      }
    }
  }

  // Starts keeping what drives the value continuously beneath the takeovers,
  // from `shown`, the value the driver gave it last. Asked as a takeover comes
  // to show where nothing is kept beneath yet.
  void KeepBeneath(const T& shown) {
    beneath_ = shown;
  }

  // What a write lands in where the value is driven continuously, so that the
  // positions no takeover shows at show it and the rest show it once released;
  // none where it is not.
  [[nodiscard]] auto Beneath() -> T* {
    return beneath_.has_value() ? &*beneath_ : nullptr;
  }

  // Forgets what was kept beneath, once no takeover shows and what a release
  // reestablished has been reevaluated.
  void DropBeneath() {
    beneath_.reset();
  }

  // Whether a write is overridden: a takeover shows, or something is kept
  // beneath, which outlasts the last takeover until the reevaluation its
  // release scheduled has shown it.
  [[nodiscard]] auto OverridesWrites() const -> bool {
    return beneath_.has_value() || Shows();
  }

 private:
  struct Takeover {
    support::TakeoverLevel level{};
    std::uint32_t generation = 0;
    HeldPositions held;
    std::optional<T> value;
  };

  [[nodiscard]] auto GenerationOf(support::TakeoverLevel level)
      -> std::uint32_t& {
    return generations_.at(static_cast<std::size_t>(level));
  }

  // Every takeover at `level` stops holding `positions`, and one left holding
  // none ends.
  void Remove(support::TakeoverLevel level, value::BitPositions positions) {
    for (Takeover& t : takeovers_) {
      if (t.level == level) {
        t.held.Remove(positions);
      }
    }
    std::erase_if(takeovers_, [](const Takeover& t) { return t.held.Empty(); });
  }

  // `written` showing what `held` holds at `run`.
  void OverlayRun(T& written, const T& held, value::BitPositions run) const {
    if constexpr (TakenOverByBits<T>) {
      value::InsertWithin(written.MutableView(), held.View(), 0, run);
    } else {
      written = take_positions_.Taken(
          written, held, static_cast<std::int64_t>(run.lsb),
          static_cast<std::int64_t>(run.width));
    }
  }

  [[no_unique_address]] TakePositionsOf<T> take_positions_;
  std::vector<Takeover> takeovers_;
  std::array<std::uint32_t, support::kTakeoverLevelCount> generations_{};
  std::optional<T> beneath_;
};

}  // namespace lyra::runtime
