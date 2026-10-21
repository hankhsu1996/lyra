#include "lyra/value/small_box.hpp"

#include <array>
#include <cstddef>
#include <gtest/gtest.h>
#include <utility>

#include "lyra/value/frame_bound.hpp"

namespace lyra::value {
namespace {

// What a test watches of the values it makes. How many are alive shows a box
// ending the value it holds when it ends, and how many were copied or moved
// shows a value built where it is kept.
struct Tally {
  int alive = 0;
  int relocated = 0;
};

template <std::size_t kBytes>
struct Counted {
  Counted(Tally& tally, int value) : tally(&tally), value(value) {
    ++tally.alive;
  }
  Counted(const Counted& other) : tally(other.tally), value(other.value) {
    ++tally->alive;
    ++tally->relocated;
  }
  auto operator=(const Counted&) -> Counted& = default;
  Counted(Counted&& other) noexcept : tally(other.tally), value(other.value) {
    ++tally->alive;
    ++tally->relocated;
  }
  auto operator=(Counted&&) -> Counted& = default;
  ~Counted() {
    --tally->alive;
  }

  Tally* tally;
  int value;
  std::array<std::byte, kBytes> padding{};
};

using Small = Counted<8>;
using Large = Counted<kMaxInlineBytes + 1>;

// A value that fits a frame is kept inline in whatever holds the box, and one
// past that in storage the box owns, so the box is then one address.
TEST(SmallBox, KeepsASmallValueInlineAndALargeOneApart) {
  static_assert(sizeof(SmallBox<Small>) == sizeof(Small));
  static_assert(sizeof(SmallBox<Large>) == sizeof(void*));

  Tally tally;
  const SmallBox<Small> small(tally, 3);
  EXPECT_EQ(small->value, 3);
  const SmallBox<Large> large(tally, 4);
  EXPECT_EQ((*large).value, 4);
}

// Either way the box holds its value from the moment it is built and ends it
// when it ends, and a move hands the value on.
TEST(SmallBox, HoldsItsValueForAsLongAsItLasts) {
  Tally small_tally;
  Tally large_tally;
  {
    SmallBox<Small> small(small_tally, 1);
    SmallBox<Large> large(large_tally, 1);
    EXPECT_EQ(small_tally.alive, 1);
    EXPECT_EQ(large_tally.alive, 1);
    const SmallBox<Large> taken(std::move(large));
    EXPECT_EQ(large_tally.alive, 1);
    EXPECT_EQ(taken->value, 1);
  }
  EXPECT_EQ(small_tally.alive, 0);
  EXPECT_EQ(large_tally.alive, 0);
}

// A box built from what a maker answers holds that value where it keeps it,
// never a copy or a move of one made elsewhere first.
TEST(SmallBox, BuildsWhatAMakerAnswersWhereItKeepsIt) {
  Tally small_tally;
  Tally large_tally;
  const auto small =
      SmallBox<Small>::BuiltBy([&] { return Small(small_tally, 5); });
  const auto large =
      SmallBox<Large>::BuiltBy([&] { return Large(large_tally, 6); });
  EXPECT_EQ(small->value, 5);
  EXPECT_EQ(large->value, 6);
  EXPECT_EQ(small_tally.relocated, 0);
  EXPECT_EQ(large_tally.relocated, 0);
}

}  // namespace
}  // namespace lyra::value
