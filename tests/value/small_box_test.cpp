#include "lyra/value/small_box.hpp"

#include <array>
#include <cstddef>
#include <gtest/gtest.h>
#include <utility>

#include "lyra/value/frame_bound.hpp"

namespace lyra::value {
namespace {

// Counts how many values of its kind are alive, so a box is seen to end the
// value it holds when it ends.
template <std::size_t kBytes>
struct Counted {
  explicit Counted(int value) : value(value) {
    ++alive;
  }
  Counted(const Counted& other) : value(other.value) {
    ++alive;
  }
  auto operator=(const Counted&) -> Counted& = default;
  Counted(Counted&& other) noexcept : value(other.value) {
    ++alive;
  }
  auto operator=(Counted&&) -> Counted& = default;
  ~Counted() {
    --alive;
  }

  int value;
  std::array<std::byte, kBytes> padding{};
  static inline int alive = 0;
};

using Small = Counted<8>;
using Large = Counted<kMaxInlineBytes + 1>;

// A value that fits a frame is kept inline in whatever holds the box, and one
// past that in storage the box owns, so the box is then one address.
TEST(SmallBox, KeepsASmallValueInlineAndALargeOneApart) {
  static_assert(sizeof(SmallBox<Small>) == sizeof(Small));
  static_assert(sizeof(SmallBox<Large>) == sizeof(void*));

  const SmallBox<Small> small(3);
  EXPECT_EQ(small->value, 3);
  const SmallBox<Large> large(4);
  EXPECT_EQ((*large).value, 4);
}

// Either way the box holds its value from the moment it is built and ends it
// when it ends, and a move hands the value on.
TEST(SmallBox, HoldsItsValueForAsLongAsItLasts) {
  {
    SmallBox<Small> small(1);
    SmallBox<Large> large(1);
    EXPECT_EQ(Small::alive, 1);
    EXPECT_EQ(Large::alive, 1);
    const SmallBox<Large> taken(std::move(large));
    EXPECT_EQ(Large::alive, 1);
    EXPECT_EQ(taken->value, 1);
  }
  EXPECT_EQ(Small::alive, 0);
  EXPECT_EQ(Large::alive, 0);
}

}  // namespace
}  // namespace lyra::value
