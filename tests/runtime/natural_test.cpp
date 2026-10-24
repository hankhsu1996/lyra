#include "lyra/runtime/natural.hpp"

#include <cstdint>
#include <gtest/gtest.h>
#include <vector>

#include "lyra/runtime/rng.hpp"

namespace {

using lyra::runtime::DrawRng;
using lyra::runtime::Natural;
using lyra::runtime::RandomSeed;

// The number one followed by `zeros` zero bits.
auto PowerOfTwo(std::uint32_t zeros) -> Natural {
  return Natural{1}.ShiftedLeft(zeros);
}

// Sums, differences and products carry and borrow across words: one less
// than 2^128, added to one, is 2^128; 2^128 less one is the first again; and
// (2^64 - 1)^2 is 2^128 - 2^65 + 1.
TEST(NaturalTest, ArithmeticCarriesAcrossWords) {
  const Natural all_ones = PowerOfTwo(128) - Natural{1};
  EXPECT_EQ(all_ones + Natural{1}, PowerOfTwo(128));
  EXPECT_EQ(PowerOfTwo(128) - Natural{1}, all_ones);
  const Natural word = Natural{~std::uint64_t{0}};
  EXPECT_EQ(word * word, PowerOfTwo(128) - PowerOfTwo(65) + Natural{1});
  EXPECT_EQ(Natural{} * word, Natural{});
  EXPECT_TRUE(PowerOfTwo(64) > word);
  EXPECT_TRUE(Natural{3} < Natural{5});
}

// Bits taken off the bottom come back least significant first and leave the
// rest shifted down, across a word boundary as within one.
TEST(NaturalTest, LowBitsComeOffAcrossAWord) {
  Natural n = PowerOfTwo(70) + Natural{0b101};
  const std::vector<bool> low = n.TakeLowBits(3);
  EXPECT_EQ(low, (std::vector<bool>{true, false, true}));
  EXPECT_EQ(n, PowerOfTwo(67));
  const std::vector<bool> more = n.TakeLowBits(68);
  EXPECT_TRUE(more[67]);
  EXPECT_TRUE(n.IsZero());
  EXPECT_EQ(Natural::FromBits(more), PowerOfTwo(67));
}

// A draw below a bound past one word stays below it and reaches both halves
// of its range.
TEST(NaturalTest, ADrawBelowAWideBoundStaysBelowIt) {
  const Natural bound = PowerOfTwo(100) + Natural{7};
  const Natural half = PowerOfTwo(99);
  DrawRng rng{RandomSeed{11}};
  int high = 0;
  constexpr int kDraws = 2000;
  for (int i = 0; i < kDraws; ++i) {
    const Natural drawn = Natural::DrawBelow(rng, bound);
    ASSERT_TRUE(drawn < bound);
    high += drawn < half ? 0 : 1;
  }
  EXPECT_GT(high, 850);
  EXPECT_LT(high, 1150);
}

}  // namespace
