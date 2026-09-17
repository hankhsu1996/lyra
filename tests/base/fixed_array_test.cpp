#include "lyra/base/fixed_array.hpp"

#include <cstdint>
#include <gtest/gtest.h>
#include <utility>
#include <vector>

namespace {

using lyra::base::FixedArray;

// Two inline slots, so one sequence can be exercised on each side of the
// boundary and two of the same length can be compared within one regime.
using Words = FixedArray<std::uint64_t, 2>;

struct Pair {
  std::int64_t left;
  std::int64_t right;

  auto operator==(const Pair&) const -> bool = default;
};

using Pairs = FixedArray<Pair, 2>;

// The whole point of the type: a sequence that fits carries no bookkeeping for
// the one that does not.
static_assert(sizeof(FixedArray<std::uint64_t, 1>) == 24);
static_assert(sizeof(Pairs) == 48);

auto Contents(const Words& words) -> std::vector<std::uint64_t> {
  return {words.begin(), words.end()};
}

TEST(FixedArrayTest, HoldsWhatFitsAndWhatDoesNot) {
  const Words empty;
  EXPECT_EQ(empty.size(), 0U);
  EXPECT_TRUE(empty.empty());
  EXPECT_EQ(Contents(empty), std::vector<std::uint64_t>{});

  const std::vector<std::uint64_t> source{10, 20, 30, 40, 50};
  for (std::size_t length = 1; length <= source.size(); ++length) {
    const auto taken = static_cast<std::ptrdiff_t>(length);
    const Words words(source.begin(), source.begin() + taken);
    EXPECT_EQ(words.size(), length);
    EXPECT_FALSE(words.empty());
    EXPECT_EQ(words.front(), 10U);
    EXPECT_EQ(words.back(), source[length - 1U]);
    EXPECT_EQ(
        Contents(words),
        std::vector<std::uint64_t>(source.begin(), source.begin() + taken));
  }
}

TEST(FixedArrayTest, FillsEveryElementWithTheGivenValue) {
  const Words fits(2, 7);
  EXPECT_EQ(Contents(fits), (std::vector<std::uint64_t>{7, 7}));

  const Words spills(4, 7);
  EXPECT_EQ(Contents(spills), (std::vector<std::uint64_t>{7, 7, 7, 7}));

  const Words zeroed(3);
  EXPECT_EQ(Contents(zeroed), (std::vector<std::uint64_t>{0, 0, 0}));
}

TEST(FixedArrayTest, WritesReachTheElementsInBothRegimes) {
  Words fits(2, 0);
  fits[0] = 1;
  fits[1] = 2;
  EXPECT_EQ(Contents(fits), (std::vector<std::uint64_t>{1, 2}));

  Words spills(4, 0);
  spills[0] = 1;
  spills[3] = 4;
  EXPECT_EQ(Contents(spills), (std::vector<std::uint64_t>{1, 0, 0, 4}));
}

// A copy that shared the spilled buffer would pass every size and element
// check above and fail only here.
TEST(FixedArrayTest, ACopyIsIndependentOfItsSource) {
  Words fits(2, 1);
  Words fits_copy = fits;
  fits_copy[0] = 99;
  EXPECT_EQ(fits[0], 1U);
  EXPECT_EQ(fits_copy[0], 99U);

  Words spills(4, 1);
  Words spills_copy = spills;
  spills_copy[0] = 99;
  EXPECT_EQ(spills[0], 1U);
  EXPECT_EQ(spills_copy[0], 99U);
  EXPECT_EQ(spills_copy.size(), 4U);
}

TEST(FixedArrayTest, AMoveTakesTheContentsAndLeavesTheSourceEmpty) {
  Words spills(4, 5);
  const Words taken = std::move(spills);
  EXPECT_EQ(Contents(taken), (std::vector<std::uint64_t>{5, 5, 5, 5}));
  EXPECT_EQ(spills.size(), 0U);

  Words fits(2, 5);
  const Words taken_inline = std::move(fits);
  EXPECT_EQ(Contents(taken_inline), (std::vector<std::uint64_t>{5, 5}));
  EXPECT_EQ(fits.size(), 0U);
}

TEST(FixedArrayTest, AssignmentReplacesTheSequenceAcrossBothRegimes) {
  Words words(2, 1);

  words = Words(5, 2);
  EXPECT_EQ(Contents(words), (std::vector<std::uint64_t>{2, 2, 2, 2, 2}));

  words = Words(1, 3);
  EXPECT_EQ(Contents(words), (std::vector<std::uint64_t>{3}));

  Words spills(4, 4);
  words = spills;
  spills[0] = 99;
  EXPECT_EQ(Contents(words), (std::vector<std::uint64_t>{4, 4, 4, 4}));

  const Words* itself = &words;
  words = *itself;
  EXPECT_EQ(Contents(words), (std::vector<std::uint64_t>{4, 4, 4, 4}));
}

TEST(FixedArrayTest, EqualityComparesContentsAndLength) {
  EXPECT_EQ(Words(2, 1), Words(2, 1));
  EXPECT_EQ(Words(5, 1), Words(5, 1));
  EXPECT_NE(Words(2, 1), Words(2, 2));
  EXPECT_NE(Words(2, 1), Words(3, 1));
  EXPECT_EQ(Words(), Words());
}

TEST(FixedArrayTest, CarriesAnElementWiderThanAPointer) {
  const std::vector<Pair> source{
      {.left = 1, .right = 2},
      {.left = 3, .right = 4},
      {.left = 5, .right = 6}};

  const Pairs fits(source.begin(), source.begin() + 2);
  EXPECT_EQ(fits.size(), 2U);
  EXPECT_EQ(fits[1], (Pair{.left = 3, .right = 4}));

  Pairs spills(source.begin(), source.end());
  EXPECT_EQ(spills.size(), 3U);
  EXPECT_EQ(spills[2], (Pair{.left = 5, .right = 6}));

  const Pairs copy = spills;
  spills[2] = Pair{.left = 7, .right = 8};
  EXPECT_EQ(copy[2], (Pair{.left = 5, .right = 6}));
}

}  // namespace
