#include "lyra/base/segmented_vector.hpp"

#include <cstddef>
#include <gtest/gtest.h>
#include <optional>
#include <utility>
#include <vector>

namespace {

using lyra::base::SegmentedVector;

// Takes everything a sequence holds, as an owner does that hands its pool on
// and fills it again.
auto TakeAll(SegmentedVector<int>& source) -> SegmentedVector<int> {
  return std::move(source);
}

// Every element keeps its address however far the sequence grows, in a copy as
// in the original and when it grows by default values. The counts cross many
// segment boundaries, so an element read through an address taken before the
// growth is the element itself rather than whatever a reallocation left there.
TEST(SegmentedVectorTest, AnElementStaysWhereItIsWhileTheSequenceGrows) {
  constexpr std::size_t kCount = 5000;
  SegmentedVector<int> sequence;
  std::vector<const int*> addresses;
  for (std::size_t i = 0; i < kCount; ++i) {
    sequence.push_back(static_cast<int>(i));
    addresses.push_back(&sequence[i]);
  }
  for (std::size_t i = 0; i < kCount; ++i) {
    EXPECT_EQ(addresses[i], &sequence[i]);
    EXPECT_EQ(*addresses[i], static_cast<int>(i));
  }

  SegmentedVector<int> copy = sequence;
  const int* last = &copy[kCount - 1];
  for (std::size_t i = 0; i < kCount; ++i) {
    copy.push_back(0);
  }
  EXPECT_EQ(last, &copy[kCount - 1]);
  EXPECT_EQ(*last, static_cast<int>(kCount - 1));

  SegmentedVector<std::optional<int>> table;
  table.GrowTo(3);
  table[2] = 7;
  const std::optional<int>* written = &table[2];
  table.GrowTo(kCount);
  EXPECT_EQ(written, &table[2]);
  EXPECT_EQ(table.size(), kCount);
  EXPECT_EQ(table[2], 7);
  EXPECT_FALSE(table[kCount - 1].has_value());
}

// Reads back what was appended, by index and by walking it in order, and two
// sequences are equal exactly when they hold equal values in the same order.
TEST(SegmentedVectorTest, HoldsWhatWasAppendedInOrder) {
  SegmentedVector<int> empty;
  EXPECT_TRUE(empty.empty());
  EXPECT_EQ(empty.begin(), empty.end());

  SegmentedVector<int> sequence;
  std::vector<int> expected;
  for (int i = 0; i < 100; ++i) {
    sequence.push_back(i * 3);
    expected.push_back(i * 3);
  }
  EXPECT_EQ(sequence.size(), expected.size());
  std::vector<int> walked;
  for (const int value : sequence) {
    walked.push_back(value);
  }
  EXPECT_EQ(walked, expected);

  SegmentedVector<int> same = sequence;
  EXPECT_EQ(same, sequence);
  same[50] = -1;
  EXPECT_NE(same, sequence);
  SegmentedVector<int> shorter;
  shorter.push_back(0);
  EXPECT_NE(shorter, sequence);
}

// Moving behaves as a `std::vector`'s move does: what is moved to holds the
// elements where they were, an iterator taken before the move still walks
// them, and what is moved from is empty and can be filled again.
TEST(SegmentedVectorTest, MovesAsAVectorDoes) {
  SegmentedVector<int> source;
  for (int i = 0; i < 100; ++i) {
    source.push_back(i);
  }
  const int* element = &source[60];
  const auto walk = source.begin();

  SegmentedVector<int> moved = std::move(source);
  EXPECT_EQ(element, &moved[60]);
  EXPECT_EQ(*walk, 0);
  EXPECT_EQ(moved.size(), 100U);

  SegmentedVector<int> assigned;
  assigned.push_back(1);
  assigned = std::move(moved);
  EXPECT_EQ(element, &assigned[60]);
  EXPECT_EQ(assigned.size(), 100U);

  SegmentedVector<int> refilled;
  for (int i = 0; i < 100; ++i) {
    refilled.push_back(i);
  }
  const SegmentedVector<int> taken = TakeAll(refilled);
  EXPECT_TRUE(refilled.empty());
  for (int i = 0; i < 100; ++i) {
    refilled.push_back(-i);
  }
  EXPECT_EQ(refilled.size(), 100U);
  EXPECT_EQ(refilled[99], -99);
  EXPECT_EQ(taken[99], 99);
}

}  // namespace
