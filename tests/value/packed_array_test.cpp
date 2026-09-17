#include "lyra/value/packed_array.hpp"

#include <array>
#include <cstdint>
#include <gtest/gtest.h>

#include "lyra/value/packed_type.hpp"
#include "lyra/value/slice_selector.hpp"

namespace lyra::value {
namespace {

// What an integral value carries: how many bits it has, whether they are read
// as signed, whether a position may hold x or z, and the bits themselves in one
// plane per state domain. Nothing about how a declaration divides those bits.
static_assert(sizeof(PackedArray) == 64);

auto Shape(std::initializer_list<PackedRange> dims, bool is_four_state = false)
    -> PackedType {
  return PackedType{
      std::span<const PackedRange>{dims.begin(), dims.size()}, false,
      is_four_state};
}

TEST(PackedArrayTest, AValueOfOneDeclarationReadsAsAnother) {
  // `bit [7:0]` and `bit [3:0][1:0]` are the same eight bits, and the two
  // declarations divide them differently. The value holds the bits; which
  // division applies is stated where the position is named, so one value
  // answers both -- which is the whole of what leaving the stack out means.
  const PackedArray bits = PackedArray::FromInt(0xD2, 8U, false, false);
  const PackedType flat = Shape({PackedRange{.left = 7, .right = 0}});
  const PackedType nested = Shape(
      {PackedRange{.left = 3, .right = 0}, PackedRange{.left = 1, .right = 0}});

  const PackedArray one_bit = bits.Element(PackedArray::Int(6), flat);
  EXPECT_EQ(one_bit.BitWidth(), 1U);
  EXPECT_EQ(one_bit.ToInt64(), 1);

  const PackedArray one_pair = bits.Element(PackedArray::Int(3), nested);
  EXPECT_EQ(one_pair.BitWidth(), 2U);
  EXPECT_EQ(one_pair.ToInt64(), 3);
}

TEST(PackedArrayTest, ASliceTakesItsCoordinatesFromTheReceiversShape) {
  const PackedArray bits = PackedArray::FromInt(0xD2, 8U, false, false);
  const PackedType flat = Shape({PackedRange{.left = 7, .right = 0}});
  const PackedType nested = Shape(
      {PackedRange{.left = 3, .right = 0}, PackedRange{.left = 1, .right = 0}});

  // Bits 5 down to 2 of 1101_0010.
  const PackedArray four_bits = bits.Slice(
      PackedArray::Int(5), PackedArray::Int(2),
      PackedArray::Int(static_cast<std::int32_t>(SliceForm::kConstant)), flat);
  EXPECT_EQ(four_bits.BitWidth(), 4U);
  EXPECT_EQ(four_bits.ToInt64(), 0x4);

  // The same bounds over the nested declaration name pairs, not bits.
  const PackedArray two_pairs = bits.Slice(
      PackedArray::Int(2), PackedArray::Int(1),
      PackedArray::Int(static_cast<std::int32_t>(SliceForm::kConstant)),
      nested);
  EXPECT_EQ(two_pairs.BitWidth(), 4U);
  EXPECT_EQ(two_pairs.ToInt64(), 0x4);
}

TEST(PackedArrayTest, AFourStateDeclarationReadsAsUnknownUntilDriven) {
  // LRM Table 6-7. The default belongs to the declaration, so an operation's
  // result -- which writes every bit itself -- does not pay for it.
  const PackedArray declared{8U, false, true};
  EXPECT_TRUE(declared.HasUnknown());

  const PackedArray driven = PackedArray::FromInt(1, 8U, false, true);
  EXPECT_FALSE(driven.HasUnknown());
  EXPECT_FALSE((driven + driven).HasUnknown());
  EXPECT_FALSE((driven & driven).HasUnknown());
  EXPECT_FALSE(driven.ShiftLeft(PackedArray::Int(1)).HasUnknown());
}

TEST(PackedArrayTest, AStoreIsCheckedAgainstWhatTheCellHolds) {
  const PackedArray eight = PackedArray::FromInt(0, 8U, false, false);
  EXPECT_TRUE(
      eight.SameRepresentation(PackedArray::FromInt(1, 8U, false, false)));
  EXPECT_FALSE(
      eight.SameRepresentation(PackedArray::FromInt(1, 16U, false, false)));
  EXPECT_FALSE(
      eight.SameRepresentation(PackedArray::FromInt(1, 8U, true, false)));
  EXPECT_FALSE(
      eight.SameRepresentation(PackedArray::FromInt(1, 8U, false, true)));
}

TEST(PackedArrayTest, AWideValueKeepsItsBitsAcrossACopy) {
  const std::array<std::uint64_t, 3> words = {
      0x1111'2222'3333'4444ULL, 0x5555'6666'7777'8888ULL,
      0x0000'0000'0000'00AAULL};
  const PackedArray wide =
      PackedArray::FromWords(words, {}, 136U, false, false);
  const PackedArray copy = wide;
  EXPECT_TRUE(copy.IsBitIdentical(wide));
  EXPECT_EQ(copy.ValueWords()[1], 0x5555'6666'7777'8888ULL);
}

}  // namespace
}  // namespace lyra::value
