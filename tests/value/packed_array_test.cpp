#include "lyra/value/packed_array.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <gtest/gtest.h>
#include <span>
#include <utility>

#include "lyra/value/packed_type.hpp"
#include "lyra/value/slice_selector.hpp"

namespace lyra::value {
namespace {

// What an integral value carries: how many bits it has, whether they are read
// as signed, whether a position may hold x or z, and the bits themselves in one
// plane per state domain, the planes sharing one run of words. Nothing about
// how a declaration divides those bits.
static_assert(sizeof(PackedArray) == 48);

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

// LRM 11.5.1. The positions that compose differently are where the run sits
// relative to a word: inside one, across a boundary, wider than one, and
// starting where the value does not reach.
TEST(PackedArrayTest, ARunOfBitsIsReadWhereverItStarts) {
  // 1101_0010
  const PackedArray byte = PackedArray::FromInt(0xD2, 8U, true, false);

  const PackedArray middle = byte.ExtractBits(PackedArray::Int(2), 4U);
  EXPECT_EQ(middle.BitWidth(), 4U);
  EXPECT_EQ(middle.ToInt64(), 0x4);

  const std::array<std::uint64_t, 3> words = {
      0x1111'2222'3333'4444ULL, 0x5555'6666'7777'8888ULL,
      0x0000'0000'0000'00AAULL};
  const PackedArray wide =
      PackedArray::FromWords(words, {}, 136U, false, false);

  const PackedArray across = wide.ExtractBits(PackedArray::Int(32), 64U);
  EXPECT_EQ(across.ValueWords()[0], 0x7777'8888'1111'2222ULL);

  const PackedArray spanning = wide.ExtractBits(PackedArray::Int(60), 72U);
  EXPECT_EQ(spanning.BitWidth(), 72U);
  EXPECT_EQ(spanning.ValueWords()[0], 0x5556'6667'7778'8881ULL);
  EXPECT_EQ(spanning.ValueWords()[1], 0x0000'0000'0000'00A5ULL);

  // A position the value does not reach reads as zero on a two-state value.
  EXPECT_EQ(byte.ExtractBits(PackedArray::Int(6), 4U).ToInt64(), 0x3);
  EXPECT_EQ(byte.ExtractBits(PackedArray::Int(-2), 4U).ToInt64(), 0x8);
  EXPECT_EQ(byte.ExtractBits(PackedArray::Int(100), 4U).ToInt64(), 0);
}

TEST(PackedArrayTest, ARunOfBitsCarriesTheUnknownItCrosses) {
  // 1101_0010, with positions 1 and 6 holding x.
  const std::array<std::uint64_t, 1> value = {0xD2ULL};
  const std::array<std::uint64_t, 1> unknown = {0x42ULL};
  const PackedArray byte =
      PackedArray::FromWords(value, unknown, 8U, false, true);

  const PackedArray run = byte.ExtractBits(PackedArray::Int(1), 6U);
  EXPECT_EQ(run.UnknownWords()[0], 0x21ULL);

  // A position the value does not reach is x on a four-state value, so a run
  // that leaves the value comes back partly unknown.
  const PackedArray over = byte.ExtractBits(PackedArray::Int(6), 4U);
  EXPECT_EQ(over.ValueWords()[0], 0xFULL);
  EXPECT_EQ(over.UnknownWords()[0], 0xDULL);

  // A start that is itself unknown puts the whole run out of reach.
  const PackedArray nowhere =
      byte.ExtractBits(PackedArray{4U, false, true}, 4U);
  EXPECT_EQ(nowhere.UnknownWords()[0], 0xFULL);

  // Both planes cross a word boundary together, which is the shape a wide
  // four-state value actually meets.
  const std::array<std::uint64_t, 3> wide_value = {
      0x1111'2222'3333'4444ULL, 0x5555'6666'7777'8888ULL,
      0x0000'0000'0000'00AAULL};
  const std::array<std::uint64_t, 3> wide_unknown = {
      0x0F0F'0F0F'0000'0000ULL, 0x0000'0000'A5A5'A5A5ULL,
      0x0000'0000'0000'0003ULL};
  const PackedArray wide =
      PackedArray::FromWords(wide_value, wide_unknown, 136U, false, true);

  const PackedArray across = wide.ExtractBits(PackedArray::Int(32), 64U);
  EXPECT_EQ(across.ValueWords()[0], 0x7777'8888'1111'2222ULL);
  EXPECT_EQ(across.UnknownWords()[0], 0xA5A5'A5A5'0F0F'0F0FULL);
}

TEST(PackedArrayTest, ARunOfBitsIsWrittenWhereverItStarts) {
  // The inverse of the read above: the same run written back at the same
  // position reproduces the words it was taken from.
  const std::array<std::uint64_t, 2> run_words = {
      0x5556'6667'7778'8881ULL, 0x0000'0000'0000'00A5ULL};
  PackedArray target{136U, false, false};
  target.AssignSlice(
      PackedArray::Int(60), 72U,
      PackedArray::FromWords(run_words, {}, 72U, false, false));
  EXPECT_EQ(target.ValueWords()[0], 0x1000'0000'0000'0000ULL);
  EXPECT_EQ(target.ValueWords()[1], 0x5555'6666'7777'8888ULL);
  EXPECT_EQ(target.ValueWords()[2], 0x0000'0000'0000'000AULL);

  // Only the overlap lands, and what the value holds elsewhere stays.
  PackedArray below = PackedArray::FromInt(0xFF, 8U, false, false);
  below.AssignSlice(
      PackedArray::Int(-2), 4U, PackedArray::FromInt(0x0, 4U, false, false));
  EXPECT_EQ(below.ToInt64(), 0xFC);

  PackedArray above = PackedArray::FromInt(0x00, 8U, false, false);
  above.AssignSlice(
      PackedArray::Int(6), 4U, PackedArray::FromInt(0xF, 4U, false, false));
  EXPECT_EQ(above.ToInt64(), 0xC0);

  // A start that is itself unknown writes nothing at all.
  PackedArray untouched = PackedArray::FromInt(0xAA, 8U, false, false);
  untouched.AssignSlice(
      PackedArray{4U, false, true}, 4U,
      PackedArray::FromInt(0xF, 4U, false, false));
  EXPECT_EQ(untouched.ToInt64(), 0xAA);
}

TEST(PackedArrayTest, AWrittenRunSettlesTheUnknownItLandsOn) {
  // A two-state value written into four-state storage clears the unknown at
  // the positions it covers; it carries none of its own (LRM 7.2.1).
  PackedArray cell{8U, false, true};
  cell.AssignSlice(
      PackedArray::Int(2), 4U, PackedArray::FromInt(0x5, 4U, false, false));
  EXPECT_EQ(cell.ValueWords()[0], 0xD7ULL);
  EXPECT_EQ(cell.UnknownWords()[0], 0xC3ULL);

  // A four-state value carries its own unknown in.
  PackedArray sink = PackedArray::FromInt(0, 8U, false, true);
  sink.AssignSlice(PackedArray::Int(2), 4U, PackedArray{4U, false, true});
  EXPECT_EQ(sink.ValueWords()[0], 0x3CULL);
  EXPECT_EQ(sink.UnknownWords()[0], 0x3CULL);
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

// A value that fits one word keeps both of its planes in place and a wider one
// keeps them elsewhere, so the positions that compose differently are the two
// sides of that line and a value moving across it. Each value's planes hold
// different words, so a plane read from the other's position shows.
TEST(PackedArrayTest, BothPlanesSurviveEveryWayAValueMoves) {
  const std::array<std::uint64_t, 1> narrow_value = {0x8000'0000'0000'0001ULL};
  const std::array<std::uint64_t, 1> narrow_unknown = {
      0x0000'0000'0000'0102ULL};
  const std::array<std::uint64_t, 2> wide_value = {
      0x1111'2222'3333'4444ULL, 0x1ULL};
  const std::array<std::uint64_t, 2> wide_unknown = {
      0x0F0F'0F0F'0000'0000ULL, 0x0ULL};

  const PackedArray narrow =
      PackedArray::FromWords(narrow_value, narrow_unknown, 64U, false, true);
  const PackedArray wide =
      PackedArray::FromWords(wide_value, wide_unknown, 65U, false, true);

  auto holds = [](const PackedArray& v, std::span<const std::uint64_t> value,
                  std::span<const std::uint64_t> unknown) {
    return std::ranges::equal(v.ValueWords(), value) &&
           std::ranges::equal(v.UnknownWords(), unknown);
  };
  EXPECT_TRUE(holds(narrow, narrow_value, narrow_unknown));
  EXPECT_TRUE(holds(wide, wide_value, wide_unknown));

  const PackedArray narrow_copy = narrow;
  const PackedArray wide_copy = wide;
  EXPECT_TRUE(holds(narrow_copy, narrow_value, narrow_unknown));
  EXPECT_TRUE(holds(wide_copy, wide_value, wide_unknown));

  PackedArray crossing = narrow;
  crossing = wide;
  EXPECT_TRUE(holds(crossing, wide_value, wide_unknown));
  crossing = narrow;
  EXPECT_TRUE(holds(crossing, narrow_value, narrow_unknown));

  PackedArray moved_wide = wide;
  const PackedArray taken_wide = std::move(moved_wide);
  EXPECT_TRUE(holds(taken_wide, wide_value, wide_unknown));
  PackedArray moved_narrow = narrow;
  crossing = std::move(moved_narrow);
  EXPECT_TRUE(holds(crossing, narrow_value, narrow_unknown));

  // A two-state value has no second plane on either side of the line.
  EXPECT_TRUE(
      PackedArray::FromInt(-1, 64U, true, false).UnknownWords().empty());
  EXPECT_TRUE(
      PackedArray::FromInt(-1, 65U, true, false).UnknownWords().empty());
}

}  // namespace
}  // namespace lyra::value
