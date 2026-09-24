#include "lyra/value/packed_array.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <gtest/gtest.h>
#include <span>
#include <utility>

#include "lyra/value/position.hpp"

namespace lyra::value {
namespace {

// What an integral value carries: how many bits it has, whether they are read
// as signed, whether a position may hold x or z, and the bits themselves in one
// plane per state domain, the planes sharing one run of words. Nothing about
// how a declaration divides those bits.
static_assert(sizeof(PackedArray) == 48);

// A position is the one integer a select's operand stands for, or nothing: an
// x or z bit names no position (LRM 11.5.1), and neither does a magnitude past
// every value, whatever width and signedness carried it there.
TEST(PackedArrayTest, APositionIsReadTheSameWayWhateverCarriesIt) {
  EXPECT_EQ(ReadPosition(PackedArray::Int(5)), 5);
  EXPECT_EQ(ReadPosition(PackedArray::Int(-3)), -3);
  EXPECT_EQ(
      ReadPosition(PackedArray::FromInt(0xFF, 8U, false, false)).value_or(0),
      255);
  EXPECT_EQ(ReadPosition(PackedArray::FromInt(-1, 8U, true, true)), -1);

  EXPECT_FALSE(ReadPosition(PackedArray{8U, false, true}).has_value());
  EXPECT_FALSE(
      ReadPosition(PackedArray::FromInt(kPositionLimit + 1, 64U, true, false))
          .has_value());
  // An unsigned 64-bit value past the signed range is huge, not negative.
  EXPECT_FALSE(
      ReadPosition(PackedArray::FromInt(-1, 64U, false, false)).has_value());

  // Past 64 bits, the words above the first decide whether the value fits.
  const std::array<std::uint64_t, 2> small = {7ULL, 0ULL};
  const std::array<std::uint64_t, 2> large = {7ULL, 1ULL};
  const std::array<std::uint64_t, 2> minus_two = {~1ULL, 0x1ULL};
  EXPECT_EQ(
      ReadPosition(PackedArray::FromWords(small, {}, 65U, false, false)), 7);
  EXPECT_FALSE(
      ReadPosition(PackedArray::FromWords(large, {}, 65U, false, false))
          .has_value());
  EXPECT_EQ(
      ReadPosition(PackedArray::FromWords(minus_two, {}, 65U, true, false)),
      -2);

  // The position type an index is brought to for arithmetic says the same.
  EXPECT_EQ(PackedArray::ToPosition(PackedArray::Int(9)).ToInt64(), 9);
  EXPECT_TRUE(
      PackedArray::ToPosition(PackedArray{8U, false, true}).HasUnknown());
}

// LRM 6.11.2 / 6.11.3 / 6.12.1: what a conversion between two values of one
// word keeps. The positions that behave differently are a signed source
// widening, an unsigned one widening, a sign bit that is unknown, a four-state
// value reaching two states, and a narrowing.
TEST(PackedArrayTest, AOneWordConversionKeepsWhatTheRulesKeep) {
  const PackedArray minus_128 = PackedArray::FromInt(-128, 8U, true, false);
  EXPECT_EQ(
      PackedArray::ConvertFrom(minus_128, 16U, true, false).ValueWords()[0],
      0xFF80ULL);
  const PackedArray unsigned_128 = PackedArray::FromInt(0x80, 8U, false, false);
  EXPECT_EQ(
      PackedArray::ConvertFrom(unsigned_128, 16U, false, false).ValueWords()[0],
      0x0080ULL);

  // 4'bx010, signed: an x sign fills with x into four states and with the 0 it
  // becomes into two.
  const std::array<std::uint64_t, 1> value = {0xAULL};
  const std::array<std::uint64_t, 1> unknown = {0x8ULL};
  const PackedArray x_sign =
      PackedArray::FromWords(value, unknown, 4U, true, true);
  const PackedArray four = PackedArray::ConvertFrom(x_sign, 8U, true, true);
  EXPECT_EQ(four.ValueWords()[0], 0xFAULL);
  EXPECT_EQ(four.UnknownWords()[0], 0xF8ULL);
  const PackedArray two = PackedArray::ConvertFrom(x_sign, 8U, true, false);
  EXPECT_EQ(two.ValueWords()[0], 0x02ULL);
  EXPECT_TRUE(two.UnknownWords().empty());

  const PackedArray wide = PackedArray::FromInt(0x1234, 16U, false, false);
  EXPECT_EQ(
      PackedArray::ConvertFrom(wide, 8U, false, false).ValueWords()[0],
      0x34ULL);
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

  const PackedArray middle = byte.Slice(PackedArray::Int(2), 4U);
  EXPECT_EQ(middle.BitWidth(), 4U);
  EXPECT_EQ(middle.ToInt64(), 0x4);

  const std::array<std::uint64_t, 3> words = {
      0x1111'2222'3333'4444ULL, 0x5555'6666'7777'8888ULL,
      0x0000'0000'0000'00AAULL};
  const PackedArray wide =
      PackedArray::FromWords(words, {}, 136U, false, false);

  const PackedArray across = wide.Slice(PackedArray::Int(32), 64U);
  EXPECT_EQ(across.ValueWords()[0], 0x7777'8888'1111'2222ULL);

  const PackedArray spanning = wide.Slice(PackedArray::Int(60), 72U);
  EXPECT_EQ(spanning.BitWidth(), 72U);
  EXPECT_EQ(spanning.ValueWords()[0], 0x5556'6667'7778'8881ULL);
  EXPECT_EQ(spanning.ValueWords()[1], 0x0000'0000'0000'00A5ULL);

  // A position the value does not reach reads as zero on a two-state value.
  EXPECT_EQ(byte.Slice(PackedArray::Int(6), 4U).ToInt64(), 0x3);
  EXPECT_EQ(byte.Slice(PackedArray::Int(-2), 4U).ToInt64(), 0x8);
  EXPECT_EQ(byte.Slice(PackedArray::Int(100), 4U).ToInt64(), 0);
}

TEST(PackedArrayTest, ARunOfBitsCarriesTheUnknownItCrosses) {
  // 1101_0010, with positions 1 and 6 holding x.
  const std::array<std::uint64_t, 1> value = {0xD2ULL};
  const std::array<std::uint64_t, 1> unknown = {0x42ULL};
  const PackedArray byte =
      PackedArray::FromWords(value, unknown, 8U, false, true);

  const PackedArray run = byte.Slice(PackedArray::Int(1), 6U);
  EXPECT_EQ(run.UnknownWords()[0], 0x21ULL);

  // A position the value does not reach is x on a four-state value, so a run
  // that leaves the value comes back partly unknown.
  const PackedArray over = byte.Slice(PackedArray::Int(6), 4U);
  EXPECT_EQ(over.ValueWords()[0], 0xFULL);
  EXPECT_EQ(over.UnknownWords()[0], 0xDULL);

  // A start that is itself unknown puts the whole run out of reach.
  const PackedArray nowhere = byte.Slice(PackedArray{4U, false, true}, 4U);
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

  const PackedArray across = wide.Slice(PackedArray::Int(32), 64U);
  EXPECT_EQ(across.ValueWords()[0], 0x7777'8888'1111'2222ULL);
  EXPECT_EQ(across.UnknownWords()[0], 0xA5A5'A5A5'0F0F'0F0FULL);
}

TEST(PackedArrayTest, ARunOfBitsIsWrittenWhereverItStarts) {
  // The inverse of the read above: the same run written back at the same
  // position reproduces the words it was taken from.
  const std::array<std::uint64_t, 2> run_words = {
      0x5556'6667'7778'8881ULL, 0x0000'0000'0000'00A5ULL};
  PackedArray target{136U, false, false};
  target.SliceRef(PackedArray::Int(60), 72) =
      PackedArray::FromWords(run_words, {}, 72U, false, false);
  EXPECT_EQ(target.ValueWords()[0], 0x1000'0000'0000'0000ULL);
  EXPECT_EQ(target.ValueWords()[1], 0x5555'6666'7777'8888ULL);
  EXPECT_EQ(target.ValueWords()[2], 0x0000'0000'0000'000AULL);

  // Only the overlap lands, and what the value holds elsewhere stays.
  PackedArray below = PackedArray::FromInt(0xFF, 8U, false, false);
  below.SliceRef(PackedArray::Int(-2), 4) =
      PackedArray::FromInt(0x0, 4U, false, false);
  EXPECT_EQ(below.ToInt64(), 0xFC);

  PackedArray above = PackedArray::FromInt(0x00, 8U, false, false);
  above.SliceRef(PackedArray::Int(6), 4) =
      PackedArray::FromInt(0xF, 4U, false, false);
  EXPECT_EQ(above.ToInt64(), 0xC0);

  // A start that is itself unknown writes nothing at all.
  PackedArray untouched = PackedArray::FromInt(0xAA, 8U, false, false);
  untouched.SliceRef(PackedArray{4U, false, true}, 4) =
      PackedArray::FromInt(0xF, 4U, false, false);
  EXPECT_EQ(untouched.ToInt64(), 0xAA);
}

TEST(PackedArrayTest, AWrittenRunSettlesTheUnknownItLandsOn) {
  // A two-state value written into four-state storage clears the unknown at
  // the positions it covers; it carries none of its own (LRM 7.2.1).
  PackedArray cell{8U, false, true};
  cell.SliceRef(PackedArray::Int(2), 4) =
      PackedArray::FromInt(0x5, 4U, false, false);
  EXPECT_EQ(cell.ValueWords()[0], 0xD7ULL);
  EXPECT_EQ(cell.UnknownWords()[0], 0xC3ULL);

  // A four-state value carries its own unknown in.
  PackedArray sink = PackedArray::FromInt(0, 8U, false, true);
  sink.SliceRef(PackedArray::Int(2), 4) = PackedArray{4U, false, true};
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
