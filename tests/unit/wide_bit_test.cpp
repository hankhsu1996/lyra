#include <gtest/gtest.h>

#include "lyra/common/value_storage.hpp"
#include "lyra/common/wide_bit.hpp"

namespace lyra::common {
namespace {

class WideBitTest : public ::testing::Test {};

// =============================================================================
// Construction Tests
// =============================================================================

TEST_F(WideBitTest, DefaultConstructorCreatesEmpty) {
  WideBit wb;
  EXPECT_EQ(wb.NumWords(), 0);
  EXPECT_TRUE(wb.IsZero());
}

TEST_F(WideBitTest, ConstructWithWordCount) {
  WideBit wb(3);
  EXPECT_EQ(wb.NumWords(), 3);
  EXPECT_EQ(wb.GetWord(0), 0);
  EXPECT_EQ(wb.GetWord(1), 0);
  EXPECT_EQ(wb.GetWord(2), 0);
  EXPECT_TRUE(wb.IsZero());
}

TEST_F(WideBitTest, ConstructFromVector) {
  std::vector<uint64_t> words = {0x1234, 0x5678, 0x9ABC};
  WideBit wb(words);
  EXPECT_EQ(wb.NumWords(), 3);
  EXPECT_EQ(wb.GetWord(0), 0x1234);
  EXPECT_EQ(wb.GetWord(1), 0x5678);
  EXPECT_EQ(wb.GetWord(2), 0x9ABC);
}

TEST_F(WideBitTest, FromUInt64) {
  WideBit wb = WideBit::FromUInt64(0xDEADBEEF, 3);
  EXPECT_EQ(wb.NumWords(), 3);
  EXPECT_EQ(wb.GetWord(0), 0xDEADBEEF);
  EXPECT_EQ(wb.GetWord(1), 0);
  EXPECT_EQ(wb.GetWord(2), 0);
}

TEST_F(WideBitTest, FromBitWidth) {
  // 65 bits requires 2 words
  WideBit wb = WideBit::FromBitWidth(65);
  EXPECT_EQ(wb.NumWords(), 2);

  // 128 bits requires 2 words
  wb = WideBit::FromBitWidth(128);
  EXPECT_EQ(wb.NumWords(), 2);

  // 129 bits requires 3 words
  wb = WideBit::FromBitWidth(129);
  EXPECT_EQ(wb.NumWords(), 3);
}

// =============================================================================
// Bit Access Tests
// =============================================================================

TEST_F(WideBitTest, GetSetBit) {
  WideBit wb(2);  // 128 bits

  // Set bit 0
  wb.SetBit(0, true);
  EXPECT_EQ(wb.GetBit(0), 1);
  EXPECT_EQ(wb.GetWord(0), 1);

  // Set bit 63 (last bit of word 0)
  wb.SetBit(63, true);
  EXPECT_EQ(wb.GetBit(63), 1);
  EXPECT_EQ(wb.GetWord(0), (1ULL << 63) | 1);

  // Set bit 64 (first bit of word 1)
  wb.SetBit(64, true);
  EXPECT_EQ(wb.GetBit(64), 1);
  EXPECT_EQ(wb.GetWord(1), 1);

  // Clear bit 0
  wb.SetBit(0, false);
  EXPECT_EQ(wb.GetBit(0), 0);
}

// =============================================================================
// Masking Tests
// =============================================================================

TEST_F(WideBitTest, MaskToWidth65Bits) {
  WideBit wb(2);
  wb.SetWord(0, ~0ULL);  // All ones
  wb.SetWord(1, ~0ULL);  // All ones

  wb.MaskToWidth(65);

  EXPECT_EQ(wb.GetWord(0), ~0ULL);       // First 64 bits unchanged
  EXPECT_EQ(wb.GetWord(1), 1);           // Only bit 64 remains
}

TEST_F(WideBitTest, MaskToWidth100Bits) {
  WideBit wb(2);
  wb.SetWord(0, ~0ULL);
  wb.SetWord(1, ~0ULL);

  wb.MaskToWidth(100);

  EXPECT_EQ(wb.GetWord(0), ~0ULL);
  // 100 - 64 = 36 bits in word 1
  EXPECT_EQ(wb.GetWord(1), (1ULL << 36) - 1);
}

TEST_F(WideBitTest, MaskToWidth128Bits) {
  WideBit wb(2);
  wb.SetWord(0, ~0ULL);
  wb.SetWord(1, ~0ULL);

  wb.MaskToWidth(128);

  // 128 bits = exactly 2 words, no masking needed
  EXPECT_EQ(wb.GetWord(0), ~0ULL);
  EXPECT_EQ(wb.GetWord(1), ~0ULL);
}

TEST_F(WideBitTest, MaskedToWidthReturnsNewCopy) {
  WideBit wb(2);
  wb.SetWord(0, ~0ULL);
  wb.SetWord(1, ~0ULL);

  WideBit masked = wb.MaskedToWidth(65);

  // Original unchanged
  EXPECT_EQ(wb.GetWord(1), ~0ULL);
  // Copy masked
  EXPECT_EQ(masked.GetWord(1), 1);
}

// =============================================================================
// Bitwise Operation Tests
// =============================================================================

TEST_F(WideBitTest, BitwiseNot) {
  WideBit wb(2);
  wb.SetWord(0, 0xAAAAAAAAAAAAAAAAULL);
  wb.SetWord(1, 0x5555555555555555ULL);

  // BitwiseNot requires bit_width for masking
  WideBit result = wb.BitwiseNot(128);

  EXPECT_EQ(result.GetWord(0), 0x5555555555555555ULL);
  EXPECT_EQ(result.GetWord(1), 0xAAAAAAAAAAAAAAAAULL);
}

TEST_F(WideBitTest, BitwiseAnd) {
  WideBit a(2);
  a.SetWord(0, 0xFF00FF00FF00FF00ULL);
  a.SetWord(1, 0x00FF00FF00FF00FFULL);

  WideBit b(2);
  b.SetWord(0, 0xF0F0F0F0F0F0F0F0ULL);
  b.SetWord(1, 0x0F0F0F0F0F0F0F0FULL);

  WideBit result = a & b;

  EXPECT_EQ(result.GetWord(0), 0xF000F000F000F000ULL);
  EXPECT_EQ(result.GetWord(1), 0x000F000F000F000FULL);
}

TEST_F(WideBitTest, BitwiseOr) {
  WideBit a(2);
  a.SetWord(0, 0xFF00FF00FF00FF00ULL);
  a.SetWord(1, 0x00FF00FF00FF00FFULL);

  WideBit b(2);
  b.SetWord(0, 0x00FF00FF00FF00FFULL);
  b.SetWord(1, 0xFF00FF00FF00FF00ULL);

  WideBit result = a | b;

  EXPECT_EQ(result.GetWord(0), ~0ULL);
  EXPECT_EQ(result.GetWord(1), ~0ULL);
}

TEST_F(WideBitTest, BitwiseXor) {
  WideBit a(2);
  a.SetWord(0, 0xAAAAAAAAAAAAAAAAULL);
  a.SetWord(1, 0x5555555555555555ULL);

  // XOR with itself should give zero
  WideBit result = a ^ a;

  EXPECT_EQ(result.GetWord(0), 0);
  EXPECT_EQ(result.GetWord(1), 0);
}

TEST_F(WideBitTest, BitwiseXorSelfInverse) {
  WideBit a(2);
  a.SetWord(0, 0x123456789ABCDEF0ULL);
  a.SetWord(1, 0xFEDCBA9876543210ULL);

  WideBit b(2);
  b.SetWord(0, 0xDEADBEEFCAFEBABEULL);
  b.SetWord(1, 0x0102030405060708ULL);

  // (a ^ b) ^ b should equal a
  WideBit result = (a ^ b) ^ b;

  EXPECT_EQ(result.GetWord(0), a.GetWord(0));
  EXPECT_EQ(result.GetWord(1), a.GetWord(1));
}

// =============================================================================
// Arithmetic Tests
// =============================================================================

TEST_F(WideBitTest, AdditionNoCarry) {
  WideBit a = WideBit::FromUInt64(100, 2);
  WideBit b = WideBit::FromUInt64(200, 2);

  WideBit result = a.Add(b, 128);

  EXPECT_EQ(result.GetWord(0), 300);
  EXPECT_EQ(result.GetWord(1), 0);
}

TEST_F(WideBitTest, AdditionWithCarry) {
  WideBit a(2);
  a.SetWord(0, ~0ULL);  // Max 64-bit value
  a.SetWord(1, 0);

  WideBit b = WideBit::FromUInt64(1, 2);

  WideBit result = a.Add(b, 128);

  EXPECT_EQ(result.GetWord(0), 0);  // Wrapped around
  EXPECT_EQ(result.GetWord(1), 1);  // Carry propagated
}

TEST_F(WideBitTest, AdditionCarryCascade) {
  // Test cascading carry through all words
  WideBit a(3);
  a.SetWord(0, ~0ULL);
  a.SetWord(1, ~0ULL);
  a.SetWord(2, 0);

  WideBit b = WideBit::FromUInt64(1, 3);

  WideBit result = a.Add(b, 192);

  EXPECT_EQ(result.GetWord(0), 0);
  EXPECT_EQ(result.GetWord(1), 0);
  EXPECT_EQ(result.GetWord(2), 1);
}

TEST_F(WideBitTest, AdditionOverflowWraps) {
  // For 65-bit addition, overflow should wrap
  WideBit a(2);
  a.SetWord(0, ~0ULL);
  a.SetWord(1, 1);  // Bit 64 set

  WideBit b = WideBit::FromUInt64(1, 2);

  WideBit result = a.Add(b, 65);

  // Result should be 0 (wrapped)
  EXPECT_EQ(result.GetWord(0), 0);
  EXPECT_EQ(result.GetWord(1), 0);  // Masked to 1 bit, overflow cleared
}

TEST_F(WideBitTest, SubtractionPositiveResult) {
  WideBit a = WideBit::FromUInt64(300, 2);
  WideBit b = WideBit::FromUInt64(100, 2);

  WideBit result = a.Sub(b, 128);

  EXPECT_EQ(result.GetWord(0), 200);
  EXPECT_EQ(result.GetWord(1), 0);
}

TEST_F(WideBitTest, SubtractionWraparound) {
  // 0 - 1 should wrap to all ones (for 65 bits)
  WideBit a = WideBit::FromUInt64(0, 2);
  WideBit b = WideBit::FromUInt64(1, 2);

  WideBit result = a.Sub(b, 65);

  EXPECT_EQ(result.GetWord(0), ~0ULL);
  EXPECT_EQ(result.GetWord(1), 1);  // Bit 64 set
}

// =============================================================================
// Shift Tests
// =============================================================================

TEST_F(WideBitTest, ShiftLeftSmall) {
  WideBit wb = WideBit::FromUInt64(1, 2);

  WideBit result = wb.ShiftLeft(4, 128);

  EXPECT_EQ(result.GetWord(0), 16);
  EXPECT_EQ(result.GetWord(1), 0);
}

TEST_F(WideBitTest, ShiftLeftWordBoundary) {
  WideBit wb = WideBit::FromUInt64(1, 2);

  WideBit result = wb.ShiftLeft(64, 128);

  EXPECT_EQ(result.GetWord(0), 0);
  EXPECT_EQ(result.GetWord(1), 1);
}

TEST_F(WideBitTest, ShiftLeftCrossWord) {
  WideBit wb = WideBit::FromUInt64(0x8000000000000001ULL, 2);

  WideBit result = wb.ShiftLeft(4, 128);

  // Low bit shifted to bit 4
  // High bit (63) shifted to bit 67
  EXPECT_EQ(result.GetWord(0), 0x0000000000000010ULL);
  EXPECT_EQ(result.GetWord(1), 8);  // Bit 67 - 64 = bit 3 in word 1
}

TEST_F(WideBitTest, ShiftLeftByZero) {
  WideBit wb = WideBit::FromUInt64(0x1234, 2);

  WideBit result = wb.ShiftLeft(0, 128);

  EXPECT_EQ(result.GetWord(0), 0x1234);
  EXPECT_EQ(result.GetWord(1), 0);
}

TEST_F(WideBitTest, ShiftLeftByWidth) {
  WideBit wb = WideBit::FromUInt64(0x1234, 2);

  WideBit result = wb.ShiftLeft(128, 128);

  EXPECT_TRUE(result.IsZero());
}

TEST_F(WideBitTest, ShiftLeftByMoreThanWidth) {
  WideBit wb = WideBit::FromUInt64(0x1234, 2);

  WideBit result = wb.ShiftLeft(200, 128);

  EXPECT_TRUE(result.IsZero());
}

TEST_F(WideBitTest, ShiftRightLogicalSmall) {
  WideBit wb = WideBit::FromUInt64(0x100, 2);

  WideBit result = wb.ShiftRightLogical(4);

  EXPECT_EQ(result.GetWord(0), 0x10);
  EXPECT_EQ(result.GetWord(1), 0);
}

TEST_F(WideBitTest, ShiftRightLogicalWordBoundary) {
  WideBit wb(2);
  wb.SetWord(0, 0);
  wb.SetWord(1, 1);  // Bit 64

  WideBit result = wb.ShiftRightLogical(64);

  EXPECT_EQ(result.GetWord(0), 1);
  EXPECT_EQ(result.GetWord(1), 0);
}

TEST_F(WideBitTest, ShiftRightLogicalCrossWord) {
  WideBit wb(2);
  wb.SetWord(0, 0);
  wb.SetWord(1, 0x10);  // Bit 68 set (0x10 = bit 4 in word 1)

  WideBit result = wb.ShiftRightLogical(4);

  // Bit 68 should move to bit 64 (bit 0 in word 1)
  EXPECT_EQ(result.GetWord(0), 0);
  EXPECT_EQ(result.GetWord(1), 1);
}

TEST_F(WideBitTest, ShiftRightArithmeticPositive) {
  // Positive number (sign bit = 0)
  WideBit wb = WideBit::FromUInt64(0x100, 2);

  WideBit result = wb.ShiftRightArithmetic(4, 128);

  EXPECT_EQ(result.GetWord(0), 0x10);
  EXPECT_EQ(result.GetWord(1), 0);
}

TEST_F(WideBitTest, ShiftRightArithmeticNegative) {
  // Negative number (sign bit = 1 for 65-bit value)
  WideBit wb(2);
  wb.SetWord(0, 0);
  wb.SetWord(1, 1);  // Bit 64 set (sign bit for 65-bit value)

  WideBit result = wb.ShiftRightArithmetic(1, 65);

  // Should sign-extend
  EXPECT_EQ(result.GetBit(64), 1);  // Sign bit preserved
  EXPECT_EQ(result.GetBit(63), 1);  // Sign extended
}

// =============================================================================
// Comparison Tests
// =============================================================================

TEST_F(WideBitTest, EqualitySameValue) {
  WideBit a(2);
  a.SetWord(0, 0x1234);
  a.SetWord(1, 0x5678);

  WideBit b(2);
  b.SetWord(0, 0x1234);
  b.SetWord(1, 0x5678);

  EXPECT_TRUE(a == b);
  EXPECT_FALSE(a != b);
}

TEST_F(WideBitTest, EqualityDifferentValue) {
  WideBit a = WideBit::FromUInt64(100, 2);
  WideBit b = WideBit::FromUInt64(200, 2);

  EXPECT_FALSE(a == b);
  EXPECT_TRUE(a != b);
}

TEST_F(WideBitTest, EqualityDifferentSize) {
  WideBit a = WideBit::FromUInt64(100, 2);
  WideBit b = WideBit::FromUInt64(100, 3);

  EXPECT_FALSE(a == b);
}

// =============================================================================
// Utility Tests
// =============================================================================

TEST_F(WideBitTest, IsZero) {
  WideBit wb(2);
  EXPECT_TRUE(wb.IsZero());

  wb.SetWord(0, 1);
  EXPECT_FALSE(wb.IsZero());
}

TEST_F(WideBitTest, BoolConversion) {
  WideBit wb(2);
  EXPECT_FALSE(static_cast<bool>(wb));

  wb.SetWord(1, 1);
  EXPECT_TRUE(static_cast<bool>(wb));
}

TEST_F(WideBitTest, ToHexString) {
  WideBit wb(2);
  wb.SetWord(0, 0xDEADBEEF);
  wb.SetWord(1, 0x12345678);

  std::string hex = wb.ToHexString();

  // word 1 (0x12345678) printed first, then word 0 (0xDEADBEEF) with 16 hex digits
  EXPECT_EQ(hex, "0x1234567800000000deadbeef");
}

TEST_F(WideBitTest, ToHexStringZero) {
  WideBit wb(2);

  EXPECT_EQ(wb.ToHexString(), "0x0");
}

TEST_F(WideBitTest, HashConsistency) {
  WideBit a(2);
  a.SetWord(0, 0x1234);
  a.SetWord(1, 0x5678);

  WideBit b(2);
  b.SetWord(0, 0x1234);
  b.SetWord(1, 0x5678);

  EXPECT_EQ(a.Hash(), b.Hash());
}

// =============================================================================
// ValueStorage Integration Tests
// =============================================================================

TEST_F(WideBitTest, ValueStorageRoundTrip) {
  WideBit original(2);
  original.SetWord(0, 0xDEADBEEF);
  original.SetWord(1, 0x12345678);

  ValueStorage storage(original);

  EXPECT_TRUE(storage.IsWideBit());
  EXPECT_FALSE(storage.IsInt64());

  const WideBit& retrieved = storage.AsWideBit();
  EXPECT_EQ(retrieved, original);
}

TEST_F(WideBitTest, ValueStorageToString) {
  WideBit wb(2);
  wb.SetWord(0, 0xFF);
  wb.SetWord(1, 0);

  ValueStorage storage(wb);

  EXPECT_EQ(storage.ToString(), "0xff");
}

TEST_F(WideBitTest, ValueStorageHash) {
  WideBit wb(2);
  wb.SetWord(0, 0x1234);
  wb.SetWord(1, 0x5678);

  ValueStorage storage(wb);

  // Just verify it doesn't crash and returns something
  std::size_t h = storage.Hash();
  EXPECT_NE(h, 0);  // Unlikely to be 0 for this value
}

}  // namespace
}  // namespace lyra::common
