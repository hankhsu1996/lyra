#include "lyra/value/integral_operation.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <gtest/gtest.h>
#include <span>
#include <variant>

#include "lyra/support/integral_operation.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {
namespace {

using support::IntegralOp;

// One value of a single word per plane, as an operation is handed it. A
// two-state value has no unknown plane.
struct Word {
  std::uint64_t value = 0;
  std::uint64_t unknown = 0;
  std::uint64_t width = 0;
  bool four_state = false;
  Signedness signedness = Signedness::kUnsigned;

  [[nodiscard]] auto Read() const -> ConstPlanes {
    return ConstPlanes{
        .value = {&value, 1},
        .unknown = four_state ? std::span<const std::uint64_t>{&unknown, 1}
                              : std::span<const std::uint64_t>{}};
  }
  [[nodiscard]] auto Bits() const -> IntegralOperand {
    return BitsOperand{.planes = Read(), .width = width};
  }
  [[nodiscard]] auto Number() const -> IntegralOperand {
    return NumberOperand{
        .planes = Read(), .width = width, .signedness = signedness};
  }
};

auto Two(std::uint64_t value, std::uint64_t width) -> Word {
  return Word{.value = value, .width = width};
}
auto Four(std::uint64_t value, std::uint64_t unknown, std::uint64_t width)
    -> Word {
  return Word{
      .value = value, .unknown = unknown, .width = width, .four_state = true};
}
auto Signed(Word word) -> Word {
  word.signedness = Signedness::kSigned;
  return word;
}

// The planes an operation answers with, at `width` bits, with or without an
// unknown plane.
struct Answer {
  std::uint64_t value = 0;
  std::uint64_t unknown = 0;
};

template <std::size_t N>
auto Apply(
    IntegralOp op, const std::array<IntegralOperand, N>& operands,
    std::uint64_t width, bool four_state) -> Answer {
  Answer answer;
  const MachineAnswer machine = ApplyIntegralOperation(
      op, operands,
      Planes{
          .value = {&answer.value, 1},
          .unknown = four_state ? std::span<std::uint64_t>{&answer.unknown, 1}
                                : std::span<std::uint64_t>{}},
      width);
  EXPECT_TRUE(std::holds_alternative<std::monostate>(machine));
  return answer;
}

// LRM 6.11.2: an x or z read into a type that holds none is 0, whichever
// operation carries the bits there.
TEST(IntegralOperationTest, ATwoStateAnswerHoldsEveryUnknownAsZero) {
  // 8'b1111_xxzz, position 2: bits 5..2 are 11xx.
  const Word source = Four(0xFC, 0x0F, 8);
  const Word at_two = Signed(Two(2, 32));
  EXPECT_EQ(
      Apply(
          IntegralOp::kSlice, std::array{source.Bits(), at_two.Number()}, 4,
          false)
          .value,
      0xCU);
  const Answer kept = Apply(
      IntegralOp::kSlice, std::array{source.Bits(), at_two.Number()}, 4, true);
  EXPECT_EQ(kept.value, 0xFU);
  EXPECT_EQ(kept.unknown, 0x3U);
  // A part outside the value reads x, which is 0 here.
  EXPECT_EQ(
      Apply(
          IntegralOp::kSlice,
          std::array{source.Bits(), Signed(Two(6, 32)).Number()}, 4, false)
          .value,
      0x3U);
  // A position holding x names none.
  EXPECT_EQ(
      Apply(
          IntegralOp::kSlice,
          std::array{source.Bits(), Signed(Four(0, 1, 32)).Number()}, 4, false)
          .value,
      0U);

  // 4'b1x0z written at position 2 of 8'hFF: bits 5..2 become 1000.
  EXPECT_EQ(
      Apply(
          IntegralOp::kWithSlice,
          std::array{
              Two(0xFF, 8).Bits(), at_two.Number(), Four(0xC, 0x5, 4).Bits()},
          8, false)
          .value,
      0xE3U);
  const Answer landed = Apply(
      IntegralOp::kWithSlice,
      std::array{
          Four(0xFF, 0, 8).Bits(), at_two.Number(), Four(0xC, 0x5, 4).Bits()},
      8, true);
  EXPECT_EQ(landed.value, 0xF3U);
  EXPECT_EQ(landed.unknown, 0x14U);

  // {3{2'b1x}} is 6'b1x1x1x, and 6'b101010 with no x.
  EXPECT_EQ(
      Apply(
          IntegralOp::kReplicate, std::array{Four(0x3, 0x1, 2).Bits()}, 6,
          false)
          .value,
      0x2AU);
  const Answer copies = Apply(
      IntegralOp::kReplicate, std::array{Four(0x3, 0x1, 2).Bits()}, 6, true);
  EXPECT_EQ(copies.value, 0x3FU);
  EXPECT_EQ(copies.unknown, 0x15U);

  EXPECT_EQ(
      Apply(IntegralOp::kConvert, std::array{source.Number()}, 8, false).value,
      0xF0U);
  // An x in a shift amount, and a zero divisor, make the whole answer x.
  EXPECT_EQ(
      Apply(
          IntegralOp::kShiftLeft,
          std::array{Two(0xFF, 8).Bits(), Four(1, 1, 4).Bits()}, 8, false)
          .value,
      0U);
  EXPECT_EQ(
      Apply(
          IntegralOp::kDivide,
          std::array{Two(9, 8).Number(), Two(0, 8).Number()}, 8, false)
          .value,
      0U);
  EXPECT_EQ(
      Apply(
          IntegralOp::kMergeConditional,
          std::array{Two(0x0F, 8).Bits(), Two(0x3C, 8).Bits()}, 8, false)
          .value,
      0x0CU);
}

// Every position above the width stays clear in both planes, whatever the
// operation sets below it.
TEST(IntegralOperationTest, NothingAboveTheWidthIsSet) {
  const Word five = Two(0x05, 5);
  EXPECT_EQ(
      Apply(IntegralOp::kBitwiseNot, std::array{five.Bits()}, 5, false).value,
      0x1AU);
  EXPECT_EQ(
      Apply(
          IntegralOp::kBitwiseXnor, std::array{five.Bits(), five.Number()}, 5,
          false)
          .value,
      0x1FU);
  EXPECT_EQ(
      Apply(IntegralOp::kNegate, std::array{five.Bits()}, 5, false).value,
      0x1BU);
  EXPECT_EQ(
      Apply(
          IntegralOp::kShiftLeft, std::array{five.Bits(), Two(3, 4).Bits()}, 5,
          false)
          .value,
      0x08U);
  EXPECT_EQ(
      Apply(
          IntegralOp::kSubtract,
          std::array{Two(0, 5).Bits(), Two(1, 5).Number()}, 5, false)
          .value,
      0x1FU);
  const Answer unknown = Apply(
      IntegralOp::kAdd,
      std::array{Four(1, 1, 5).Bits(), Four(1, 0, 5).Number()}, 5, true);
  EXPECT_EQ(unknown.value, 0x1FU);
  EXPECT_EQ(unknown.unknown, 0x1FU);
  EXPECT_EQ(
      Apply(
          IntegralOp::kFromInt,
          std::array<IntegralOperand, 1>{std::int64_t{-1}}, 5, false)
          .value,
      0x1FU);
  EXPECT_EQ(
      Apply(
          IntegralOp::kConvert, std::array{Signed(Two(0x5, 3)).Number()}, 5,
          false)
          .value,
      0x1DU);
}

// LRM 11.4.3, 11.4.4, 11.4.10, 6.24.1: an operation that reads a number reads
// it at the signedness its operand's type states.
TEST(IntegralOperationTest, ANumberIsReadAtItsSignedness) {
  const Word minus_two = Two(0xFE, 8);
  const Word three = Two(0x03, 8);
  const auto one_bit = [](IntegralOp op, const Word& a, const Word& b) {
    return Apply(op, std::array{a.Number(), b.Number()}, 1, false).value;
  };
  EXPECT_EQ(one_bit(IntegralOp::kLess, minus_two, three), 0U);
  EXPECT_EQ(one_bit(IntegralOp::kLess, Signed(minus_two), Signed(three)), 1U);
  EXPECT_EQ(one_bit(IntegralOp::kGreaterEqual, minus_two, three), 1U);
  EXPECT_EQ(
      one_bit(IntegralOp::kGreaterEqual, Signed(minus_two), Signed(three)), 0U);

  const auto eight = [](IntegralOp op, const Word& a, const Word& b) {
    return Apply(op, std::array{a.Number(), b.Number()}, 8, false).value;
  };
  // -2 / 3 truncates to 0 and leaves -2; 254 / 3 is 84 and leaves 2.
  EXPECT_EQ(eight(IntegralOp::kDivide, Signed(minus_two), Signed(three)), 0U);
  EXPECT_EQ(
      eight(IntegralOp::kModulo, Signed(minus_two), Signed(three)), 0xFEU);
  EXPECT_EQ(eight(IntegralOp::kDivide, minus_two, three), 84U);
  EXPECT_EQ(eight(IntegralOp::kModulo, minus_two, three), 2U);
  // The lowest number over -1 wraps to itself.
  EXPECT_EQ(
      eight(IntegralOp::kDivide, Signed(Two(0x80, 8)), Signed(Two(0xFF, 8))),
      0x80U);

  const auto shifted = [](const Word& a) {
    return Apply(
               IntegralOp::kArithmeticShiftRight,
               std::array{a.Number(), Two(1, 4).Bits()}, 8, false)
        .value;
  };
  EXPECT_EQ(shifted(Signed(minus_two)), 0xFFU);
  EXPECT_EQ(shifted(minus_two), 0x7FU);

  EXPECT_EQ(
      Apply(
          IntegralOp::kConvert, std::array{Signed(minus_two).Number()}, 16,
          false)
          .value,
      0xFFFEU);
  EXPECT_EQ(
      Apply(IntegralOp::kConvert, std::array{minus_two.Number()}, 16, false)
          .value,
      0x00FEU);
  const auto to_int = [](const Word& a) {
    Answer unused;
    return std::get<std::int64_t>(ApplyIntegralOperation(
        IntegralOp::kToInt64, std::array{a.Number()},
        Planes{.value = {&unused.value, 1}, .unknown = {}}, 0));
  };
  EXPECT_EQ(to_int(Signed(minus_two)), -2);
  EXPECT_EQ(to_int(minus_two), 254);
  // An x or z bit reads as 0 (LRM 6.12.1).
  EXPECT_EQ(to_int(Four(0xFF, 0x0F, 8)), 0xF0);
}

}  // namespace
}  // namespace lyra::value
