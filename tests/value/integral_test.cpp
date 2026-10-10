#include "lyra/value/integral.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <gtest/gtest.h>
#include <limits>
#include <optional>
#include <span>
#include <string>
#include <type_traits>
#include <utility>

#include "lyra/value/integral_words.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::value {
namespace {

using Logic8 = Integral<8, Signedness::kUnsigned, StateDomain::kFourState>;
using Byte8 = Integral<8, Signedness::kSigned, StateDomain::kTwoState>;
using Bits8 = Integral<8, Signedness::kUnsigned, StateDomain::kTwoState>;
using Logic100 = Integral<100, Signedness::kUnsigned, StateDomain::kFourState>;
using Signed100 = Integral<100, Signedness::kSigned, StateDomain::kTwoState>;
using Signed65 = Integral<65, Signedness::kSigned, StateDomain::kTwoState>;
using Unsigned65 = Integral<65, Signedness::kUnsigned, StateDomain::kTwoState>;
using Bits4 = Integral<4, Signedness::kUnsigned, StateDomain::kTwoState>;
using Bits136 = Integral<136, Signedness::kUnsigned, StateDomain::kTwoState>;

// A value is the bits its type declares and nothing else: one storage unit of
// the width's size per plane, a second plane only where a bit can hold x or z.
static_assert(sizeof(Int) == 4);
static_assert(sizeof(Bit) == 1);
static_assert(sizeof(Logic8) == 2);
static_assert(sizeof(Logic100) == 32);
static_assert(std::is_trivially_copyable_v<Logic100>);

// An operation on constants is a constant.
static_assert((Int::FromInt(3) + Int::FromInt(4)).ToInt64() == 7);
static_assert((Int::FromInt(-7) / Int::FromInt(2)).ToInt64() == -3);

// The low word of each plane of a value, which is the whole of one no wider
// than a word.
template <IntegralValue T>
auto ValueBits(const T& value) -> std::uint64_t {
  return value.Load().value[0];
}
template <IntegralValue T>
auto UnknownBits(const T& value) -> std::uint64_t {
  return WordAt(value.Load().unknown, 0);
}

// A four-state value built from the two planes of its low word.
template <IntegralValue T>
auto FromPlanes(std::uint64_t value, std::uint64_t unknown) -> T {
  typename T::Words words;
  words.value[0] = value;
  words.unknown[0] = unknown;
  return T::FromWords(words);
}

// The bit at `position` of a plane held as words, worked out without the
// library: what a read of that position has to answer.
auto BitOf(std::span<const std::uint64_t> words, std::uint64_t position)
    -> bool {
  return ((words[position / 64U] >> (position % 64U)) & 1U) != 0U;
}

constexpr std::array<FourStateBit, 4> kScalars{
    FourStateBit::kZero, FourStateBit::kOne, FourStateBit::kHighImpedance,
    FourStateBit::kUnknown};

// LRM 11.4.3: arithmetic wraps at the width, divides toward zero, gives a
// remainder the dividend's sign, and makes a zero divisor x -- which is 0 in a
// two-state value.
TEST(IntegralTest, ArithmeticFollowsTheOperandType) {
  EXPECT_EQ((Byte8::FromInt(100) + Byte8::FromInt(100)).ToInt64(), -56);
  EXPECT_EQ((Byte8::FromInt(-7) / Byte8::FromInt(2)).ToInt64(), -3);
  EXPECT_EQ((Byte8::FromInt(-7) % Byte8::FromInt(2)).ToInt64(), -1);
  EXPECT_EQ((Int::FromInt(-10) % Int::FromInt(3)).ToInt64(), -1);
  EXPECT_EQ((Int::FromInt(11) % Int::FromInt(-3)).ToInt64(), 2);
  EXPECT_EQ((Int::FromInt(7) * Int::FromInt(-3)).ToInt64(), -21);
  EXPECT_EQ(
      (IntUnsigned::FromInt(0xFFFFFFFF) / IntUnsigned::FromInt(3)).ToInt64(),
      0x55555555);
  EXPECT_EQ(
      (IntUnsigned::FromInt(0xFFFFFFFF) % IntUnsigned::FromInt(7)).ToInt64(),
      0xFFFFFFFF % 7);
  EXPECT_EQ((Signed100::FromInt(-1) + Signed100::FromInt(1)).ToInt64(), 0);
  EXPECT_EQ((-Signed100::FromInt(5)).ToInt64(), -5);
  const auto product = Signed100::FromInt(std::int64_t{1} << 40) *
                       Signed100::FromInt(std::int64_t{1} << 40);
  EXPECT_EQ(ExtractBits<BitVector<8>>(product, 80).ToInt64(), 1);
  EXPECT_EQ(
      (product / Signed100::FromInt(std::int64_t{1} << 40)).ToInt64(),
      std::int64_t{1} << 40);
  EXPECT_EQ(
      (LongInt::FromInt(std::int64_t{1} << 40) *
       LongInt::FromInt(std::int64_t{1} << 40))
          .ToInt64(),
      0);
}

// LRM 11.4.3: a zero divisor makes a quotient and a remainder x, and so does
// an x or z in either operand of any arithmetic operator.
TEST(IntegralTest, AZeroDivisorOrAnUnknownOperandMakesArithmeticUnknown) {
  EXPECT_EQ((Byte8::FromInt(5) / Byte8::FromInt(0)).ToInt64(), 0);
  EXPECT_EQ((Byte8::FromInt(5) % Byte8::FromInt(0)).ToInt64(), 0);
  const auto quotient = Logic8::FromInt(5) / Logic8::FromInt(0);
  EXPECT_EQ(ValueBits(quotient), 0xFFU);
  EXPECT_EQ(UnknownBits(quotient), 0xFFU);
  const auto remainder = Logic100::FromInt(5) % Logic100::FromInt(0);
  EXPECT_TRUE(remainder.IsBitIdentical(Logic100{}));
  const Logic8 unwritten;
  EXPECT_TRUE((unwritten * Logic8::FromInt(0)).IsBitIdentical(unwritten));
  EXPECT_TRUE((Logic8::FromInt(1) / unwritten).IsBitIdentical(unwritten));
  EXPECT_TRUE((-unwritten).IsBitIdentical(unwritten));
  EXPECT_TRUE((Logic100::FromInt(1) - FromPlanes<Logic100>(0, 1))
                  .IsBitIdentical(Logic100{}));
}

// A value of one word multiplies and divides as the machine does, and a value
// of many words by the long algorithms. The two are one operator, so they
// answer alike wherever the answer fits both.
TEST(IntegralTest, OneWordAndManyWordsAnswerAlike) {
  constexpr std::array<std::int64_t, 9> kNumbers{
      -1000003, -7, -2, -1, 1, 2, 3, 100, 2147483647};
  for (const std::int64_t a : kNumbers) {
    for (const std::int64_t b : kNumbers) {
      const auto a32 = static_cast<std::int32_t>(a);
      const auto b32 = static_cast<std::int32_t>(b);
      const Int quotient = Int::FromInt(a) / Int::FromInt(b);
      const Int remainder = Int::FromInt(a) % Int::FromInt(b);
      EXPECT_EQ(quotient.ToInt64(), a32 / b32) << a << " / " << b;
      EXPECT_EQ(remainder.ToInt64(), a32 % b32) << a << " % " << b;
      EXPECT_TRUE(
          Convert<Signed100>(quotient).IsBitIdentical(
              Signed100::FromInt(a) / Signed100::FromInt(b)))
          << a << " / " << b;
      EXPECT_TRUE(
          Convert<Signed100>(remainder).IsBitIdentical(
              Signed100::FromInt(a) % Signed100::FromInt(b)))
          << a << " % " << b;
      EXPECT_TRUE(
          Convert<Signed65>(LongInt::FromInt(a) * LongInt::FromInt(b))
              .IsBitIdentical(Signed65::FromInt(a) * Signed65::FromInt(b)))
          << a << " * " << b;
      EXPECT_TRUE(
          Convert<Signed65>(LongInt::FromInt(a) / LongInt::FromInt(b))
              .IsBitIdentical(Signed65::FromInt(a) / Signed65::FromInt(b)))
          << a << " / " << b;
      const auto ua = static_cast<std::uint32_t>(a);
      const auto ub = static_cast<std::uint32_t>(b);
      EXPECT_EQ(
          (IntUnsigned::FromInt(ua) / IntUnsigned::FromInt(ub)).ToInt64(),
          ua / ub)
          << ua << " / " << ub;
      EXPECT_TRUE(
          Convert<BitVector<100>>(
              IntUnsigned::FromInt(ua) % IntUnsigned::FromInt(ub))
              .IsBitIdentical(
                  BitVector<100>::FromInt(ua) % BitVector<100>::FromInt(ub)))
          << ua << " % " << ub;
    }
  }
  // The one quotient a machine divide cannot hold wraps to the dividend, at
  // every width, with nothing left over.
  constexpr std::int64_t kLowest32 = std::numeric_limits<std::int32_t>::min();
  constexpr std::int64_t kLowest64 = std::numeric_limits<std::int64_t>::min();
  EXPECT_EQ((Int::FromInt(kLowest32) / Int::FromInt(-1)).ToInt64(), kLowest32);
  EXPECT_EQ((Int::FromInt(kLowest32) % Int::FromInt(-1)).ToInt64(), 0);
  EXPECT_EQ(
      (LongInt::FromInt(kLowest64) / LongInt::FromInt(-1)).ToInt64(),
      kLowest64);
  EXPECT_EQ((LongInt::FromInt(kLowest64) % LongInt::FromInt(-1)).ToInt64(), 0);
  const Signed65 lowest65 = Signed65::FromInt(1).ShiftLeft(Int::FromInt(64));
  EXPECT_TRUE((lowest65 / Signed65::FromInt(-1)).IsBitIdentical(lowest65));
  EXPECT_EQ((lowest65 % Signed65::FromInt(-1)).ToInt64(), 0);
}

// LRM 11.4.3 Table 11-4, a row per sign of the exponent and a column per base.
// An x or z in either operand makes the result x, a zero exponent included, and
// an exponent is read whole however wide it is.
TEST(IntegralTest, PowerFollowsItsTable) {
  const auto pow = [](std::int64_t base, std::int64_t exponent) {
    return Int::FromInt(base).Pow(Int::FromInt(exponent)).ToInt64();
  };
  EXPECT_EQ(pow(-2, 3), -8);
  EXPECT_EQ(pow(-1, 3), -1);
  EXPECT_EQ(pow(-1, 4), 1);
  EXPECT_EQ(pow(0, 5), 0);
  EXPECT_EQ(pow(1, 9), 1);
  EXPECT_EQ(pow(3, 4), 81);
  for (const std::int64_t base : {-5, -1, 0, 1, 5}) {
    EXPECT_EQ(pow(base, 0), 1) << base;
  }
  EXPECT_EQ(pow(-2, -1), 0);
  EXPECT_EQ(pow(-1, -3), -1);
  EXPECT_EQ(pow(-1, -2), 1);
  EXPECT_EQ(pow(1, -5), 1);
  EXPECT_EQ(pow(2, -1), 0);
  EXPECT_EQ(pow(0, -1), 0);
  EXPECT_TRUE(
      Integer::FromInt(0).Pow(Int::FromInt(-1)).IsBitIdentical(Integer{}));
  // An unsigned base holds no -1: all ones is a number above 1.
  EXPECT_EQ(
      IntUnsigned::FromInt(0xFFFFFFFF).Pow(Int::FromInt(-1)).ToInt64(), 0);
  EXPECT_EQ(IntUnsigned::FromInt(0xFFFFFFFF).Pow(Int::FromInt(2)).ToInt64(), 1);

  EXPECT_TRUE(Integer{}.Pow(Int::FromInt(0)).IsBitIdentical(Integer{}));
  EXPECT_TRUE(Integer{}.Pow(Int::FromInt(2)).IsBitIdentical(Integer{}));
  EXPECT_TRUE(Integer::FromInt(2).Pow(Integer{}).IsBitIdentical(Integer{}));
  EXPECT_EQ(Int::FromInt(2).Pow(Integer{}).ToInt64(), 0);

  const std::array<std::uint64_t, 0> two_state{};
  const Unsigned65 huge =
      Unsigned65::FromWords(std::array<std::uint64_t, 2>{0, 1}, two_state);
  EXPECT_EQ(Int::FromInt(2).Pow(huge).ToInt64(), 0);
  EXPECT_EQ(Int::FromInt(1).Pow(huge).ToInt64(), 1);
  EXPECT_EQ(Int::FromInt(-1).Pow(huge).ToInt64(), 1);
  const Signed65 hugely_negative =
      Signed65::FromWords(std::array<std::uint64_t, 2>{0, 1}, two_state);
  EXPECT_EQ(Int::FromInt(2).Pow(hugely_negative).ToInt64(), 0);
  EXPECT_EQ(Int::FromInt(-1).Pow(hugely_negative).ToInt64(), 1);
  EXPECT_TRUE(
      Integer::FromInt(0).Pow(hugely_negative).IsBitIdentical(Integer{}));

  const Signed100 top = Signed100::FromInt(2).Pow(Int::FromInt(99));
  EXPECT_TRUE(
      top.IsBitIdentical(Signed100::FromInt(1).ShiftLeft(Int::FromInt(99))));
  EXPECT_EQ(Signed100::FromInt(-3).Pow(Int::FromInt(3)).ToInt64(), -27);
}

// A four-state value reads x until written (LRM Table 6-7); x anywhere in an
// arithmetic operand makes the result x, and a bitwise operator decides each
// position by its own table, so a known 0 settles `&` and a known 1 settles
// `|`. An operation on operands that hold no x or z answers none.
TEST(IntegralTest, UnknownBitsPropagateByEachOperatorsTable) {
  const Logic8 unwritten;
  EXPECT_TRUE(unwritten.HasUnknown());
  EXPECT_TRUE((unwritten + Logic8::FromInt(1)).HasUnknown());
  EXPECT_FALSE((Logic8::FromInt(0) & unwritten).HasUnknown());
  EXPECT_EQ((Logic8::FromInt(0xff) | unwritten).ToInt64(), 0xff);
  EXPECT_EQ((~Logic8::FromInt(0x0f)).ToInt64(), 0xf0);
  // z reads as x under every bitwise operator (LRM 11.4.8).
  const auto mixed = FromPlanes<Logic8>(0b0011'0101, 0b0000'1111);
  const Logic8 anded = Logic8::FromInt(0b0101'0101) & mixed;
  EXPECT_EQ(ValueBits(anded), 0b0001'0101U);
  EXPECT_EQ(UnknownBits(anded), 0b0000'0101U);
  const Logic8 xored = Logic8::FromInt(0b0101'0101) ^ mixed;
  EXPECT_EQ(ValueBits(xored), 0b0110'1111U);
  EXPECT_EQ(UnknownBits(xored), 0b0000'1111U);
  const Logic8 xnored = Logic8::FromInt(0b0101'0101).BitwiseXnor(mixed);
  EXPECT_EQ(ValueBits(xnored), 0b1001'1111U);
  EXPECT_EQ(UnknownBits(xnored), 0b0000'1111U);
  const Logic8 inverted = ~mixed;
  EXPECT_EQ(ValueBits(inverted), 0b1100'1111U);
  EXPECT_EQ(UnknownBits(inverted), 0b0000'1111U);

  const Logic8 sum = Logic8::FromInt(3) + Logic8::FromInt(4);
  EXPECT_EQ(ValueBits(sum), 7U);
  EXPECT_EQ(UnknownBits(sum), 0U);
  const Logic8 shifted = Logic8::FromInt(3).ShiftLeft(Int::FromInt(2));
  EXPECT_EQ(ValueBits(shifted), 12U);
  EXPECT_EQ(UnknownBits(shifted), 0U);
  const Logic8 product = Logic8::FromInt(3) * Logic8::FromInt(4);
  EXPECT_EQ(ValueBits(product), 12U);
  EXPECT_EQ(UnknownBits(product), 0U);
}

// LRM 11.4.4, 11.4.5, 11.4.6: a relation compares the numbers its operands
// hold at their signedness and is x where either holds an x or z; an equality
// is settled by a known mismatch however many unknown bits sit beside it; a
// case equality matches x and z as themselves; and a wildcard equality treats
// an x or z in its right operand as matching anything.
TEST(IntegralTest, ComparisonsAnswerOneBit) {
  EXPECT_EQ((Byte8::FromInt(-1) < Byte8::FromInt(1)).ToInt64(), 1);
  EXPECT_EQ((Bits8::FromInt(0xff) < Bits8::FromInt(1)).ToInt64(), 0);
  EXPECT_EQ((Byte8::FromInt(-128) <= Byte8::FromInt(-128)).ToInt64(), 1);
  EXPECT_EQ((Byte8::FromInt(-128) > Byte8::FromInt(127)).ToInt64(), 0);
  EXPECT_EQ((Byte8::FromInt(3) >= Byte8::FromInt(3)).ToInt64(), 1);
  EXPECT_EQ((Byte8::FromInt(3) >= Byte8::FromInt(4)).ToInt64(), 0);
  EXPECT_EQ((Signed100::FromInt(-1) < Signed100::FromInt(0)).ToInt64(), 1);
  EXPECT_EQ(
      (BitVector<100>::FromInt(-1) > BitVector<100>::FromInt(0)).ToInt64(), 1);
  const Signed100 top = Signed100::FromInt(1).ShiftLeft(Int::FromInt(70));
  EXPECT_EQ((top > Signed100::FromInt(5)).ToInt64(), 1);
  EXPECT_EQ((-top < Signed100::FromInt(-5)).ToInt64(), 1);
  const Logic8 unwritten;
  for (const Logic& answer :
       {Logic8::FromInt(3) < unwritten, unwritten <= Logic8::FromInt(3),
        Logic8::FromInt(3) > unwritten, unwritten >= unwritten}) {
    EXPECT_EQ(answer.Lsb(), FourStateBit::kUnknown);
  }

  EXPECT_EQ((Logic8::FromInt(3) == Logic8::FromInt(3)).ToInt64(), 1);
  EXPECT_EQ((Logic8::FromInt(3) == unwritten).Lsb(), FourStateBit::kUnknown);
  EXPECT_EQ((Logic8::FromInt(3) != unwritten).Lsb(), FourStateBit::kUnknown);
  const auto partly = FromPlanes<Logic8>(0b1000'0001, 0b0000'0001);
  EXPECT_EQ((partly == Logic8::FromInt(0)).Lsb(), FourStateBit::kZero);
  EXPECT_EQ((partly != Logic8::FromInt(0)).Lsb(), FourStateBit::kOne);
  EXPECT_EQ((partly == Logic8::FromInt(0x80)).Lsb(), FourStateBit::kUnknown);
  EXPECT_EQ(
      (Logic100::FromInt(1) == FromPlanes<Logic100>(1, 2)).Lsb(),
      FourStateBit::kUnknown);

  const auto x_low = FromPlanes<Logic8>(0xAF, 0x0F);
  const auto z_low = FromPlanes<Logic8>(0xA0, 0x0F);
  EXPECT_EQ(x_low.CaseEqual(x_low).ToInt64(), 1);
  EXPECT_EQ(x_low.CaseEqual(z_low).ToInt64(), 0);
  EXPECT_EQ(Int::FromInt(7).CaseEqual(Int::FromInt(7)).ToInt64(), 1);

  EXPECT_EQ(
      Logic8::FromInt(0xA5).WildcardEquals(x_low).Lsb(), FourStateBit::kOne);
  EXPECT_EQ(
      Logic8::FromInt(0xB5).WildcardEquals(z_low).Lsb(), FourStateBit::kZero);
  EXPECT_EQ(
      x_low.WildcardEquals(Logic8::FromInt(0xA5)).Lsb(),
      FourStateBit::kUnknown);
  EXPECT_EQ(
      x_low.WildcardEquals(Logic8::FromInt(0x55)).Lsb(), FourStateBit::kZero);

  // LRM 12.5.1: `casez` takes z on either side as a wildcard and x as itself;
  // `casex` takes both as wildcards.
  EXPECT_EQ(Logic8::FromInt(0xA5).CasezEquals(z_low).ToInt64(), 1);
  EXPECT_EQ(z_low.CasezEquals(Logic8::FromInt(0xA5)).ToInt64(), 1);
  EXPECT_EQ(Logic8::FromInt(0xA5).CasezEquals(x_low).ToInt64(), 0);
  EXPECT_EQ(x_low.CasezEquals(x_low).ToInt64(), 1);
  EXPECT_EQ(Logic8::FromInt(0xB5).CasezEquals(z_low).ToInt64(), 0);
  EXPECT_EQ(Logic8::FromInt(0xA5).CasexEquals(x_low).ToInt64(), 1);
  EXPECT_EQ(z_low.CasexEquals(x_low).ToInt64(), 1);
  EXPECT_EQ(Logic8::FromInt(0xB5).CasexEquals(x_low).ToInt64(), 0);
}

// LRM 11.4.7: a logical operator reads each operand as its truth value, which
// one definitely-set bit settles as true whatever x or z sits beside it.
TEST(IntegralTest, LogicalOperatorsReadTruth) {
  const Logic8 unwritten;
  const auto known_one_beside_x = FromPlanes<Logic8>(0b0000'0011, 0b10);
  EXPECT_TRUE((Int::FromInt(3) && unwritten).HasUnknown());
  EXPECT_EQ((Int::FromInt(0) && unwritten).ToInt64(), 0);
  EXPECT_EQ((Int::FromInt(3) || unwritten).ToInt64(), 1);
  EXPECT_TRUE((Int::FromInt(0) || unwritten).HasUnknown());
  EXPECT_EQ((Int::FromInt(3) && known_one_beside_x).Lsb(), FourStateBit::kOne);
  EXPECT_EQ((!known_one_beside_x).Lsb(), FourStateBit::kZero);
  EXPECT_EQ((!unwritten).Lsb(), FourStateBit::kUnknown);
  EXPECT_EQ((!Int::FromInt(0)).ToInt64(), 1);
  EXPECT_EQ(Int::FromInt(3).LogicalEquivalence(Byte8::FromInt(5)).ToInt64(), 1);
  EXPECT_EQ(Int::FromInt(0).LogicalEquivalence(Byte8::FromInt(5)).ToInt64(), 0);
  EXPECT_EQ(Int::FromInt(0).LogicalEquivalence(Byte8::FromInt(0)).ToInt64(), 1);
  EXPECT_EQ(
      Int::FromInt(0).LogicalEquivalence(unwritten).Lsb(),
      FourStateBit::kUnknown);
  EXPECT_FALSE(unwritten.IsTruthy());
  EXPECT_TRUE(known_one_beside_x.IsTruthy());
}

// LRM 11.4.10: a shift past a word boundary carries bits across it, `>>>`
// fills from the sign of a signed value and with 0 from an unsigned one, an
// amount at or past the width moves every bit out, and an x or z in the amount
// makes the whole result x.
TEST(IntegralTest, ShiftsMoveBitsByTheNumberTheAmountHolds) {
  const auto top = Signed100::FromInt(1).ShiftLeft(Int::FromInt(99));
  EXPECT_EQ((top < Signed100::FromInt(0)).ToInt64(), 1);
  EXPECT_EQ(top.ArithmeticShiftRight(Int::FromInt(98)).ToInt64(), -2);
  EXPECT_EQ(top.LogicalShiftRight(Int::FromInt(98)).ToInt64(), 2);
  EXPECT_EQ(
      Bits8::FromInt(0x80).ArithmeticShiftRight(Int::FromInt(2)).ToInt64(),
      0x20);
  EXPECT_EQ(
      Byte8::FromInt(-128).ArithmeticShiftRight(Int::FromInt(2)).ToInt64(),
      -32);
  EXPECT_EQ(
      Byte8::FromInt(-128).LogicalShiftRight(Int::FromInt(2)).ToInt64(), 0x20);
  // An x at the top of a signed value extends down as x.
  const auto x_sign = FromPlanes<SignedLogicVector<8>>(0x80, 0x80);
  const auto extended = x_sign.ArithmeticShiftRight(Int::FromInt(3));
  EXPECT_EQ(ValueBits(extended), 0xF0U);
  EXPECT_EQ(UnknownBits(extended), 0xF0U);

  EXPECT_EQ(Bits8::FromInt(0xff).ShiftLeft(Int::FromInt(8)).ToInt64(), 0);
  EXPECT_EQ(Bits8::FromInt(0xff).ShiftLeft(Int::FromInt(7)).ToInt64(), 0x80);
  EXPECT_EQ(
      Bits8::FromInt(0xff).LogicalShiftRight(Int::FromInt(200)).ToInt64(), 0);
  EXPECT_EQ(
      Byte8::FromInt(-2).ArithmeticShiftRight(Int::FromInt(200)).ToInt64(), -1);
  // The amount is read unsigned and whole: one whose high word is set moves
  // every bit out.
  const std::array<std::uint64_t, 0> two_state{};
  const Unsigned65 huge =
      Unsigned65::FromWords(std::array<std::uint64_t, 2>{0, 1}, two_state);
  EXPECT_EQ(Bits8::FromInt(0xff).ShiftLeft(huge).ToInt64(), 0);
  EXPECT_EQ(Bits8::FromInt(1).ShiftLeft(Byte8::FromInt(-1)).ToInt64(), 0);

  EXPECT_TRUE(Logic8::FromInt(1).ShiftLeft(Logic8{}).IsBitIdentical(Logic8{}));
  EXPECT_TRUE(
      Logic8::FromInt(1).LogicalShiftRight(Logic8{}).IsBitIdentical(Logic8{}));
  EXPECT_TRUE(
      Logic100::FromInt(1)
          .ArithmeticShiftRight(FromPlanes<Logic8>(0, 1))
          .IsBitIdentical(Logic100{}));
  EXPECT_EQ(Bits8::FromInt(0xff).ShiftLeft(Logic8{}).ToInt64(), 0);
}

// Conversion widens by the source's signedness and makes x or z 0 in a
// two-state result; concatenation and replication place their operands from
// the least significant end up.
TEST(IntegralTest, ConversionAndJoinsPlaceBits) {
  EXPECT_EQ(Convert<Int>(Byte8::FromInt(-3)).ToInt64(), -3);
  EXPECT_EQ(Convert<IntUnsigned>(Logic8::FromInt(0xf0)).ToInt64(), 0xf0);
  EXPECT_EQ(
      ExtractBits<BitVector<8>>(Convert<Signed100>(Byte8::FromInt(-3)), 90)
          .ToInt64(),
      0xff);
  EXPECT_FALSE(Convert<Int>(Logic8{}).HasUnknown());
  EXPECT_TRUE(Convert<Integer>(Logic8{}).HasUnknown());
  const auto joined = Logic8::FromInt(0xab).Concat(Byte8::FromInt(0x12));
  static_assert(std::is_same_v<decltype(joined), const LogicVector<16>>);
  EXPECT_EQ(joined.ToInt64(), 0xab12);
  const auto wide = Signed100::FromInt(-1).Concat(Bits4::FromInt(0x5));
  static_assert(std::is_same_v<decltype(wide), const BitVector<104>>);
  EXPECT_EQ(ExtractBits<Bits8>(wide, 0).ToInt64(), 0xf5);
  EXPECT_EQ(ExtractBits<Bits8>(wide, 96).ToInt64(), 0xff);
  const auto with_unknown = Bits4::FromInt(0x9).Concat(Logic8{});
  EXPECT_EQ(ValueBits(with_unknown), 0x9FFU);
  EXPECT_EQ(UnknownBits(with_unknown), 0x0FFU);
  EXPECT_EQ(Replicate<BitVector<12>>(Bits4::FromInt(0x5)).ToInt64(), 0x555);
  const auto copies = Replicate<BitVector<100>>(Bits4::FromInt(0x9));
  EXPECT_EQ(ExtractBits<Bits8>(copies, 60).ToInt64(), 0x99);
  EXPECT_EQ(ExtractBits<Bits8>(copies, 92).ToInt64(), 0x99);
  const auto filled = Replicate<Logic100>(Logic{});
  EXPECT_TRUE(filled.IsBitIdentical(Logic100{}));
}

// LRM 6.11.2 / 6.12.1: a sign bit that is unknown fills with x into four
// states and with the 0 it becomes into two, and a narrowing keeps the low
// bits.
TEST(IntegralTest, AConversionExtendsAnUnknownSign) {
  const auto x_sign = FromPlanes<SignedLogicVector<4>>(0xa, 0x8);
  const auto four = Convert<SignedLogicVector<8>>(x_sign);
  EXPECT_EQ(ValueBits(four), 0xFAU);
  EXPECT_EQ(UnknownBits(four), 0xF8U);
  const auto z_sign = FromPlanes<SignedLogicVector<4>>(0x2, 0x8);
  const auto wide = Convert<SignedLogicVector<100>>(z_sign);
  EXPECT_EQ(ExtractBits<Logic>(wide, 99).Lsb(), FourStateBit::kHighImpedance);
  EXPECT_EQ(ExtractBits<Logic>(wide, 64).Lsb(), FourStateBit::kHighImpedance);
  EXPECT_EQ(ExtractBits<Logic>(wide, 1).Lsb(), FourStateBit::kOne);
  EXPECT_EQ(Convert<Byte8>(x_sign).ToInt64(), 2);
  EXPECT_EQ(Convert<Bits4>(BitVector<16>::FromInt(0x1234)).ToInt64(), 4);
  // An unsigned source widens with 0 whatever its top bit holds.
  const auto unsigned_x = FromPlanes<LogicVector<4>>(0xa, 0x8);
  const auto zero_extended = Convert<Logic8>(unsigned_x);
  EXPECT_EQ(ValueBits(zero_extended), 0x0AU);
  EXPECT_EQ(UnknownBits(zero_extended), 0x08U);
}

// LRM 11.5.1: a select reads the bits at a position, x where it reaches past
// the value -- 0 in a two-state result -- and reads the result's default where
// its position names none.
TEST(IntegralTest, ASelectReadsTheBitsAtAPosition) {
  using Nibble = LogicVector<4>;
  const Logic8 source = Logic8::FromInt(0xD2);
  EXPECT_EQ(Slice<Nibble>(source, Position::FromInt(4)).ToInt64(), 0xD);
  const auto leaving = Slice<Nibble>(source, Position::FromInt(6));
  EXPECT_EQ(ValueBits(leaving), 0xFU);
  EXPECT_EQ(UnknownBits(leaving), 0xCU);
  const auto below = Slice<Nibble>(source, Int::FromInt(-2));
  EXPECT_EQ(ValueBits(below), 0xBU);
  EXPECT_EQ(UnknownBits(below), 0x3U);
  EXPECT_TRUE(
      Slice<Nibble>(source, Int::FromInt(100)).IsBitIdentical(Nibble{}));
  EXPECT_TRUE(Slice<Nibble>(source, Int::FromInt(-4)).IsBitIdentical(Nibble{}));
  EXPECT_TRUE(Slice<Nibble>(source, Position{}).IsBitIdentical(Nibble{}));

  const Bits8 two_state = Bits8::FromInt(0xD2);
  EXPECT_EQ(Slice<Bits4>(two_state, Int::FromInt(6)).ToInt64(), 0x3);
  EXPECT_EQ(Slice<Bits4>(two_state, Int::FromInt(-2)).ToInt64(), 0x8);
  EXPECT_EQ(Slice<Bits4>(two_state, Int::FromInt(100)).ToInt64(), 0);
  EXPECT_EQ(Slice<Bits4>(two_state, Logic8{}).ToInt64(), 0);

  // Bits read out of a value that can hold x or z into one that cannot hold
  // each x or z as 0.
  const auto partly = FromPlanes<Logic8>(0xFF, 0x0F);
  EXPECT_EQ(ExtractBits<Bits4>(partly, 2).ToInt64(), 0xC);
  EXPECT_EQ(Slice<Bits4>(partly, Int::FromInt(6)).ToInt64(), 0x3);
}

// A read out of a value of many words answers each position of the result
// from the position of the source it names, whether the result is narrower
// than a word, exactly one at an offset inside a word, or wider than one.
TEST(IntegralTest, ASelectReadsAcrossWords) {
  const std::array<std::uint64_t, 3> words{
      0x0123456789ABCDEFULL, 0xFEDCBA9876543210ULL, 0x5AULL};
  const std::array<std::uint64_t, 0> two_state{};
  const Bits136 source = Bits136::FromWords(words, two_state);

  const auto wide = ExtractBits<BitVector<72>>(source, 60);
  const auto wide_words = wide.Load();
  for (std::uint64_t at = 0; at < 72; ++at) {
    EXPECT_EQ(BitOf(wide_words.value, at), BitOf(words, 60 + at)) << at;
  }
  EXPECT_EQ(wide_words.value[1] >> 8U, 0U);

  const auto word = Slice<BitVector<64>>(source, Int::FromInt(32));
  EXPECT_EQ(ValueBits(word), 0x7654321001234567ULL);

  const auto past_the_top = ExtractBits<BitVector<72>>(source, 100);
  const auto past_words = past_the_top.Load();
  for (std::uint64_t at = 0; at < 72; ++at) {
    EXPECT_EQ(BitOf(past_words.value, at), at < 36 && BitOf(words, 100 + at))
        << at;
  }

  const auto below =
      ExtractBits<LogicVector<72>>(Convert<LogicVector<136>>(source), -70);
  const auto below_words = below.Load();
  for (std::uint64_t at = 0; at < 72; ++at) {
    EXPECT_EQ(BitOf(below_words.unknown, at), at < 70) << at;
  }
  EXPECT_EQ(BitOf(below_words.value, 70), BitOf(words, 0));
  EXPECT_EQ(BitOf(below_words.value, 71), BitOf(words, 1));
}

// LRM 11.5.1: a write lands only on the positions inside the value and leaves
// every other as it stands, and answers the positions it reached.
TEST(IntegralTest, AWriteLandsOnThePositionsItNames) {
  Logic8 written = Logic8::FromInt(0);
  const auto reached = InsertBits(written, 6, Bits4::FromInt(0xf));
  EXPECT_EQ(written.ToInt64(), 0xc0);
  ASSERT_TRUE(reached.has_value());
  EXPECT_EQ(reached->lsb, 6U);
  EXPECT_EQ(reached->width, 2U);

  Bits8 below = Bits8::FromInt(0);
  const auto low = InsertBits(below, -2, Bits4::FromInt(0b0110));
  EXPECT_EQ(below.ToInt64(), 0b01);
  ASSERT_TRUE(low.has_value());
  EXPECT_EQ(low->lsb, 0U);
  EXPECT_EQ(low->width, 2U);
  EXPECT_FALSE(InsertBits(below, 8, Bits4::FromInt(0xf)).has_value());
  EXPECT_FALSE(InsertBits(below, -4, Bits4::FromInt(0xf)).has_value());
  EXPECT_EQ(below.ToInt64(), 0b01);

  // Bits written carry their own x and z into four-state storage, and bits
  // that hold none settle the positions they land on and no others.
  Logic8 carried = Logic8::FromInt(0);
  InsertBits(carried, 2, FromPlanes<LogicVector<4>>(0b1010, 0b0110));
  EXPECT_EQ(ValueBits(carried), 0x28U);
  EXPECT_EQ(UnknownBits(carried), 0x18U);
  Logic8 settled;
  InsertBits(settled, 2, Bits4::FromInt(0b0101));
  EXPECT_EQ(ValueBits(settled), 0xD7U);
  EXPECT_EQ(UnknownBits(settled), 0xC3U);
  // An x or z written into storage that cannot hold one lands as 0.
  Bits8 cleared = Bits8::FromInt(0xff);
  InsertBits(cleared, 2, LogicVector<4>{});
  EXPECT_EQ(cleared.ToInt64(), 0xC3);

  // A position that is itself a value names none where it holds x or z.
  const auto with_slice = [](Bits8 target, const auto& position) {
    using P = std::remove_cvref_t<decltype(position)>;
    typename Bits8::Words words = target.Load();
    WithSlice(
        words.Write(), 8, position.Load().Read(), P::kWidth, P::kSignedness,
        Bits4::FromInt(0x9).Load().Read(), 4);
    return Bits8::FromWords(words).ToInt64();
  };
  EXPECT_EQ(with_slice(Bits8::FromInt(0), Int::FromInt(4)), 0x90);
  EXPECT_EQ(with_slice(Bits8::FromInt(0x11), Logic8{}), 0x11);
}

// Bits wider than a word written across words each land at the position they
// name and move no other.
TEST(IntegralTest, AWriteLandsAcrossWords) {
  const std::array<std::uint64_t, 3> words{
      0x0123456789ABCDEFULL, 0xFEDCBA9876543210ULL, 0x5AULL};
  const std::array<std::uint64_t, 2> bits{0xA5A5A5A5DEADBEEFULL, 0xC3ULL};
  const std::array<std::uint64_t, 0> two_state{};
  Bits136 written = Bits136::FromWords(words, two_state);
  const auto reached =
      InsertBits(written, 60, BitVector<72>::FromWords(bits, two_state));
  ASSERT_TRUE(reached.has_value());
  EXPECT_EQ(reached->lsb, 60U);
  EXPECT_EQ(reached->width, 72U);
  const auto after = written.Load();
  for (std::uint64_t at = 0; at < 136; ++at) {
    const bool inside = at >= 60 && at < 132;
    EXPECT_EQ(
        BitOf(after.value, at),
        inside ? BitOf(bits, at - 60) : BitOf(words, at))
        << at;
  }

  Logic100 partly;
  InsertBits(partly, 60, BitVector<8>::FromInt(0xc3));
  EXPECT_FALSE(ExtractBits<Logic8>(partly, 60).HasUnknown());
  EXPECT_EQ(ExtractBits<Logic8>(partly, 60).ToInt64(), 0xc3);
  EXPECT_EQ(ExtractBits<Logic>(partly, 59).Lsb(), FourStateBit::kUnknown);
  EXPECT_EQ(ExtractBits<Logic>(partly, 68).Lsb(), FourStateBit::kUnknown);

  Bits136 top = Bits136::FromWords(words, two_state);
  const auto clipped =
      InsertBits(top, 130, BitVector<72>::FromWords(bits, two_state));
  ASSERT_TRUE(clipped.has_value());
  EXPECT_EQ(clipped->lsb, 130U);
  EXPECT_EQ(clipped->width, 6U);
  const auto top_words = top.Load();
  for (std::uint64_t at = 0; at < 136; ++at) {
    EXPECT_EQ(
        BitOf(top_words.value, at),
        at >= 130 ? BitOf(bits, at - 130) : BitOf(words, at))
        << at;
  }
}

// A part of a part is written only where both lie: the outer part's own
// positions bound what the inner one reaches.
TEST(IntegralTest, AWriteThroughAPartStaysInsideIt) {
  Logic8 written = Logic8::FromInt(0);
  const auto inside = InsertBitsWithin(
      written, 2, Bits4::FromInt(0xf), BitPositions{.lsb = 0, .width = 4});
  EXPECT_EQ(written.ToInt64(), 0x0c);
  ASSERT_TRUE(inside.has_value());
  EXPECT_EQ(inside->lsb, 2U);
  EXPECT_EQ(inside->width, 2U);
  EXPECT_FALSE(
      InsertBitsWithin(
          written, 6, Bits4::FromInt(0xf), BitPositions{.lsb = 0, .width = 4})
          .has_value());
  EXPECT_EQ(written.ToInt64(), 0x0c);
}

// Bits named for a write are of a type: a write lands where they lie, a part
// of them inside them only, and they are read at that type.
TEST(IntegralTest, ABitsDesignationWritesInPlaceAtItsType) {
  using Nibble = LogicVector<4>;
  Logic8 held = Logic8::FromInt(0);
  held.SliceRef<Nibble>(Int::FromInt(4)) = Nibble::FromInt(0x9);
  EXPECT_EQ(held.ToInt64(), 0x90);
  held.SliceRef<Nibble>(Int::FromInt(0)).SliceRef<Nibble>(Int::FromInt(2)) =
      Nibble::FromInt(0xf);
  EXPECT_EQ(held.ToInt64(), 0x9c);
  held.SliceRef<Nibble>(Logic8{}) = Nibble::FromInt(0);
  EXPECT_EQ(held.ToInt64(), 0x9c);
  const auto reached = held.SliceRef<Nibble>(Int::FromInt(6)).Reached();
  ASSERT_TRUE(reached.has_value());
  EXPECT_EQ(reached->lsb, 6U);
  EXPECT_EQ(reached->width, 2U);
  EXPECT_FALSE(held.SliceRef<Nibble>(Int::FromInt(8)).Reached().has_value());
  EXPECT_FALSE(held.SliceRef<Nibble>(Logic8{}).Reached().has_value());

  EXPECT_EQ(
      held.SliceRef<SignedLogicVector<4>>(Int::FromInt(4)).Read().ToInt64(),
      -7);
  EXPECT_EQ(held.SliceRef<Nibble>(Int::FromInt(4)).Read().ToInt64(), 9);
  EXPECT_TRUE(held.SliceRef<Nibble>(Logic8{}).Read().IsBitIdentical(Nibble{}));

  // A two-state part of four-state storage reads each x or z as 0 and settles
  // the bits it writes.
  Logic8 mixed;
  EXPECT_EQ(mixed.SliceRef<Bits4>(Int::FromInt(0)).Read().ToInt64(), 0);
  mixed.SliceRef<Bits4>(Int::FromInt(0)) = Bits4::FromInt(0x5);
  EXPECT_EQ(ValueBits(mixed), 0xF5U);
  EXPECT_EQ(UnknownBits(mixed), 0xF0U);
}

// LRM 11.4.11: an ambiguous condition merges its arms bit by bit, x where they
// differ -- which a two-state type holds as 0.
TEST(IntegralTest, AnAmbiguousConditionMergesTheArms) {
  const auto merged =
      Logic8::FromInt(0x0f).MergeConditional(Logic8::FromInt(0x3c));
  EXPECT_EQ(ValueBits(merged), 0x3FU);
  EXPECT_EQ(UnknownBits(merged), 0x33U);
  const auto with_z = FromPlanes<Logic8>(0x00, 0x01)
                          .MergeConditional(FromPlanes<Logic8>(0x00, 0x01));
  EXPECT_EQ(ValueBits(with_z), 0x01U);
  EXPECT_EQ(UnknownBits(with_z), 0x01U);
  const auto two_state =
      Byte8::FromInt(0x0f).MergeConditional(Byte8::FromInt(0x3c));
  EXPECT_EQ(two_state.ToInt64(), 0x0c);
  const auto wide = Logic100::FromInt(-1).MergeConditional(
      Logic100::FromInt(1).ShiftLeft(Int::FromInt(99)));
  EXPECT_EQ(ExtractBits<Logic>(wide, 99).Lsb(), FourStateBit::kOne);
  EXPECT_EQ(ExtractBits<Logic>(wide, 98).Lsb(), FourStateBit::kUnknown);
  EXPECT_EQ(ExtractBits<Logic>(wide, 0).Lsb(), FourStateBit::kUnknown);
}

// LRM 6.6.1 Table 6-2 and 6.6.3 Tables 6-3 and 6-4, every cell: z defers to
// the other driver; where both drive, a `wire` passes agreement and makes a
// conflict x, a `wand` lets any 0 win and a `wor` any 1.
TEST(IntegralTest, NetContributionsFoldUnderEachTable) {
  const auto driven = [](FourStateBit bit) {
    return bit == FourStateBit::kZero || bit == FourStateBit::kOne;
  };
  const auto expected = [&](NetResolution fold, FourStateBit a,
                            FourStateBit b) {
    if (a == FourStateBit::kHighImpedance) {
      return b;
    }
    if (b == FourStateBit::kHighImpedance) {
      return a;
    }
    const FourStateBit winner = fold == NetResolution::kWiredAnd
                                    ? FourStateBit::kZero
                                    : FourStateBit::kOne;
    switch (fold) {
      case NetResolution::kTriState:
        return a == b && driven(a) ? a : FourStateBit::kUnknown;
      case NetResolution::kWiredAnd:
      case NetResolution::kWiredOr:
        if (a == winner || b == winner) {
          return winner;
        }
        return a == b && driven(a) ? a : FourStateBit::kUnknown;
    }
    return FourStateBit::kUnknown;
  };
  for (const NetResolution fold :
       {NetResolution::kTriState, NetResolution::kWiredAnd,
        NetResolution::kWiredOr}) {
    for (const FourStateBit a : kScalars) {
      for (const FourStateBit b : kScalars) {
        const FourStateBit want = expected(fold, a, b);
        EXPECT_EQ(Resolve(fold, Logic::Filled(a), Logic::Filled(b)).Lsb(), want)
            << static_cast<int>(fold) << ' ' << static_cast<int>(a) << ' '
            << static_cast<int>(b);
        EXPECT_TRUE(Resolve(fold, Logic100::Filled(a), Logic100::Filled(b))
                        .IsBitIdentical(Logic100::Filled(want)));
      }
    }
  }
  const Logic8 one = Logic8::FromInt(0xff);
  const Logic8 zero = Logic8::FromInt(0);
  EXPECT_TRUE(one.ResolveTriState(zero).IsBitIdentical(Logic8{}));
  EXPECT_TRUE(one.ResolveWiredAnd(zero).IsBitIdentical(zero));
  EXPECT_TRUE(one.ResolveWiredOr(zero).IsBitIdentical(one));

  // LRM 28.12.1: a stronger contribution determines every position it drives.
  for (const FourStateBit stronger : kScalars) {
    for (const FourStateBit weaker : kScalars) {
      EXPECT_EQ(
          Logic::Filled(stronger).Dominating(Logic::Filled(weaker)).Lsb(),
          stronger == FourStateBit::kHighImpedance ? weaker : stronger);
    }
  }
  EXPECT_TRUE(
      FilledAs(Logic100{}, Logic::Filled(FourStateBit::kHighImpedance))
          .IsBitIdentical(Logic100::Filled(FourStateBit::kHighImpedance)));
}

// LRM 11.4.9: a reduction folds every position of the value and none above
// its width, and an x or z leaves the answer unknown only where no known bit
// settles it.
TEST(IntegralTest, ReductionsFoldEveryPositionOfTheValue) {
  using Logic67 = LogicVector<67>;
  const Logic67 ones = Logic67::FromInt(-1);
  EXPECT_EQ(ones.ReductionAnd().Lsb(), FourStateBit::kOne);
  EXPECT_EQ(ones.ReductionOr().Lsb(), FourStateBit::kOne);
  EXPECT_EQ(ones.ReductionXor().Lsb(), FourStateBit::kOne);
  EXPECT_EQ(ones.ReductionNand().Lsb(), FourStateBit::kZero);
  EXPECT_EQ(ones.ReductionNor().Lsb(), FourStateBit::kZero);
  EXPECT_EQ(ones.ReductionXnor().Lsb(), FourStateBit::kZero);
  const Logic67 zeros = Logic67::FromInt(0);
  EXPECT_EQ(zeros.ReductionAnd().Lsb(), FourStateBit::kZero);
  EXPECT_EQ(zeros.ReductionOr().Lsb(), FourStateBit::kZero);
  EXPECT_EQ(zeros.ReductionXor().Lsb(), FourStateBit::kZero);
  EXPECT_EQ(zeros.ReductionNor().Lsb(), FourStateBit::kOne);
  const Logic67 top_clear = ones.LogicalShiftRight(Int::FromInt(1));
  EXPECT_EQ(top_clear.ReductionAnd().Lsb(), FourStateBit::kZero);
  EXPECT_EQ(top_clear.ReductionXor().Lsb(), FourStateBit::kZero);
  const Logic67 top_only = Logic67::FromInt(1).ShiftLeft(Int::FromInt(66));
  EXPECT_EQ(top_only.ReductionOr().Lsb(), FourStateBit::kOne);
  EXPECT_EQ(top_only.ReductionXor().Lsb(), FourStateBit::kOne);

  Logic67 one_unknown = ones;
  InsertBits(one_unknown, 66, Logic{});
  EXPECT_EQ(one_unknown.ReductionAnd().Lsb(), FourStateBit::kUnknown);
  EXPECT_EQ(one_unknown.ReductionOr().Lsb(), FourStateBit::kOne);
  EXPECT_EQ(one_unknown.ReductionXor().Lsb(), FourStateBit::kUnknown);
  EXPECT_EQ(one_unknown.ReductionNand().Lsb(), FourStateBit::kUnknown);
  EXPECT_EQ(one_unknown.ReductionNor().Lsb(), FourStateBit::kZero);
  Logic67 zero_beside_unknown = one_unknown;
  InsertBits(zero_beside_unknown, 0, Bit::FromInt(0));
  EXPECT_EQ(zero_beside_unknown.ReductionAnd().Lsb(), FourStateBit::kZero);

  using Bits67 = BitVector<67>;
  EXPECT_EQ(Bits67::FromInt(-1).ReductionAnd().ToInt64(), 1);
  EXPECT_EQ(Bits67::FromInt(-1).ReductionXor().ToInt64(), 1);
  EXPECT_EQ(Bits67::FromInt(7).ReductionXnor().ToInt64(), 0);
  EXPECT_EQ(Logic8::FromInt(0x7).ReductionXor().ToInt64(), 1);
  EXPECT_EQ(Logic8::FromInt(0xff).ReductionNand().ToInt64(), 0);
}

// LRM 20.8.1 and 20.9: `$clog2`, `$countbits` and `$isunknown`.
TEST(IntegralTest, BitQueries) {
  EXPECT_EQ(Int::FromInt(9).Clog2().ToInt64(), 4);
  EXPECT_EQ(Int::FromInt(8).Clog2().ToInt64(), 3);
  EXPECT_EQ(Int::FromInt(1).Clog2().ToInt64(), 0);
  EXPECT_EQ(Int::FromInt(0).Clog2().ToInt64(), 0);
  EXPECT_EQ(
      BitVector<100>::FromInt(1).ShiftLeft(Int::FromInt(70)).Clog2().ToInt64(),
      70);
  EXPECT_EQ(BitVector<100>::FromInt(-1).Clog2().ToInt64(), 100);

  // 8'b0101_xz01: three ones, three zeros, one x, one z.
  const auto counted = FromPlanes<Logic8>(0b0101'1001, 0b0000'1100);
  const auto count = [&](FourStateBit control) {
    return counted.CountBits(Logic::Filled(control)).ToInt64();
  };
  EXPECT_EQ(count(FourStateBit::kOne), 3);
  EXPECT_EQ(count(FourStateBit::kZero), 3);
  EXPECT_EQ(count(FourStateBit::kUnknown), 1);
  EXPECT_EQ(count(FourStateBit::kHighImpedance), 1);
  EXPECT_EQ(counted.CountBits(Bits4::FromInt(0b0010)).ToInt64(), 6);
  EXPECT_EQ(counted.CountBits(Bits4::FromInt(0b1111)).ToInt64(), 3);
  EXPECT_EQ(
      counted.CountBits(FromPlanes<LogicVector<4>>(0b1010, 0b1100)).ToInt64(),
      8);
  EXPECT_EQ(
      LogicVector<67>{}
          .CountBits(Logic::Filled(FourStateBit::kUnknown))
          .ToInt64(),
      67);
  EXPECT_EQ(BitVector<67>::FromInt(0).CountBits(Bit::FromInt(0)).ToInt64(), 67);
  EXPECT_EQ(
      BitVector<67>::FromInt(-1).CountBits(Bit::FromInt(1)).ToInt64(), 67);

  EXPECT_EQ(counted.IsUnknown().ToInt64(), 1);
  EXPECT_EQ(Logic8::FromInt(3).IsUnknown().ToInt64(), 0);
  EXPECT_EQ(Int::FromInt(3).IsUnknown().ToInt64(), 0);
}

// LRM 11.4.14.2: a streaming concatenation reverses the order of the blocks a
// value is cut into from its least significant end, the bits of each left as
// they are, and the last block is whatever the width leaves over.
TEST(IntegralTest, BlocksAreReversedInPlace) {
  const auto reversed = [](const auto& value, std::uint64_t block) {
    using T = std::remove_cvref_t<decltype(value)>;
    typename T::Words out;
    ReverseBlocks(out.Write(), value.Load().Read(), T::kWidth, block);
    return T::FromWords(out);
  };
  EXPECT_EQ(reversed(Bits8::FromInt(0b0011'0101), 1).ToInt64(), 0b1010'1100);
  EXPECT_EQ(reversed(Bits8::FromInt(0x35), 4).ToInt64(), 0x53);
  EXPECT_EQ(reversed(Bits8::FromInt(0x35), 8).ToInt64(), 0x35);
  EXPECT_EQ(reversed(BitVector<6>::FromInt(0b11'0101), 4).ToInt64(), 0b0101'11);
  const auto four_state = reversed(FromPlanes<Logic8>(0x0F, 0x03), 1);
  EXPECT_EQ(ValueBits(four_state), 0xF0U);
  EXPECT_EQ(UnknownBits(four_state), 0xC0U);

  const std::array<std::uint64_t, 2> words{0x0123456789ABCDEFULL, 0x9ULL};
  const std::array<std::uint64_t, 0> two_state{};
  const auto wide = BitVector<100>::FromWords(words, two_state);
  const auto bitwise = reversed(wide, 1).Load();
  for (std::uint64_t at = 0; at < 100; ++at) {
    EXPECT_EQ(BitOf(bitwise.value, at), BitOf(words, 99 - at)) << at;
  }
  const auto by_bytes = reversed(wide, 8).Load();
  for (std::uint64_t at = 0; at < 100; ++at) {
    // Twelve whole bytes and a top block of four bits.
    const std::uint64_t block = at < 4 ? 12 : 11 - ((at - 4) / 8);
    const std::uint64_t within = at < 4 ? at : (at - 4) % 8;
    EXPECT_EQ(BitOf(by_bytes.value, at), BitOf(words, (block * 8) + within))
        << at;
  }
}

// LRM 21.4: a memory word is digits of its radix, the last least significant,
// with `_` between them; x and z cover a whole digit, and a two-state value
// reads them as 0.
TEST(IntegralTest, AWordIsReadInItsRadix) {
  typename Logic8::Words read;
  ASSERT_TRUE(FromDigits(read.Write(), 8, DigitRadix::kHex, "a_5"));
  EXPECT_EQ(Logic8::FromWords(read).ToInt64(), 0xa5);
  ASSERT_TRUE(FromDigits(read.Write(), 8, DigitRadix::kBinary, "1x0z"));
  const auto with_unknown = Logic8::FromWords(read);
  EXPECT_EQ(ValueBits(with_unknown), 0b1100U);
  EXPECT_EQ(UnknownBits(with_unknown), 0b0101U);
  ASSERT_TRUE(FromDigits(read.Write(), 8, DigitRadix::kHex, "?1"));
  EXPECT_EQ(ValueBits(Logic8::FromWords(read)), 0x01U);
  EXPECT_EQ(UnknownBits(Logic8::FromWords(read)), 0xF0U);
  typename Byte8::Words two_state;
  ASSERT_TRUE(FromDigits(two_state.Write(), 8, DigitRadix::kOctal, "z7"));
  EXPECT_EQ(Byte8::FromWords(two_state).ToInt64(), 7);
  ASSERT_TRUE(FromDigits(two_state.Write(), 8, DigitRadix::kHex, "x7"));
  EXPECT_EQ(Byte8::FromWords(two_state).ToInt64(), 7);
  // A longer word loses its leading digits, and a digit the width cuts keeps
  // the bits below it.
  ASSERT_TRUE(FromDigits(read.Write(), 8, DigitRadix::kHex, "123"));
  EXPECT_EQ(Logic8::FromWords(read).ToInt64(), 0x23);
  typename BitVector<5>::Words five;
  ASSERT_TRUE(FromDigits(five.Write(), 5, DigitRadix::kOctal, "77"));
  EXPECT_EQ(BitVector<5>::FromWords(five).ToInt64(), 0x1f);
  EXPECT_FALSE(FromDigits(read.Write(), 8, DigitRadix::kOctal, "9"));
  EXPECT_FALSE(FromDigits(read.Write(), 8, DigitRadix::kBinary, "2"));
  EXPECT_FALSE(FromDigits(read.Write(), 8, DigitRadix::kHex, "g"));
  EXPECT_FALSE(FromDigits(read.Write(), 8, DigitRadix::kHex, "_"));
  EXPECT_FALSE(FromDigits(read.Write(), 8, DigitRadix::kHex, ""));

  // An octal digit lying across two words lands in both.
  typename BitVector<100>::Words wide;
  ASSERT_TRUE(FromDigits(
      wide.Write(), 100, DigitRadix::kOctal, "7000000000000000000000"));
  const auto across = BitVector<100>::FromWords(wide);
  EXPECT_EQ(ExtractBits<Bits4>(across, 62).ToInt64(), 0b1110);
  EXPECT_EQ(across.CountBits(Bit::FromInt(1)).ToInt64(), 3);
}

// LRM 6.24.3 / 11.4.14.3: a stream holds its first item most significant, and
// reading a two-state item out of a four-state stream takes x as 0.
TEST(IntegralTest, StreamsHoldTheirFirstItemMostSignificant) {
  typename LogicVector<12>::Words stream;
  std::uint64_t filled =
      WriteToStream(Bits4::FromInt(0xa), stream.Write(), 12, 0);
  filled = WriteToStream(Logic8{}, stream.Write(), 12, filled);
  EXPECT_EQ(filled, 12U);
  const auto whole = LogicVector<12>::FromWords(stream);
  EXPECT_EQ(ValueBits(whole), 0xAFFU);
  EXPECT_EQ(UnknownBits(whole), 0x0FFU);
  EXPECT_EQ(ReadFromStream<Bits4>(stream.Read(), 12, 0).ToInt64(), 0xa);
  EXPECT_TRUE(ReadFromStream<Logic8>(stream.Read(), 12, 4).HasUnknown());
  EXPECT_EQ(ReadFromStream<BitVector<8>>(stream.Read(), 12, 4).ToInt64(), 0);
}

// LRM 5.9: text is a value whose last character is least significant, and
// reading a value back as text gives its bytes most significant first.
TEST(IntegralTest, TextIsBytesFromTheLeastSignificantEnd) {
  const std::string text = "AB";
  EXPECT_EQ(FromBytes<Int>(text).ToInt64(), 0x4142);
  EXPECT_EQ(FromBytes<Bits4>(text).ToInt64(), 0x2);
  EXPECT_EQ(BytesOf(BitVector<16>::FromInt(0x4142).Load().View()), "AB");
  EXPECT_EQ(BytesOf(LogicVector<16>{}.Load().View()), std::string(2, '\0'));
  const std::string long_text = "0123456789";
  const auto wide = FromBytes<BitVector<100>>(long_text);
  EXPECT_EQ(BytesOf(wide.Load().View()).substr(3), long_text);
}

// A constant states its planes as words.
TEST(IntegralTest, AConstantIsItsWords) {
  constexpr auto constant = Logic8::FromWords(
      std::array<std::uint64_t, 1>{0x05}, std::array<std::uint64_t, 1>{0xf0});
  EXPECT_EQ(ValueBits(constant), 0x05U);
  EXPECT_EQ(UnknownBits(constant), 0xF0U);
}

// Code compiled once for every integral type reads a value through its planes,
// width and signedness, and writes one the same way.
TEST(IntegralTest, AValueIsReadAndWrittenThroughAView) {
  const typename Logic8::Words read = Logic8{}.Load();
  EXPECT_EQ(read.View().width, 8U);
  EXPECT_EQ(read.View().signedness, Signedness::kUnsigned);
  EXPECT_TRUE(read.View().IsFourState());
  EXPECT_FALSE(Int::FromInt(1).Load().View().IsFourState());
  EXPECT_EQ(Int::FromInt(1).Load().View().signedness, Signedness::kSigned);
  typename Byte8::Words written;
  FromInt(written.MutableView().planes, 8U, -2);
  EXPECT_EQ(Byte8::FromWords(written).ToInt64(), -2);
}

// A position is the one integer a select's operand stands for, or nothing: past
// 64 bits the words above the first decide whether the value fits, no value is
// reached past the limit a position may hold, and the position type an index
// is brought to for arithmetic says the same.
TEST(IntegralTest, APositionIsReadTheSameWayWhateverCarriesIt) {
  EXPECT_EQ(ReadPosition(Logic8::FromInt(0xff)), 255);
  EXPECT_EQ(ReadPosition(Byte8::FromInt(-1)), -1);
  EXPECT_EQ(ReadPosition(Int::FromInt(-3)), -3);
  EXPECT_FALSE(ReadPosition(Logic8{}).has_value());
  EXPECT_FALSE(ReadPosition(BitVector<64>::FromInt(-1)).has_value());
  const std::array<std::uint64_t, 0> no_unknown{};
  EXPECT_EQ(
      ReadPosition(
          Unsigned65::FromWords(
              std::array<std::uint64_t, 2>{7ULL, 0ULL}, no_unknown)),
      7);
  EXPECT_FALSE(ReadPosition(
                   Unsigned65::FromWords(
                       std::array<std::uint64_t, 2>{7ULL, 1ULL}, no_unknown))
                   .has_value());
  EXPECT_EQ(
      ReadPosition(
          Signed65::FromWords(
              std::array<std::uint64_t, 2>{~1ULL, 1ULL}, no_unknown)),
      -2);
  EXPECT_EQ(ReadPosition(LongInt::FromInt(kPositionLimit)), kPositionLimit);
  EXPECT_EQ(ReadPosition(LongInt::FromInt(-kPositionLimit)), -kPositionLimit);
  EXPECT_FALSE(ReadPosition(LongInt::FromInt(kPositionLimit + 1)).has_value());
  EXPECT_FALSE(ReadPosition(LongInt::FromInt(-kPositionLimit - 1)).has_value());
  EXPECT_EQ(ToPosition(Int::FromInt(9)).ToInt64(), 9);
  EXPECT_EQ(ToPosition(Byte8::FromInt(-9)).ToInt64(), -9);
  EXPECT_TRUE(ToPosition(Logic8{}).IsBitIdentical(Position{}));
  EXPECT_TRUE(ToPosition(LongInt::FromInt(kPositionLimit + 1))
                  .IsBitIdentical(Position{}));
  EXPECT_EQ(
      Slice<Bits4>(Bits8::FromInt(0xff), LongInt::FromInt(kPositionLimit + 1))
          .ToInt64(),
      0);
}

// The planes of a value of a type the code holding it was compiled without
// are loaded out of its bytes and stored back the same, both planes, at every
// layout a width takes.
TEST(IntegralTest, BothPlanesSurviveLoadingAndStoringAtEveryLayout) {
  const auto round_trip = [](const auto& value) {
    using T = std::remove_cvref_t<decltype(value)>;
    const LoadedWords loaded = LoadedWords::Load(&value, kShapeOf<T>);
    const typename T::Words words = value.Load();
    const ConstPlanes planes = loaded.Read();
    for (std::size_t i = 0; i < T::kWords; ++i) {
      EXPECT_EQ(planes.value[i], words.value.at(i));
      EXPECT_EQ(WordAt(planes.unknown, i), WordAt(words.unknown, i));
    }
    EXPECT_EQ(loaded.View().IsFourState(), T::kFourState);
    T stored = T::FromInt(0);
    loaded.StoreTo(&stored);
    EXPECT_TRUE(stored.IsBitIdentical(value));
  };
  round_trip(FromPlanes<Logic8>(0xA5, 0x3C));
  round_trip(FromPlanes<LogicVector<12>>(0xA5A, 0x3C3));
  round_trip(FromPlanes<LogicVector<32>>(0xDEADBEEF, 0x0F0F0F0F));
  round_trip(FromPlanes<LogicVector<64>>(0x0123456789ABCDEFULL, ~0ULL));
  round_trip(
      Logic100::FromWords(
          std::array<std::uint64_t, 2>{0xa000000000000000ULL, 0x5ULL},
          std::array<std::uint64_t, 2>{0x4000000000000000ULL, 0x2ULL}));
  round_trip(Byte8::FromInt(-3));
  round_trip(Int::FromInt(-3));
  round_trip(Signed100::FromInt(-3));
}

// A value wider than a word held as its words keeps both planes however it
// moves -- copied or moved into a holder as wide, a narrower one or a wider
// one -- and a value written into a holder as wide lands in the words the
// holder already has.
TEST(IntegralTest, AWideValueKeepsBothPlanesHoweverItMoves) {
  const Logic100 narrow = Logic100::FromWords(
      std::array<std::uint64_t, 2>{0xa000000000000000ULL, 0x5ULL},
      std::array<std::uint64_t, 2>{0x4000000000000000ULL, 0x2ULL});
  const LogicVector<200> wide = LogicVector<200>::FromWords(
      std::array<std::uint64_t, 4>{1, 2, 3, 0x5ULL},
      std::array<std::uint64_t, 4>{4, 0, 0, 0x2ULL});
  const WideLogicVector held_narrow(100, &narrow);
  const WideLogicVector held_wide(200, &wide);
  const auto holds = [](const WideLogicVector& held, const auto& value) {
    using T = std::remove_cvref_t<decltype(value)>;
    T read = T::FromInt(0);
    held.CopyInto(&read);
    return held.Width() == T::kWidth && held.ByteSize() == sizeof(T) &&
           read.IsBitIdentical(value);
  };
  EXPECT_TRUE(holds(held_narrow, narrow));
  EXPECT_TRUE(holds(held_wide, wide));

  const WideLogicVector copied_wide(held_wide);
  EXPECT_TRUE(holds(copied_wide, wide));
  WideLogicVector holder = held_narrow;
  holder = held_wide;
  EXPECT_TRUE(holds(holder, wide));
  holder = held_narrow;
  EXPECT_TRUE(holds(holder, narrow));
  const void* const bytes = holder.Bytes();
  holder = holder.Holding(&narrow);
  EXPECT_EQ(holder.Bytes(), bytes);
  WideLogicVector moved_from = held_wide;
  holder = std::move(moved_from);
  EXPECT_TRUE(holds(holder, wide));
  WideLogicVector moved_narrow = held_narrow;
  const WideLogicVector moved_into(std::move(moved_narrow));
  EXPECT_TRUE(holds(moved_into, narrow));

  EXPECT_TRUE(held_narrow.SameRepresentation(held_narrow.Holding(&narrow)));
  EXPECT_FALSE(held_narrow.SameRepresentation(held_wide));
  EXPECT_TRUE(held_wide.IsBitIdentical(copied_wide));
  EXPECT_FALSE(held_wide.IsBitIdentical(held_narrow));
  EXPECT_TRUE(held_wide.HasUnknown());
  EXPECT_FALSE(WideBitVector(100, &narrow).HasUnknown());
  EXPECT_TRUE(WideLogicVector().IsUninitialized());
}

}  // namespace
}  // namespace lyra::value
