#include <array>
#include <cstdint>
#include <gtest/gtest.h>
#include <string>

#include "lyra/value/format.hpp"
#include "lyra/value/integral_format.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {
namespace {

auto Microseconds(std::int32_t precision) -> TimeFormat {
  return TimeFormat{
      .units_power = -6, .precision = precision, .suffix = "", .min_width = 0};
}

// How %t writes a count of nanoseconds in microseconds at a precision. The
// standard gives `$timeformat`'s precision only as a count of fraction digits,
// so what happens to the digits past it is the tool's choice; these pin Lyra's,
// which is the one a real operand's already is.
auto MicrosecondsOf(std::int64_t nanoseconds, std::int32_t precision)
    -> std::string {
  const std::array<std::uint64_t, 1> words{
      static_cast<std::uint64_t>(nanoseconds)};
  const ConstIntegralView value{
      .value = words,
      .unknown = {},
      .width = 64,
      .signedness = Signedness::kSigned};
  FormatSpec spec;
  spec.kind = FormatKind::kTime;
  spec.width = 0;
  spec.timeunit_power = -9;
  return FormatIntegralTime(spec, value, Microseconds(precision));
}

TEST(TimeFormat, DigitsPastThePrecisionRoundToTheNearest) {
  EXPECT_EQ(MicrosecondsOf(2499, 0), "2");
  EXPECT_EQ(MicrosecondsOf(2501, 0), "3");
  EXPECT_EQ(MicrosecondsOf(1234567, 2), "1234.57");
  EXPECT_EQ(MicrosecondsOf(9995, 2), "10.00");
  EXPECT_EQ(MicrosecondsOf(15, 2), "0.02");
}

TEST(TimeFormat, AnExactHalfGoesToTheEvenDigitAsARealOperandsDoes) {
  EXPECT_EQ(MicrosecondsOf(2500, 0), "2");
  EXPECT_EQ(MicrosecondsOf(3500, 0), "4");
  EXPECT_EQ(MicrosecondsOf(5, 2), "0.00");

  FormatSpec spec;
  spec.kind = FormatKind::kTime;
  spec.width = 0;
  spec.timeunit_power = -9;
  EXPECT_EQ(FormatRealTime(spec, 2500.0, Microseconds(0)), "2");
}

TEST(TimeFormat, ANegativeCountRoundingToZeroHasNoSign) {
  EXPECT_EQ(MicrosecondsOf(-5, 0), "0");
  EXPECT_EQ(MicrosecondsOf(-2600, 0), "-3");
}

}  // namespace
}  // namespace lyra::value
