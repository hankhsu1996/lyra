#pragma once

#include <string>

#include "lyra/value/format.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

// Integer-leaf format algorithm, reading the operand's planes directly -- the
// singular 4-state / sized / signed / X-Z-propagating algorithms (decimal,
// hex, octal, binary, char) all live behind this entry point.
[[nodiscard]] auto FormatIntegral(
    const FormatSpec& spec, const ConstIntegralView& value) -> std::string;

// %t (LRM 20.4.3): a count of the calling scope's time unit
// (`spec.timeunit_power`) written in the design-wide display unit
// (`tf.units_power`) with the `$timeformat` precision, suffix and field width.
// An integral count is written exactly, at any width; where it has unknown or
// high-impedance bits it is written as the decimal conversion writes it (LRM
// 21.2.1.3), a letter in place of the number.
[[nodiscard]] auto FormatIntegralTime(
    const FormatSpec& spec, const ConstIntegralView& value,
    const TimeFormat& tf) -> std::string;

[[nodiscard]] auto FormatRealTime(
    const FormatSpec& spec, double value, const TimeFormat& tf) -> std::string;

}  // namespace lyra::value
