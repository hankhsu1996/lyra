#pragma once

#include <string>

#include "lyra/value/format.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::value {

// Integer-leaf format algorithm, reading the operand's planes directly -- the
// singular 4-state / sized / signed / X-Z-propagating algorithms (decimal,
// hex, octal, binary, char) all live behind this entry point.
[[nodiscard]] auto FormatIntegral(
    const FormatSpec& spec, const ConstIntegralView& value) -> std::string;

// LRM 21.2.1.3 %t. Rescales `magnitude` from the calling scope's time unit
// (`spec.timeunit_power`) to the design-wide display unit (`tf.units_power`),
// then formats with the `$timeformat` precision / suffix / min field width.
// The per-type formatter extracts the magnitude (uint64 low-word for
// integral, the `double`/`float` value for real) and hands it here.
[[nodiscard]] auto FormatTimeMagnitude(
    const FormatSpec& spec, double magnitude, const TimeFormat& tf)
    -> std::string;

}  // namespace lyra::value
