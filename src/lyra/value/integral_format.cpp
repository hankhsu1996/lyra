#include "lyra/value/integral_format.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

namespace {

auto Pow10Double(int exp) -> double {
  double result = 1.0;
  for (int i = 0; i < exp; ++i) {
    result *= 10.0;
  }
  for (int i = 0; i < -exp; ++i) {
    result /= 10.0;
  }
  return result;
}

// Strip leading '0' digits, keeping at least one digit. 'x'/'z'/'X'/'Z' are
// never stripped.
auto StripLeadingZeros(std::string body) -> std::string {
  const auto first_nonzero = body.find_first_not_of('0');
  if (first_nonzero == std::string::npos) return "0";
  if (first_nonzero == 0) return body;
  return body.substr(first_nonzero);
}

// Which of x and z a value holds anywhere, and whether it holds nothing else.
struct UnknownScalars {
  bool any_x = false;
  bool any_z = false;
  bool all_x = true;
  bool all_z = true;
};

auto ScanUnknowns(const ConstIntegralView& value) -> UnknownScalars {
  UnknownScalars found;
  for (std::size_t i = 0; i < value.planes.value.size(); ++i) {
    const std::uint64_t mask = ValidBitsMask(i, value.width);
    const std::uint64_t v = value.planes.value[i];
    const std::uint64_t u = WordAt(value.planes.unknown, i);
    const std::uint64_t x_bits = v & u;
    const std::uint64_t z_bits = ~v & u & mask;
    found.any_x = found.any_x || x_bits != 0U;
    found.any_z = found.any_z || z_bits != 0U;
    found.all_x = found.all_x && x_bits == mask;
    found.all_z = found.all_z && z_bits == mask;
  }
  return found;
}

enum class UnknownSummary : std::uint8_t {
  kNone,
  kAllX,
  kAllZ,
  kPartialX,
  kPartialZ,
  kMixed,
};

auto SummarizeUnknowns(const ConstIntegralView& value) -> UnknownSummary {
  const UnknownScalars found = ScanUnknowns(value);
  if (!found.any_x && !found.any_z) return UnknownSummary::kNone;
  if (found.any_x && found.any_z) return UnknownSummary::kMixed;
  if (found.any_x) {
    return found.all_x ? UnknownSummary::kAllX : UnknownSummary::kPartialX;
  }
  return found.all_z ? UnknownSummary::kAllZ : UnknownSummary::kPartialZ;
}

enum class GroupSummary : std::uint8_t {
  kNone,
  kAllX,
  kAllZ,
  kPartialX,
  kPartialZ,
};

// LRM 21.2.1.3: per-group X/Z classification for hex/octal display.
// Any X in the group -> uppercase X (kPartialX); Z-only partial -> uppercase Z.
auto SummarizeGroup(
    std::uint64_t value_bits, std::uint64_t unknown_bits, std::uint64_t mask)
    -> GroupSummary {
  if (unknown_bits == 0U) return GroupSummary::kNone;
  const std::uint64_t x_bits = value_bits & unknown_bits;
  const std::uint64_t z_bits = (~value_bits) & unknown_bits & mask;
  if (x_bits == mask) return GroupSummary::kAllX;
  if (z_bits == mask) return GroupSummary::kAllZ;
  if (x_bits != 0U) return GroupSummary::kPartialX;
  return GroupSummary::kPartialZ;
}

constexpr std::string_view kHexDigits = "0123456789abcdef";

// The character one digit's bits print as: the digit itself, or the letter
// its x and z bits make it.
auto GroupDigit(
    std::uint64_t value_bits, std::uint64_t unknown_bits, std::uint64_t mask)
    -> char {
  switch (SummarizeGroup(value_bits, unknown_bits, mask)) {
    case GroupSummary::kNone:
      return kHexDigits[value_bits];
    case GroupSummary::kAllX:
      return 'x';
    case GroupSummary::kAllZ:
      return 'z';
    case GroupSummary::kPartialX:
      return 'X';
    case GroupSummary::kPartialZ:
      return 'Z';
  }
  std::unreachable();
}

// LRM 21.2.1.3: a value written out most significant digit first in a radix
// whose digits each cover a whole number of bits, the top digit covering
// whatever the width leaves of it.
auto FormatRadixBody(const ConstIntegralView& value, DigitRadix radix)
    -> std::string {
  const std::uint64_t bits_per_digit = BitsPerDigit(radix);
  const std::uint64_t digits =
      (value.width + bits_per_digit - 1U) / bits_per_digit;
  std::string body;
  body.reserve(static_cast<std::size_t>(digits));
  for (std::uint64_t n = digits; n > 0U; --n) {
    const std::uint64_t start = (n - 1U) * bits_per_digit;
    const std::uint64_t count =
        std::min<std::uint64_t>(bits_per_digit, value.width - start);
    body.push_back(GroupDigit(
        BitsAt(value.planes.value, start, count),
        BitsAt(value.planes.unknown, start, count), LowBits(count)));
  }
  return body;
}

auto FormatDecimalNumeric(const ConstIntegralView& value) -> std::string {
  const std::uint64_t bit_width = value.width;
  const bool is_signed = value.signedness == Signedness::kSigned;

  // A value of one word is the machine integer it holds.
  if (bit_width <= 64U) {
    if (is_signed) {
      return std::format(
          "{}", ToInt64(value.planes, bit_width, value.signedness));
    }
    return std::format("{}", WordAt(value.planes.value, 0));
  }

  // Wide path: copy into a working buffer, take the magnitude of a negative
  // number, then chunk-divide by 10^19 to build decimal digits group by group.
  std::vector<std::uint64_t> words(
      value.planes.value.begin(), value.planes.value.end());
  bool is_negative = false;
  if (is_signed && BitAt(words, bit_width - 1U)) {
    is_negative = true;
    std::uint64_t carry = 1;
    for (std::uint64_t& word : words) {
      word = ~word;
      const std::uint64_t sum = word + carry;
      carry = (sum < word) ? 1U : 0U;
      word = sum;
    }
    ClearAboveWidth(words, bit_width);
  }

  while (words.size() > 1U && words.back() == 0U) words.pop_back();
  if (words.size() == 1U && words[0] == 0U) return "0";

  // 10^19 is the largest power of 10 fitting in uint64_t.
  constexpr std::uint64_t kChunkDivisor = 10000000000000000000ULL;
  std::vector<std::uint64_t> chunks;
  while (words.size() > 1U || words[0] != 0U) {
    std::uint64_t remainder = 0;
    for (std::size_t i = words.size(); i > 0U; --i) {
      const auto combined =
          (static_cast<__uint128_t>(remainder) << 64U) | words[i - 1U];
      words[i - 1U] = static_cast<std::uint64_t>(combined / kChunkDivisor);
      remainder = static_cast<std::uint64_t>(combined % kChunkDivisor);
    }
    chunks.push_back(remainder);
    while (words.size() > 1U && words.back() == 0U) words.pop_back();
  }

  std::string out = is_negative ? "-" : "";
  out += std::format("{}", chunks.back());
  for (std::size_t i = chunks.size() - 1U; i > 0U; --i) {
    out += std::format("{:019}", chunks[i - 1U]);
  }
  return out;
}

// LRM 21.2.1.1 example: %c emits the low byte as an ASCII character (e.g.
// rval=101 -> 'e'). X/Z handling follows the Verilog simulator convention:
// any X bit in the low byte collapses to "x"; otherwise any Z bit collapses
// to "z"; otherwise the byte's value is the ASCII code.
auto FormatCharBody(const ConstIntegralView& value) -> std::string {
  const std::uint64_t bits = std::min<std::uint64_t>(8U, value.width);
  const std::uint64_t value_byte = BitsAt(value.planes.value, 0, bits);
  const std::uint64_t unknown_byte = BitsAt(value.planes.unknown, 0, bits);
  if (unknown_byte != 0U) {
    return (value_byte & unknown_byte) != 0U ? "x" : "z";
  }
  std::string out;
  out.push_back(static_cast<char>(value_byte));
  return out;
}

auto FormatStringBody(const ConstIntegralView& value) -> std::string {
  // LRM 21.2.1.7: the value's bytes render as ASCII characters, most
  // significant byte first. The LRM leaves a NUL byte's rendering unpinned;
  // match the de-facto convention -- a NUL (and an x/z byte, which reaches here
  // as 0x00) renders as a space, so it stays one column rather than vanishing.
  std::string out = BytesOf(value);
  for (char& c : out) {
    if (c == '\0') c = ' ';
  }
  return out;
}

auto FormatDecimalBody(const ConstIntegralView& value) -> std::string {
  switch (SummarizeUnknowns(value)) {
    case UnknownSummary::kNone:
      break;
    case UnknownSummary::kAllX:
      return "x";
    case UnknownSummary::kAllZ:
      return "z";
    case UnknownSummary::kPartialX:
      return "X";
    case UnknownSummary::kPartialZ:
      return "Z";
    case UnknownSummary::kMixed:
      return "X";
  }
  return FormatDecimalNumeric(value);
}

// Decimal digits of the largest value `bit_width` bits can hold, which is the
// floor of that value's base-10 logarithm plus one. `log10(2)` is irrational,
// so the product is never an integer, and it stays further from one than a
// double's error for any width a declaration can carry -- which is what makes
// truncating it the floor rather than an approximation of it.
auto DecimalDigitsOfWidest(std::uint64_t bit_width) -> std::uint64_t {
  constexpr double kLog10Of2 = 0.30102999566398119521;
  return static_cast<std::uint64_t>(
             static_cast<double>(bit_width) * kLog10Of2) +
         1U;
}

// LRM 21.2.1.2: with no field width written, a conversion is given the columns
// the largest value the operand's type can hold occupies in that radix, so no
// value has to expand its field. A signed decimal needs one column more than
// that, for the sign its most negative value carries; the other radices print
// every bit pattern in the same width whatever the type's signedness.
auto AutoWidthFor(FormatKind kind, const ConstIntegralView& value)
    -> std::int32_t {
  const std::uint64_t bit_width = value.width;
  switch (kind) {
    case FormatKind::kHex:
      return static_cast<std::int32_t>((bit_width + 3U) / 4U);
    case FormatKind::kBinary:
      return static_cast<std::int32_t>(bit_width);
    case FormatKind::kOctal:
      return static_cast<std::int32_t>((bit_width + 2U) / 3U);
    case FormatKind::kDecimal:
      return static_cast<std::int32_t>(
          DecimalDigitsOfWidest(bit_width) +
          (value.signedness == Signedness::kSigned ? 1U : 0U));
    case FormatKind::kString:
    case FormatKind::kChar:
    case FormatKind::kRealDecimal:
    case FormatKind::kRealExponential:
    case FormatKind::kRealGeneral:
    case FormatKind::kAssignmentPattern:
    case FormatKind::kTime:
      return -1;
  }
  std::unreachable();
}

}  // namespace

auto FormatTimeMagnitude(
    const FormatSpec& spec, double magnitude, const TimeFormat& tf)
    -> std::string {
  const double scaled =
      magnitude * Pow10Double(spec.timeunit_power - tf.units_power);
  const int precision = tf.precision >= 0 ? tf.precision : 0;
  std::string body = std::format("{:.{}f}", scaled, precision);
  body += tf.suffix;
  if (tf.min_width > 0 &&
      body.size() < static_cast<std::size_t>(tf.min_width)) {
    body.insert(0, static_cast<std::size_t>(tf.min_width) - body.size(), ' ');
  }
  return body;
}

auto FormatIntegral(const FormatSpec& spec, const ConstIntegralView& value)
    -> std::string {
  std::string body;
  switch (spec.kind) {
    case FormatKind::kDecimal:
      body = FormatDecimalBody(value);
      break;
    case FormatKind::kHex:
      body = StripLeadingZeros(FormatRadixBody(value, DigitRadix::kHex));
      break;
    case FormatKind::kBinary:
      body = StripLeadingZeros(FormatRadixBody(value, DigitRadix::kBinary));
      break;
    case FormatKind::kOctal:
      body = StripLeadingZeros(FormatRadixBody(value, DigitRadix::kOctal));
      break;
    case FormatKind::kChar:
      body = FormatCharBody(value);
      break;
    case FormatKind::kString:
      body = FormatStringBody(value);
      break;
    case FormatKind::kRealDecimal:
    case FormatKind::kRealExponential:
    case FormatKind::kRealGeneral:
      throw InternalError(
          "FormatIntegral: real format kinds must route through "
          "Formatter<double> / Formatter<float>");
    case FormatKind::kAssignmentPattern:
      throw InternalError(
          "FormatIntegral: kAssignmentPattern must be rewritten to kDecimal "
          "by the caller before reaching FormatIntegral");
    case FormatKind::kTime:
      throw InternalError(
          "FormatIntegral: kTime must route through FormatTimeMagnitude");
  }

  FormatSpec effective = spec;
  if (effective.width < 0) {
    effective.width = AutoWidthFor(spec.kind, value);
  }
  return ApplyFieldWidth(std::move(body), effective);
}

}  // namespace lyra::value
