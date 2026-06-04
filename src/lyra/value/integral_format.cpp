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
#include "lyra/value/packed.hpp"
#include "lyra/value/packed_array.hpp"

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

auto StateKindOf(const PackedArray& pa) -> IntegralStateKind {
  return pa.IsFourState() ? IntegralStateKind::kFourState
                          : IntegralStateKind::kTwoState;
}

auto WordCountFor(const PackedArray& pa) -> std::size_t {
  return WordCountForBits(pa.BitWidth());
}

auto ValueWordAt(const PackedArray& pa, std::size_t word_index)
    -> std::uint64_t {
  const auto words = pa.ValueWords();
  return word_index < words.size() ? words[word_index] : 0U;
}

auto UnknownWordAt(const PackedArray& pa, std::size_t word_index)
    -> std::uint64_t {
  const auto words = pa.UnknownWords();
  return word_index < words.size() ? words[word_index] : 0U;
}

auto ValueBit(const PackedArray& pa, std::uint64_t bit_index) -> bool {
  const auto wi = static_cast<std::size_t>(bit_index / 64U);
  const std::uint64_t mask = std::uint64_t{1} << (bit_index % 64U);
  return (ValueWordAt(pa, wi) & mask) != 0U;
}

auto UnknownBit(const PackedArray& pa, std::uint64_t bit_index) -> bool {
  const auto wi = static_cast<std::size_t>(bit_index / 64U);
  const std::uint64_t mask = std::uint64_t{1} << (bit_index % 64U);
  return (UnknownWordAt(pa, wi) & mask) != 0U;
}

auto TopWordMask(std::uint64_t bit_width) -> std::uint64_t {
  const std::uint64_t used = bit_width % 64U;
  if (used == 0U) return ~std::uint64_t{0};
  return (std::uint64_t{1} << used) - 1U;
}

// Strip leading '0' digits, keeping at least one digit. 'x'/'z'/'X'/'Z' are
// never stripped.
auto StripLeadingZeros(std::string body) -> std::string {
  const auto first_nonzero = body.find_first_not_of('0');
  if (first_nonzero == std::string::npos) return "0";
  if (first_nonzero == 0) return body;
  return body.substr(first_nonzero);
}

// X bit: value=1, unknown=1.
auto HasX(const PackedArray& pa) -> bool {
  if (StateKindOf(pa) == IntegralStateKind::kTwoState) return false;
  const std::size_t wc = WordCountFor(pa);
  for (std::size_t i = 0; i < wc; ++i) {
    const std::uint64_t mask =
        (i + 1U == wc) ? TopWordMask(pa.BitWidth()) : ~std::uint64_t{0};
    if ((ValueWordAt(pa, i) & UnknownWordAt(pa, i) & mask) != 0U) {
      return true;
    }
  }
  return false;
}

// Z bit: value=0, unknown=1.
auto HasZ(const PackedArray& pa) -> bool {
  if (StateKindOf(pa) == IntegralStateKind::kTwoState) return false;
  const std::size_t wc = WordCountFor(pa);
  for (std::size_t i = 0; i < wc; ++i) {
    const std::uint64_t mask =
        (i + 1U == wc) ? TopWordMask(pa.BitWidth()) : ~std::uint64_t{0};
    if ((~ValueWordAt(pa, i) & UnknownWordAt(pa, i) & mask) != 0U) {
      return true;
    }
  }
  return false;
}

auto IsAllX(const PackedArray& pa) -> bool {
  if (StateKindOf(pa) == IntegralStateKind::kTwoState) return false;
  if (pa.BitWidth() == 0U) return false;
  const std::size_t wc = WordCountFor(pa);
  for (std::size_t i = 0; i < wc; ++i) {
    const std::uint64_t mask =
        (i + 1U == wc) ? TopWordMask(pa.BitWidth()) : ~std::uint64_t{0};
    if ((ValueWordAt(pa, i) & mask) != mask) return false;
    if ((UnknownWordAt(pa, i) & mask) != mask) return false;
  }
  return true;
}

auto IsAllZ(const PackedArray& pa) -> bool {
  if (StateKindOf(pa) == IntegralStateKind::kTwoState) return false;
  if (pa.BitWidth() == 0U) return false;
  const std::size_t wc = WordCountFor(pa);
  for (std::size_t i = 0; i < wc; ++i) {
    const std::uint64_t mask =
        (i + 1U == wc) ? TopWordMask(pa.BitWidth()) : ~std::uint64_t{0};
    if ((ValueWordAt(pa, i) & mask) != 0U) return false;
    if ((UnknownWordAt(pa, i) & mask) != mask) return false;
  }
  return true;
}

enum class UnknownSummary : std::uint8_t {
  kNone,
  kAllX,
  kAllZ,
  kPartialX,
  kPartialZ,
  kMixed,
};

auto SummarizeUnknowns(const PackedArray& pa) -> UnknownSummary {
  const bool x = HasX(pa);
  const bool z = HasZ(pa);
  if (!x && !z) return UnknownSummary::kNone;
  if (x && z) return UnknownSummary::kMixed;
  if (x) return IsAllX(pa) ? UnknownSummary::kAllX : UnknownSummary::kPartialX;
  return IsAllZ(pa) ? UnknownSummary::kAllZ : UnknownSummary::kPartialZ;
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
    std::uint32_t value_bits, std::uint32_t unknown_bits, std::uint32_t mask)
    -> GroupSummary {
  if (unknown_bits == 0U) return GroupSummary::kNone;
  const std::uint32_t x_bits = value_bits & unknown_bits;
  const std::uint32_t z_bits = (~value_bits) & unknown_bits & mask;
  if (x_bits == mask) return GroupSummary::kAllX;
  if (z_bits == mask) return GroupSummary::kAllZ;
  if (x_bits != 0U) return GroupSummary::kPartialX;
  return GroupSummary::kPartialZ;
}

// Returns the X/Z letter for an unknown group; caller dispatches kNone to the
// radix-specific digit lookup (hex digits for %h, '0'..'7' for %o).
auto UnknownLetterForGroup(GroupSummary s) -> char {
  switch (s) {
    case GroupSummary::kAllX:
      return 'x';
    case GroupSummary::kAllZ:
      return 'z';
    case GroupSummary::kPartialX:
      return 'X';
    case GroupSummary::kPartialZ:
      return 'Z';
    case GroupSummary::kNone:
      break;
  }
  throw InternalError(
      "UnknownLetterForGroup: kNone has no letter; caller must dispatch");
}

auto FormatBinaryBody(const PackedArray& pa) -> std::string {
  std::string body;
  body.reserve(static_cast<std::size_t>(pa.BitWidth()));
  for (std::uint64_t i = pa.BitWidth(); i > 0U; --i) {
    const std::uint64_t bit_pos = i - 1U;
    const bool vb = ValueBit(pa, bit_pos);
    const bool ub = UnknownBit(pa, bit_pos);
    if (!ub) {
      body.push_back(vb ? '1' : '0');
    } else {
      body.push_back(vb ? 'x' : 'z');
    }
  }
  return body;
}

constexpr std::string_view kHexDigits = "0123456789abcdef";

auto FormatHexBody(const PackedArray& pa) -> std::string {
  const std::uint64_t num_nibbles = (pa.BitWidth() + 3U) / 4U;
  std::string body;
  body.reserve(static_cast<std::size_t>(num_nibbles));
  for (std::uint64_t n = num_nibbles; n > 0U; --n) {
    const std::uint64_t nibble_start = (n - 1U) * 4U;
    const std::uint64_t nibble_bits =
        std::min<std::uint64_t>(4U, pa.BitWidth() - nibble_start);
    std::uint32_t nibble_val = 0;
    std::uint32_t nibble_unk = 0;
    for (std::uint64_t b = 0; b < nibble_bits; ++b) {
      if (ValueBit(pa, nibble_start + b)) nibble_val |= (1U << b);
      if (UnknownBit(pa, nibble_start + b)) nibble_unk |= (1U << b);
    }
    const std::uint32_t nibble_mask = (1U << nibble_bits) - 1U;
    const auto summary = SummarizeGroup(nibble_val, nibble_unk, nibble_mask);
    if (summary == GroupSummary::kNone) {
      body.push_back(kHexDigits[nibble_val]);
    } else {
      body.push_back(UnknownLetterForGroup(summary));
    }
  }
  return body;
}

auto FormatOctalBody(const PackedArray& pa) -> std::string {
  const std::uint64_t num_octets = (pa.BitWidth() + 2U) / 3U;
  std::string body;
  body.reserve(static_cast<std::size_t>(num_octets));
  for (std::uint64_t n = num_octets; n > 0U; --n) {
    const std::uint64_t octet_start = (n - 1U) * 3U;
    const std::uint64_t octet_bits =
        std::min<std::uint64_t>(3U, pa.BitWidth() - octet_start);
    std::uint32_t octet_val = 0;
    std::uint32_t octet_unk = 0;
    for (std::uint64_t b = 0; b < octet_bits; ++b) {
      if (ValueBit(pa, octet_start + b)) octet_val |= (1U << b);
      if (UnknownBit(pa, octet_start + b)) octet_unk |= (1U << b);
    }
    const std::uint32_t octet_mask = (1U << octet_bits) - 1U;
    const auto summary = SummarizeGroup(octet_val, octet_unk, octet_mask);
    if (summary == GroupSummary::kNone) {
      body.push_back(static_cast<char>('0' + octet_val));
    } else {
      body.push_back(UnknownLetterForGroup(summary));
    }
  }
  return body;
}

auto FormatDecimalNumeric(const PackedArray& pa) -> std::string {
  const std::uint64_t bit_width = pa.BitWidth();
  const bool is_signed = pa.IsSigned();

  // Narrow path: <= 64 bits fit in a single uint64. Avoids the chunked
  // bignum loop below for the common 32 / 64-bit cases.
  if (bit_width <= 64U) {
    const std::uint64_t value_word = ValueWordAt(pa, 0);
    const std::uint64_t mask = (bit_width >= 64U)
                                   ? ~std::uint64_t{0}
                                   : (std::uint64_t{1} << bit_width) - 1U;
    const std::uint64_t raw = value_word & mask;
    if (is_signed) {
      std::int64_t s = 0;
      if (bit_width >= 64U) {
        s = static_cast<std::int64_t>(raw);
      } else {
        const std::uint64_t sign_bit = std::uint64_t{1} << (bit_width - 1U);
        if ((raw & sign_bit) != 0U) {
          s = static_cast<std::int64_t>(raw | ~mask);
        } else {
          s = static_cast<std::int64_t>(raw);
        }
      }
      return std::format("{}", s);
    }
    return std::format("{}", raw);
  }

  // Wide path: copy into a working buffer, sign-extend if needed, then
  // chunk-divide by 10^19 to build decimal digits group-by-group.
  const auto value_words = pa.ValueWords();
  PackedWordVector words(value_words.begin(), value_words.end());
  MaskUnusedTopBits(
      std::span<std::uint64_t>{words.data(), words.size()}, bit_width);

  bool is_negative = false;
  if (is_signed) {
    const std::uint64_t sign_pos = bit_width - 1U;
    const auto sign_word = static_cast<std::size_t>(sign_pos / 64U);
    const std::uint64_t sign_mask = std::uint64_t{1} << (sign_pos % 64U);
    if (sign_word < words.size() && (words[sign_word] & sign_mask) != 0U) {
      is_negative = true;
      std::uint64_t carry = 1;
      for (std::uint64_t& word : words) {
        word = ~word;
        const std::uint64_t sum = word + carry;
        carry = (sum < word) ? 1U : 0U;
        word = sum;
      }
      MaskUnusedTopBits(
          std::span<std::uint64_t>{words.data(), words.size()}, bit_width);
    }
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
auto FormatCharBody(const PackedArray& pa) -> std::string {
  std::uint8_t value_byte = 0;
  std::uint8_t unknown_byte = 0;
  const std::uint64_t bits = std::min<std::uint64_t>(8U, pa.BitWidth());
  for (std::uint64_t i = 0; i < bits; ++i) {
    if (ValueBit(pa, i)) {
      value_byte |= static_cast<std::uint8_t>(1U << i);
    }
    if (UnknownBit(pa, i)) {
      unknown_byte |= static_cast<std::uint8_t>(1U << i);
    }
  }
  if (unknown_byte != 0U) {
    const std::uint8_t x_bits = value_byte & unknown_byte;
    return x_bits != 0U ? "x" : "z";
  }
  std::string out;
  out.push_back(static_cast<char>(value_byte));
  return out;
}

auto FormatDecimalBody(const PackedArray& pa) -> std::string {
  switch (SummarizeUnknowns(pa)) {
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
  return FormatDecimalNumeric(pa);
}

auto AutoWidthFor(FormatKind kind, std::uint64_t bit_width) -> std::int32_t {
  switch (kind) {
    case FormatKind::kHex:
      return static_cast<std::int32_t>((bit_width + 3U) / 4U);
    case FormatKind::kBinary:
      return static_cast<std::int32_t>(bit_width);
    case FormatKind::kOctal:
      return static_cast<std::int32_t>((bit_width + 2U) / 3U);
    case FormatKind::kDecimal:
    case FormatKind::kString:
    case FormatKind::kChar:
    case FormatKind::kRealDecimal:
    case FormatKind::kRealExponential:
    case FormatKind::kRealGeneral:
    case FormatKind::kAssignmentPattern:
    case FormatKind::kTime:
      return -1;
  }
  return -1;
}

auto AutoPadChar(const PackedArray& pa) -> char {
  if (IsAllX(pa)) return 'x';
  if (IsAllZ(pa)) return 'z';
  return '0';
}

auto ApplyWidthWithChar(std::string body, const FormatSpec& spec, char pad_char)
    -> std::string {
  if (spec.width <= 0) return body;
  const auto target = static_cast<std::size_t>(spec.width);
  if (body.size() >= target) return body;
  const std::size_t pad = target - body.size();
  if (spec.left_align) {
    body.append(pad, ' ');
    return body;
  }
  // Sign before zero pad: %05d of -5 -> "-0005", not "000-5".
  if (pad_char == '0' && !body.empty() && body[0] == '-') {
    return "-" + std::string(pad, '0') + body.substr(1);
  }
  return std::string(pad, pad_char) + std::move(body);
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

auto FormatIntegral(const FormatSpec& spec, const PackedArray& value)
    -> std::string {
  std::string body;
  switch (spec.kind) {
    case FormatKind::kDecimal:
      body = FormatDecimalBody(value);
      break;
    case FormatKind::kHex:
      body = FormatHexBody(value);
      break;
    case FormatKind::kBinary:
      body = FormatBinaryBody(value);
      break;
    case FormatKind::kOctal:
      body = FormatOctalBody(value);
      break;
    case FormatKind::kChar:
      body = FormatCharBody(value);
      break;
    case FormatKind::kString:
      throw InternalError(
          "FormatIntegral: kString must route through Formatter<String>");
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

  if (spec.kind == FormatKind::kHex || spec.kind == FormatKind::kBinary ||
      spec.kind == FormatKind::kOctal) {
    body = StripLeadingZeros(std::move(body));
  }

  FormatSpec effective = spec;
  char pad_char = spec.zero_pad ? '0' : ' ';
  if (spec.width < 0) {
    const std::int32_t aw = AutoWidthFor(spec.kind, value.BitWidth());
    if (aw >= 0) {
      effective.width = aw;
      effective.zero_pad = true;
      pad_char = AutoPadChar(value);
    }
  }

  return ApplyWidthWithChar(std::move(body), effective, pad_char);
}

}  // namespace lyra::value
