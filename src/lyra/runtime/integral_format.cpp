#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/format.hpp"
#include "lyra/runtime/packed.hpp"

namespace lyra::runtime {

namespace {

auto WordCountFor(const IntegralValueView& v) -> std::size_t {
  return WordCountForBits(v.BitWidth());
}

auto ValueWordAt(const IntegralValueView& v, std::size_t word_index)
    -> std::uint64_t {
  return std::visit(
      Overloaded{
          [&](const NarrowIntegralView& n) -> std::uint64_t {
            return word_index == 0U ? n.value_word : 0U;
          },
          [&](const WideIntegralView& w) -> std::uint64_t {
            return word_index < w.value_words.size() ? w.value_words[word_index]
                                                     : 0U;
          },
      },
      v.data);
}

auto UnknownWordAt(const IntegralValueView& v, std::size_t word_index)
    -> std::uint64_t {
  return std::visit(
      Overloaded{
          [&](const NarrowIntegralView& n) -> std::uint64_t {
            return word_index == 0U ? n.unknown_word : 0U;
          },
          [&](const WideIntegralView& w) -> std::uint64_t {
            if (word_index >= w.unknown_words.size()) return 0U;
            return w.unknown_words[word_index];
          },
      },
      v.data);
}

auto ValueBit(const IntegralValueView& v, std::uint64_t bit_index) -> bool {
  const auto wi = static_cast<std::size_t>(bit_index / 64U);
  const std::uint64_t mask = std::uint64_t{1} << (bit_index % 64U);
  return (ValueWordAt(v, wi) & mask) != 0U;
}

auto UnknownBit(const IntegralValueView& v, std::uint64_t bit_index) -> bool {
  const auto wi = static_cast<std::size_t>(bit_index / 64U);
  const std::uint64_t mask = std::uint64_t{1} << (bit_index % 64U);
  return (UnknownWordAt(v, wi) & mask) != 0U;
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
auto HasX(const IntegralValueView& v) -> bool {
  if (v.StateKind() == IntegralStateKind::kTwoState) return false;
  const std::size_t wc = WordCountFor(v);
  for (std::size_t i = 0; i < wc; ++i) {
    const std::uint64_t mask =
        (i + 1U == wc) ? TopWordMask(v.BitWidth()) : ~std::uint64_t{0};
    if ((ValueWordAt(v, i) & UnknownWordAt(v, i) & mask) != 0U) {
      return true;
    }
  }
  return false;
}

// Z bit: value=0, unknown=1.
auto HasZ(const IntegralValueView& v) -> bool {
  if (v.StateKind() == IntegralStateKind::kTwoState) return false;
  const std::size_t wc = WordCountFor(v);
  for (std::size_t i = 0; i < wc; ++i) {
    const std::uint64_t mask =
        (i + 1U == wc) ? TopWordMask(v.BitWidth()) : ~std::uint64_t{0};
    if ((~ValueWordAt(v, i) & UnknownWordAt(v, i) & mask) != 0U) {
      return true;
    }
  }
  return false;
}

auto IsAllX(const IntegralValueView& v) -> bool {
  if (v.StateKind() == IntegralStateKind::kTwoState) return false;
  if (v.BitWidth() == 0U) return false;
  const std::size_t wc = WordCountFor(v);
  for (std::size_t i = 0; i < wc; ++i) {
    const std::uint64_t mask =
        (i + 1U == wc) ? TopWordMask(v.BitWidth()) : ~std::uint64_t{0};
    if ((ValueWordAt(v, i) & mask) != mask) return false;
    if ((UnknownWordAt(v, i) & mask) != mask) return false;
  }
  return true;
}

auto IsAllZ(const IntegralValueView& v) -> bool {
  if (v.StateKind() == IntegralStateKind::kTwoState) return false;
  if (v.BitWidth() == 0U) return false;
  const std::size_t wc = WordCountFor(v);
  for (std::size_t i = 0; i < wc; ++i) {
    const std::uint64_t mask =
        (i + 1U == wc) ? TopWordMask(v.BitWidth()) : ~std::uint64_t{0};
    if ((ValueWordAt(v, i) & mask) != 0U) return false;
    if ((UnknownWordAt(v, i) & mask) != mask) return false;
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

auto SummarizeUnknowns(const IntegralValueView& v) -> UnknownSummary {
  const bool x = HasX(v);
  const bool z = HasZ(v);
  if (!x && !z) return UnknownSummary::kNone;
  if (x && z) return UnknownSummary::kMixed;
  if (x) return IsAllX(v) ? UnknownSummary::kAllX : UnknownSummary::kPartialX;
  return IsAllZ(v) ? UnknownSummary::kAllZ : UnknownSummary::kPartialZ;
}

auto FormatBinaryBody(const IntegralValueView& v) -> std::string {
  std::string body;
  body.reserve(static_cast<std::size_t>(v.BitWidth()));
  for (std::uint64_t i = v.BitWidth(); i > 0U; --i) {
    const std::uint64_t bit_pos = i - 1U;
    const bool vb = ValueBit(v, bit_pos);
    const bool ub = UnknownBit(v, bit_pos);
    if (!ub) {
      body.push_back(vb ? '1' : '0');
    } else {
      body.push_back(vb ? 'x' : 'z');
    }
  }
  return body;
}

constexpr std::string_view kHexDigits = "0123456789abcdef";

auto FormatHexBody(const IntegralValueView& v) -> std::string {
  const std::uint64_t num_nibbles = (v.BitWidth() + 3U) / 4U;
  std::string body;
  body.reserve(static_cast<std::size_t>(num_nibbles));
  for (std::uint64_t n = num_nibbles; n > 0U; --n) {
    const std::uint64_t nibble_start = (n - 1U) * 4U;
    const std::uint64_t nibble_bits =
        std::min<std::uint64_t>(4U, v.BitWidth() - nibble_start);
    std::uint32_t nibble_val = 0;
    std::uint32_t nibble_unk = 0;
    for (std::uint64_t b = 0; b < nibble_bits; ++b) {
      if (ValueBit(v, nibble_start + b)) nibble_val |= (1U << b);
      if (UnknownBit(v, nibble_start + b)) nibble_unk |= (1U << b);
    }
    const std::uint32_t nibble_mask = (1U << nibble_bits) - 1U;
    if (nibble_unk == 0U) {
      body.push_back(kHexDigits[nibble_val]);
    } else if (nibble_unk == nibble_mask && nibble_val == nibble_mask) {
      body.push_back('x');
    } else if (nibble_unk == nibble_mask && nibble_val == 0U) {
      body.push_back('z');
    } else {
      body.push_back('X');
    }
  }
  return body;
}

auto FormatOctalBody(const IntegralValueView& v) -> std::string {
  const std::uint64_t num_octets = (v.BitWidth() + 2U) / 3U;
  std::string body;
  body.reserve(static_cast<std::size_t>(num_octets));
  for (std::uint64_t n = num_octets; n > 0U; --n) {
    const std::uint64_t octet_start = (n - 1U) * 3U;
    const std::uint64_t octet_bits =
        std::min<std::uint64_t>(3U, v.BitWidth() - octet_start);
    std::uint32_t octet_val = 0;
    std::uint32_t octet_unk = 0;
    for (std::uint64_t b = 0; b < octet_bits; ++b) {
      if (ValueBit(v, octet_start + b)) octet_val |= (1U << b);
      if (UnknownBit(v, octet_start + b)) octet_unk |= (1U << b);
    }
    const std::uint32_t octet_mask = (1U << octet_bits) - 1U;
    if (octet_unk == 0U) {
      body.push_back(static_cast<char>('0' + octet_val));
    } else if (octet_unk == octet_mask && octet_val == octet_mask) {
      body.push_back('x');
    } else if (octet_unk == octet_mask && octet_val == 0U) {
      body.push_back('z');
    } else {
      body.push_back('X');
    }
  }
  return body;
}

auto FormatDecimalNumeric(const IntegralValueView& v) -> std::string {
  if (!v.IsWide()) {
    const auto& n = std::get<NarrowIntegralView>(v.data);
    const std::uint64_t mask = (n.bit_width >= 64U)
                                   ? ~std::uint64_t{0}
                                   : (std::uint64_t{1} << n.bit_width) - 1U;
    const std::uint64_t raw = n.value_word & mask;
    if (n.is_signed) {
      std::int64_t s = 0;
      if (n.bit_width >= 64U) {
        s = static_cast<std::int64_t>(raw);
      } else {
        const std::uint64_t sign_bit = std::uint64_t{1} << (n.bit_width - 1U);
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

  const auto& w = std::get<WideIntegralView>(v.data);
  PackedWordVector words(w.value_words.begin(), w.value_words.end());
  MaskUnusedTopBits(
      std::span<std::uint64_t>{words.data(), words.size()}, w.bit_width);

  bool is_negative = false;
  if (w.is_signed) {
    const std::uint64_t sign_pos = w.bit_width - 1U;
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
          std::span<std::uint64_t>{words.data(), words.size()}, w.bit_width);
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

auto FormatDecimalBody(const IntegralValueView& v) -> std::string {
  switch (SummarizeUnknowns(v)) {
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
  return FormatDecimalNumeric(v);
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
      return -1;
  }
  return -1;
}

auto AutoPadChar(const IntegralValueView& v) -> char {
  if (IsAllX(v)) return 'x';
  if (IsAllZ(v)) return 'z';
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

auto FormatIntegral(const FormatSpec& spec, const IntegralValueView& v)
    -> std::string {
  std::string body;
  switch (spec.kind) {
    case FormatKind::kDecimal:
      body = FormatDecimalBody(v);
      break;
    case FormatKind::kHex:
      body = FormatHexBody(v);
      break;
    case FormatKind::kBinary:
      body = FormatBinaryBody(v);
      break;
    case FormatKind::kOctal:
      body = FormatOctalBody(v);
      break;
    case FormatKind::kString:
      throw InternalError(
          "FormatIntegral: kString must route through StringValueView");
  }

  if (spec.kind == FormatKind::kHex || spec.kind == FormatKind::kBinary ||
      spec.kind == FormatKind::kOctal) {
    body = StripLeadingZeros(std::move(body));
  }

  FormatSpec effective = spec;
  char pad_char = spec.zero_pad ? '0' : ' ';
  if (spec.width < 0) {
    const std::int32_t aw = AutoWidthFor(spec.kind, v.BitWidth());
    if (aw >= 0) {
      effective.width = aw;
      effective.zero_pad = true;
      pad_char = AutoPadChar(v);
    }
  }

  return ApplyWidthWithChar(std::move(body), effective, pad_char);
}

}  // namespace lyra::runtime
