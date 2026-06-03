#include "sv_literal.hpp"

#include <cctype>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

#include "lyra/value/format.hpp"

namespace lyra::test {

namespace {

auto IsAsciiDigit(char c) -> bool {
  return c >= '0' && c <= '9';
}

// Parse the leading run of decimal digits. Returns the parsed value and the
// number of characters consumed, or an error if there are no digits or the
// value does not fit.
auto ParseLeadingDecimal(std::string_view text)
    -> std::expected<std::pair<std::uint64_t, std::size_t>, std::string> {
  std::size_t i = 0;
  while (i < text.size() && IsAsciiDigit(text[i])) {
    ++i;
  }
  if (i == 0) {
    return std::unexpected(std::string{"expected decimal digits"});
  }
  std::uint64_t value = 0;
  auto result =
      std::from_chars(text.data(), text.data() + i, value, 10);  // NOLINT
  if (result.ec != std::errc{}) {
    return std::unexpected(
        std::format(
            "decimal value out of range: {}", std::string{text.substr(0, i)}));
  }
  return std::pair{value, i};
}

auto DigitValueForBase(char c, int base) -> int {
  if (base == 2) {
    if (c == '0') return 0;
    if (c == '1') return 1;
    return -1;
  }
  if (base == 8) {
    if (c >= '0' && c <= '7') return c - '0';
    return -1;
  }
  if (base == 10) {
    if (c >= '0' && c <= '9') return c - '0';
    return -1;
  }
  if (base == 16) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
    if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
    return -1;
  }
  return -1;
}

auto BitsPerDigit(int base) -> int {
  switch (base) {
    case 2:
      return 1;
    case 8:
      return 3;
    case 16:
      return 4;
    default:
      return 0;  // decimal handled separately
  }
}

void EnsureWordsLarge(
    std::vector<std::uint64_t>& words, std::uint64_t bit_width) {
  const auto needed = static_cast<std::size_t>((bit_width + 63U) / 64U);
  if (words.size() < needed) {
    words.resize(needed, 0U);
  }
}

void SetValueBit(std::vector<std::uint64_t>& words, std::uint64_t bit_index) {
  const auto wi = static_cast<std::size_t>(bit_index / 64U);
  words[wi] |= (std::uint64_t{1} << (bit_index % 64U));
}

auto ParseBaseChar(char c) -> std::expected<int, std::string> {
  switch (c) {
    case 'b':
    case 'B':
      return 2;
    case 'o':
    case 'O':
      return 8;
    case 'd':
    case 'D':
      return 10;
    case 'h':
    case 'H':
      return 16;
    default:
      return std::unexpected(
          std::format("unknown SV literal base '{}'", std::string(1, c)));
  }
}

auto FormatKindForBase(int base) -> value::FormatKind {
  switch (base) {
    case 2:
      return value::FormatKind::kBinary;
    case 8:
      return value::FormatKind::kOctal;
    case 16:
      return value::FormatKind::kHex;
    case 10:
    default:
      return value::FormatKind::kDecimal;
  }
}

auto SvFormatSpecifierForBase(int base) -> std::string {
  switch (base) {
    case 2:
      return "%b";
    case 8:
      return "%o";
    case 16:
      return "%h";
    case 10:
    default:
      return "%0d";
  }
}

// Parse the digit body of a power-of-two-base SV literal into value/unknown
// bit words. Returns words sized to ceil(bit_width / 64), and a 4-state flag
// (true if any `x`/`z` digit was seen).
auto ParseBinaryFamilyBody(
    std::string_view body, std::uint64_t bit_width, int base)
    -> std::expected<
        std::tuple<
            std::vector<std::uint64_t>, std::vector<std::uint64_t>, bool>,
        std::string> {
  std::vector<std::uint64_t> value_words;
  std::vector<std::uint64_t> unknown_words;
  EnsureWordsLarge(value_words, bit_width);
  EnsureWordsLarge(unknown_words, bit_width);

  const int per_digit = BitsPerDigit(base);
  bool four_state = false;

  // Filter out '_' separators while iterating right-to-left over the body.
  std::vector<char> chars;
  chars.reserve(body.size());
  for (char c : body) {
    if (c == '_') continue;
    chars.push_back(c);
  }

  std::uint64_t bit_pos = 0;
  for (const char c : std::ranges::reverse_view(chars)) {
    if (c == 'x' || c == 'X' || c == 'z' || c == 'Z') {
      // Runtime 4-state encoding (see runtime/integral_format.cpp):
      //   X bit: value=1, unknown=1
      //   Z bit: value=0, unknown=1
      four_state = true;
      const bool is_x = (c == 'x' || c == 'X');
      for (int b = 0; b < per_digit; ++b) {
        if (bit_pos + static_cast<std::uint64_t>(b) >= bit_width) break;
        SetValueBit(unknown_words, bit_pos + static_cast<std::uint64_t>(b));
        if (is_x) {
          SetValueBit(value_words, bit_pos + static_cast<std::uint64_t>(b));
        }
      }
      bit_pos += static_cast<std::uint64_t>(per_digit);
      continue;
    }
    const int dv = DigitValueForBase(c, base);
    if (dv < 0) {
      return std::unexpected(
          std::format(
              "invalid digit '{}' for base {}", std::string(1, c), base));
    }
    for (int b = 0; b < per_digit; ++b) {
      if (bit_pos + static_cast<std::uint64_t>(b) >= bit_width) break;
      if ((dv & (1 << b)) != 0) {
        SetValueBit(value_words, bit_pos + static_cast<std::uint64_t>(b));
      }
    }
    bit_pos += static_cast<std::uint64_t>(per_digit);
  }

  return std::tuple{
      std::move(value_words), std::move(unknown_words), four_state};
}

auto ParseDecimalBody(std::string_view body, std::uint64_t bit_width)
    -> std::expected<std::vector<std::uint64_t>, std::string> {
  // Decimal SV literals are restricted: no `_` separators are allowed for
  // this phase's needs, but they show up enough in real SV to support. No
  // 4-state digits in decimal.
  std::vector<std::uint64_t> value_words;
  EnsureWordsLarge(value_words, bit_width);

  std::vector<char> chars;
  chars.reserve(body.size());
  for (char c : body) {
    if (c == '_') continue;
    if (!IsAsciiDigit(c)) {
      return std::unexpected(
          std::format(
              "invalid digit '{}' in decimal SV literal", std::string(1, c)));
    }
    chars.push_back(c);
  }
  if (chars.empty()) {
    return std::unexpected(std::string{"decimal SV literal has no digits"});
  }
  std::uint64_t value = 0;
  auto result = std::from_chars(
      chars.data(), chars.data() + chars.size(), value, 10);  // NOLINT
  if (result.ec != std::errc{}) {
    return std::unexpected(std::string{"decimal SV literal value too large"});
  }
  if (bit_width == 0) {
    return std::unexpected(std::string{"SV literal width must be positive"});
  }
  if (bit_width >= 64U) {
    value_words[0] = value;
  } else {
    const std::uint64_t mask = (std::uint64_t{1} << bit_width) - 1U;
    value_words[0] = value & mask;
  }
  return value_words;
}

// Try to parse `text` as an SV literal. Returns nullopt result (not error) if
// the form does not match the SV literal pattern at all.
auto TryParseSvLiteral(std::string_view text)
    -> std::expected<std::optional<ExpectedValue>, std::string> {
  if (text.empty() || !IsAsciiDigit(text.front())) {
    return std::optional<ExpectedValue>{};
  }
  auto width_or = ParseLeadingDecimal(text);
  if (!width_or) return std::unexpected(width_or.error());
  const auto [width, width_chars] = *width_or;
  if (width_chars >= text.size() || text[width_chars] != '\'') {
    return std::optional<ExpectedValue>{};
  }
  std::size_t i = width_chars + 1;
  bool is_signed = false;
  if (i < text.size() && (text[i] == 's' || text[i] == 'S')) {
    is_signed = true;
    ++i;
  }
  if (i >= text.size()) {
    return std::unexpected(std::string{"SV literal: missing base character"});
  }
  auto base_or = ParseBaseChar(text[i]);
  if (!base_or) return std::unexpected(base_or.error());
  ++i;
  if (i >= text.size()) {
    return std::unexpected(std::string{"SV literal: missing digit body"});
  }
  const auto body = text.substr(i);

  ExpectedValue out;
  out.kind = ExpectedValueKind::kSvLiteral;
  out.bit_width = width;
  out.is_signed = is_signed;
  out.format_spec.kind = FormatKindForBase(*base_or);
  out.sv_format_specifier = SvFormatSpecifierForBase(*base_or);

  if (*base_or == 10) {
    auto words_or = ParseDecimalBody(body, width);
    if (!words_or) return std::unexpected(words_or.error());
    out.value_words = std::move(*words_or);
    out.unknown_words.assign(out.value_words.size(), 0U);
    out.state_kind = value::IntegralStateKind::kTwoState;
  } else {
    auto bits_or = ParseBinaryFamilyBody(body, width, *base_or);
    if (!bits_or) return std::unexpected(bits_or.error());
    auto& [vw, uw, four_state] = *bits_or;
    out.value_words = std::move(vw);
    out.unknown_words = std::move(uw);
    out.state_kind = four_state ? value::IntegralStateKind::kFourState
                                : value::IntegralStateKind::kTwoState;
  }
  return std::optional<ExpectedValue>{std::move(out)};
}

}  // namespace

auto ExpectedValue::BuildView() const -> value::RuntimeValueView {
  switch (kind) {
    case ExpectedValueKind::kIntegerScalar: {
      const auto word = static_cast<std::uint64_t>(integer_value);
      return value::RuntimeValueView{
          .data = value::IntegralValueView::Narrow(
              word, 0U, 32U, value::IntegralStateKind::kTwoState, true)};
    }
    case ExpectedValueKind::kSvLiteral: {
      if (value_words.size() == 1) {
        const std::uint64_t unk = unknown_words.empty() ? 0U : unknown_words[0];
        return value::RuntimeValueView{
            .data = value::IntegralValueView::Narrow(
                value_words[0], unk, bit_width, state_kind, is_signed)};
      }
      const std::span<const std::uint64_t> unk_span =
          state_kind == value::IntegralStateKind::kTwoState
              ? std::span<const std::uint64_t>{}
              : std::span<const std::uint64_t>(unknown_words);
      return value::RuntimeValueView{
          .data = value::IntegralValueView::Wide(
              std::span<const std::uint64_t>(value_words), unk_span, bit_width,
              state_kind, is_signed)};
    }
    case ExpectedValueKind::kStringScalar:
      return value::RuntimeValueView{
          .data = value::StringValueView{
              .data = string_value.data(),
              .size = static_cast<std::uint32_t>(string_value.size())}};
    case ExpectedValueKind::kUnpackedArray: {
      std::vector<value::RuntimeValueView> element_views;
      element_views.reserve(elements.size());
      for (const auto& e : elements) {
        element_views.push_back(e.BuildView());
      }
      return value::RuntimeValueView{
          .data = value::UnpackedArrayValueView{
              .elements = std::move(element_views)}};
    }
  }
  return value::RuntimeValueView{
      .data = value::IntegralValueView::Narrow(
          0U, 0U, 32U, value::IntegralStateKind::kTwoState, true)};
}

auto ParseExpectedValue(std::string_view text, bool node_is_integer)
    -> std::expected<ExpectedValue, std::string> {
  if (node_is_integer) {
    ExpectedValue out;
    out.kind = ExpectedValueKind::kIntegerScalar;
    std::int64_t value = 0;
    // yaml-cpp delivers the raw lexeme regardless of base prefix when
    // node_is_integer is true. Honor the C/SV-style `0x` / `0X` prefix (and
    // optional leading minus) so hex expectations like `0x08` work without
    // forcing every yaml author to hand-convert to decimal. Bare unprefixed
    // text parses as base 10 to match yaml's IntegerScalar contract.
    std::string_view body = text;
    bool negative = false;
    if (!body.empty() && body.front() == '-') {
      negative = true;
      body.remove_prefix(1);
    }
    int base = 10;
    if (body.size() >= 2 && body.front() == '0' &&
        (body[1] == 'x' || body[1] == 'X')) {
      base = 16;
      body.remove_prefix(2);
    }
    auto result =
        std::from_chars(body.data(), body.data() + body.size(), value, base);
    if (result.ec != std::errc{} || result.ptr != body.data() + body.size()) {
      return std::unexpected(
          std::format(
              "expect.variables: cannot parse integer '{}'",
              std::string{text}));
    }
    if (negative) value = -value;
    out.integer_value = value;
    out.bit_width = 32;
    out.is_signed = true;
    out.state_kind = value::IntegralStateKind::kTwoState;
    out.format_spec.kind = value::FormatKind::kDecimal;
    out.sv_format_specifier = "%0d";
    return out;
  }

  auto sv_or = TryParseSvLiteral(text);
  if (!sv_or) return std::unexpected(sv_or.error());
  if (sv_or->has_value()) {
    return std::move(**sv_or);
  }

  ExpectedValue out;
  out.kind = ExpectedValueKind::kStringScalar;
  out.string_value = std::string{text};
  out.format_spec.kind = value::FormatKind::kString;
  out.sv_format_specifier = "%s";
  return out;
}

auto RenderExpectedFormatted(const ExpectedValue& value) -> std::string {
  return value::FormatValue(value.format_spec, value.BuildView());
}

}  // namespace lyra::test
