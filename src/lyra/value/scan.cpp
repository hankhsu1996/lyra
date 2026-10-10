#include "lyra/value/scan.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

namespace {

[[nodiscard]] auto IsAsciiWhitespace(int ch) -> bool {
  return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '\f' ||
         ch == '\v';
}

[[nodiscard]] auto IsDecDigit(int ch) -> bool {
  return ch >= '0' && ch <= '9';
}

// The absence of a byte: what a read past the end of the input yields, and
// what an empty pushback slot holds. One spelling for both, because they are
// the same fact read at two moments.
constexpr int kNoByte = -1;

// The byte stream the format walker reads through. The parser looks at most
// one byte ahead of what it has accepted, so a single-byte pushback slot is
// the whole rewind capability it needs. Which bytes separate input fields is
// a property of the scan the caller asked for (LRM 21.3.4.3(a)), so the
// stream answers that question rather than a free helper.
class ScanCursor {
 public:
  ScanCursor(std::string_view buf, detail::NullByte null_byte)
      : buf_(buf), null_byte_(null_byte) {
  }

  auto Peek() -> int {
    if (pushback_ != kNoByte) {
      return pushback_;
    }
    if (cursor_ >= buf_.size()) {
      return kNoByte;
    }
    return static_cast<unsigned char>(buf_[cursor_]);
  }

  auto Consume() -> int {
    if (pushback_ != kNoByte) {
      const int byte = pushback_;
      pushback_ = kNoByte;
      return byte;
    }
    if (cursor_ >= buf_.size()) {
      return kNoByte;
    }
    const int byte = static_cast<unsigned char>(buf_[cursor_]);
    ++cursor_;
    return byte;
  }

  void Unget(int byte) {
    if (pushback_ != kNoByte) {
      throw InternalError(
          "ScanCursor::Unget: pushback slot already holds a byte; scanner "
          "attempted to Unget twice without an intervening Consume");
    }
    if (byte == kNoByte) {
      return;
    }
    pushback_ = byte;
  }

  [[nodiscard]] auto IsWhitespace(int ch) const -> bool {
    if (ch == '\0') {
      return null_byte_ == detail::NullByte::kWhiteSpace;
    }
    return IsAsciiWhitespace(ch);
  }

  // Offset of the next unread byte, ignoring any pending pushback.
  [[nodiscard]] auto Position() const -> std::size_t {
    return cursor_;
  }

 private:
  std::string_view buf_;
  detail::NullByte null_byte_;
  std::size_t cursor_ = 0;
  int pushback_ = kNoByte;
};

void SkipSourceWhitespace(ScanCursor& src) {
  while (true) {
    const int ch = src.Peek();
    if (ch == kNoByte || !src.IsWhitespace(ch)) {
      return;
    }
    src.Consume();
  }
}

[[nodiscard]] auto RequireIntegralTarget(
    const ScanTarget& target, std::string_view spec) -> const IntegralView& {
  const auto* slot = std::get_if<IntegralView>(&target);
  if (slot == nullptr) {
    throw SimulationError(
        std::format(
            "$sscanf/$fscanf: format spec '%{}' expects an integral output "
            "argument, but the corresponding actual is not integral",
            spec));
  }
  return *slot;
}

[[nodiscard]] auto RequireStringTarget(
    const ScanTarget& target, std::string_view spec) -> value::String* {
  auto* const* slot = std::get_if<value::String*>(&target);
  if (slot == nullptr) {
    throw SimulationError(
        std::format(
            "$sscanf/$fscanf: format spec '%{}' expects a string output "
            "argument, but the corresponding actual is not a string",
            spec));
  }
  return *slot;
}

// Per-spec parser convention: each returns `nullopt` on LRM-defined input
// failure. The dispatcher builds the final value from the parsed result
// plus the target's type metadata.

struct DecimalResult {
  enum class Form : std::uint8_t { kInt, kFillX, kFillZ };
  Form form;
  std::int64_t int_value = 0;
};

// LRM 21.3.4.3 Table 21-7 `%d`. Either a sign-prefixed decimal digit run
// (with `_` separators) or a single x/X/z/Z/? that fills the entire dest.
// `max_width == 0` means no limit; non-zero caps the digit / `_` chars
// consumed (C-scanf convention: the sign does not count toward the width).
[[nodiscard]] auto ReadDecimal(ScanCursor& src, std::size_t max_width)
    -> std::optional<DecimalResult> {
  SkipSourceWhitespace(src);
  int ch = src.Peek();
  if (ch == kNoByte) return std::nullopt;

  std::size_t consumed = 0;
  auto can_consume = [&]() { return max_width == 0 || consumed < max_width; };

  if (ch == 'x' || ch == 'X') {
    if (!can_consume()) return std::nullopt;
    src.Consume();
    return DecimalResult{.form = DecimalResult::Form::kFillX};
  }
  if (ch == 'z' || ch == 'Z' || ch == '?') {
    if (!can_consume()) return std::nullopt;
    src.Consume();
    return DecimalResult{.form = DecimalResult::Form::kFillZ};
  }

  bool negative = false;
  bool had_sign = false;
  if (ch == '+' || ch == '-') {
    negative = (ch == '-');
    had_sign = true;
    src.Consume();
    ch = src.Peek();
  }

  if (!IsDecDigit(ch)) {
    if (had_sign) {
      src.Unget(negative ? '-' : '+');
    }
    return std::nullopt;
  }

  std::int64_t acc = 0;
  bool consumed_digit = false;
  while (ch != kNoByte && can_consume() && (IsDecDigit(ch) || ch == '_')) {
    if (ch != '_') {
      acc = (acc * 10) + (ch - '0');
      consumed_digit = true;
    }
    src.Consume();
    ++consumed;
    ch = src.Peek();
  }
  if (!consumed_digit) return std::nullopt;
  if (negative) acc = -acc;
  return DecimalResult{.form = DecimalResult::Form::kInt, .int_value = acc};
}

// LRM 21.3.4.3 Table 21-7 `%b`, `%o`, `%h` / `%x`: the run of characters that
// are digits of the radix, an x, a z or `?` standing for a whole digit, or the
// `_` that separates digits. A run holding no digit at all is no value.
[[nodiscard]] auto ReadDigits(
    ScanCursor& src, std::size_t max_width, DigitRadix radix)
    -> std::optional<std::string> {
  SkipSourceWhitespace(src);
  std::string run;
  bool consumed_digit = false;
  while (max_width == 0 || run.size() < max_width) {
    const int ch = src.Peek();
    if (ch == kNoByte) {
      break;
    }
    const auto c = static_cast<char>(ch);
    const bool is_digit = DigitOf(c, radix).has_value() || IsUnknownDigit(c) ||
                          IsHighImpedanceDigit(c);
    if (!is_digit && c != '_') {
      break;
    }
    consumed_digit = consumed_digit || is_digit;
    run.push_back(c);
    src.Consume();
  }
  if (!consumed_digit) return std::nullopt;
  return run;
}

// Standard scanf `%s`: skip leading whitespace, then read non-whitespace
// chars until whitespace or EOF.
[[nodiscard]] auto ReadString(ScanCursor& src, std::size_t max_width)
    -> std::optional<std::string> {
  SkipSourceWhitespace(src);
  std::string buf;
  while (true) {
    if (max_width != 0 && buf.size() >= max_width) {
      break;
    }
    const int ch = src.Peek();
    if (ch == kNoByte || src.IsWhitespace(ch)) {
      break;
    }
    buf.push_back(static_cast<char>(ch));
    src.Consume();
  }
  if (buf.empty()) return std::nullopt;
  return buf;
}

// Standard scanf `%c`: no whitespace skip, one byte. The scanf `%5c`
// extension (read N bytes into a char array) needs a string-slot output
// shape, which is a separate feature; reject it here so the gap stays
// visible.
[[nodiscard]] auto ReadChar(ScanCursor& src, std::size_t max_width)
    -> std::optional<unsigned char> {
  if (max_width > 1U) {
    throw SimulationError(
        "$sscanf/$fscanf: max field width on '%c' is not yet supported "
        "(needs a string-slot output shape)");
  }
  const int ch = src.Consume();
  if (ch == kNoByte) return std::nullopt;
  return static_cast<unsigned char>(ch & 0xFF);
}

// A parsed value written into the target, at the target's own width and
// state domain.
void WriteDecimal(const DecimalResult& parsed, const IntegralView& dest) {
  switch (parsed.form) {
    case DecimalResult::Form::kInt:
      FromInt(dest.planes, dest.width, parsed.int_value);
      return;
    case DecimalResult::Form::kFillX:
      FillScalar(dest.planes, dest.width, FourStateBit::kUnknown);
      return;
    case DecimalResult::Form::kFillZ:
      FillScalar(dest.planes, dest.width, FourStateBit::kHighImpedance);
      return;
  }
  std::unreachable();
}

// A scanned run of digits written into the target: a shorter run leaves the
// positions above it 0, a longer one loses its leading digits, and a two-state
// target holds an x or z digit as 0.
void WriteDigits(
    std::string_view run, DigitRadix radix, const IntegralView& dest) {
  if (!FromDigits(dest.planes, dest.width, radix, run)) {
    throw InternalError(
        "$sscanf/$fscanf: a scanned run of digits holds a character that is no "
        "digit of its radix -- please report this as a bug");
  }
}

void WriteChar(unsigned char ch, const IntegralView& dest) {
  FromInt(dest.planes, dest.width, static_cast<std::int64_t>(ch));
}

[[nodiscard]] auto ScanFromSource(
    ScanCursor& src, std::string_view fmt, std::span<const ScanTarget> targets)
    -> std::int32_t {
  std::int32_t items = 0;
  std::size_t target_ix = 0;
  bool first_conversion = true;
  std::size_t fmt_ix = 0;

  while (fmt_ix < fmt.size()) {
    const char fc = fmt[fmt_ix];

    if (IsAsciiWhitespace(static_cast<unsigned char>(fc))) {
      SkipSourceWhitespace(src);
      ++fmt_ix;
      continue;
    }

    if (fc != '%') {
      const int ch = src.Peek();
      if (ch == kNoByte) {
        return first_conversion ? -1 : items;
      }
      const int fc_byte = static_cast<unsigned char>(fc);
      if (ch != fc_byte) {
        return items;
      }
      src.Consume();
      ++fmt_ix;
      continue;
    }

    // After '%': optional `*` (assignment suppression), optional decimal
    // max-field-width digits, then the conversion code (LRM 21.3.4.3(c)).
    ++fmt_ix;
    if (fmt_ix >= fmt.size()) {
      throw SimulationError(
          "$sscanf/$fscanf: format string ended after '%' with no conversion "
          "specifier");
    }

    bool suppress = false;
    if (fmt[fmt_ix] == '*') {
      suppress = true;
      ++fmt_ix;
      if (fmt_ix >= fmt.size()) {
        throw SimulationError(
            "$sscanf/$fscanf: format string ended after '%*' with no "
            "conversion specifier");
      }
    }

    std::size_t max_width = 0;
    while (fmt_ix < fmt.size() &&
           IsDecDigit(static_cast<unsigned char>(fmt[fmt_ix]))) {
      max_width =
          (max_width * 10) + static_cast<std::size_t>(fmt[fmt_ix] - '0');
      ++fmt_ix;
    }
    if (fmt_ix >= fmt.size()) {
      throw SimulationError(
          "$sscanf/$fscanf: format string ended in a conversion spec with no "
          "specifier code");
    }
    const char spec = fmt[fmt_ix];
    ++fmt_ix;

    if (spec == '%') {
      if (suppress || max_width != 0) {
        throw SimulationError(
            "$sscanf/$fscanf: '%%' literal does not accept assignment "
            "suppression or field width modifiers");
      }
      const int ch = src.Peek();
      if (ch == kNoByte) {
        return first_conversion ? -1 : items;
      }
      if (ch != '%') {
        return items;
      }
      src.Consume();
      continue;
    }

    if (!suppress && target_ix >= targets.size()) {
      throw SimulationError(
          "$sscanf/$fscanf: format string has more (non-suppressed) "
          "conversion specifiers than output arguments");
    }

    bool ok = false;
    switch (spec) {
      case 'd': {
        if (auto parsed = ReadDecimal(src, max_width); parsed.has_value()) {
          ok = true;
          if (!suppress) {
            WriteDecimal(
                *parsed, RequireIntegralTarget(targets[target_ix], "d"));
          }
        }
        break;
      }
      case 'h':
      case 'x': {
        if (auto parsed = ReadDigits(src, max_width, DigitRadix::kHex);
            parsed.has_value()) {
          ok = true;
          if (!suppress) {
            WriteDigits(
                *parsed, DigitRadix::kHex,
                RequireIntegralTarget(
                    targets[target_ix], spec == 'x' ? "x" : "h"));
          }
        }
        break;
      }
      case 'b': {
        if (auto parsed = ReadDigits(src, max_width, DigitRadix::kBinary);
            parsed.has_value()) {
          ok = true;
          if (!suppress) {
            WriteDigits(
                *parsed, DigitRadix::kBinary,
                RequireIntegralTarget(targets[target_ix], "b"));
          }
        }
        break;
      }
      case 'o': {
        if (auto parsed = ReadDigits(src, max_width, DigitRadix::kOctal);
            parsed.has_value()) {
          ok = true;
          if (!suppress) {
            WriteDigits(
                *parsed, DigitRadix::kOctal,
                RequireIntegralTarget(targets[target_ix], "o"));
          }
        }
        break;
      }
      case 's': {
        if (auto parsed = ReadString(src, max_width); parsed.has_value()) {
          ok = true;
          if (!suppress) {
            auto* dest = RequireStringTarget(targets[target_ix], "s");
            *dest = value::String(std::move(*parsed));
          }
        }
        break;
      }
      case 'c': {
        if (auto parsed = ReadChar(src, max_width); parsed.has_value()) {
          ok = true;
          if (!suppress) {
            WriteChar(*parsed, RequireIntegralTarget(targets[target_ix], "c"));
          }
        }
        break;
      }
      default:
        throw SimulationError(
            std::format(
                "$sscanf/$fscanf: unsupported conversion specifier '%{}'",
                spec));
    }

    if (!ok) {
      if (first_conversion && src.Peek() == kNoByte) {
        return -1;
      }
      return items;
    }
    first_conversion = false;
    if (!suppress) {
      ++items;
      ++target_ix;
    }
  }
  return items;
}

}  // namespace

namespace detail {

auto ScanImpl(
    const value::String& input, const value::String& format, NullByte null_byte,
    std::span<const ScanTarget> targets) -> ScanCount {
  ScanCursor src(input.View(), null_byte);
  const std::int32_t items = ScanFromSource(src, format.View(), targets);
  return ScanCount{
      .items = items, .consumed = static_cast<std::int64_t>(src.Position())};
}

}  // namespace detail

}  // namespace lyra::value
