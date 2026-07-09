#include "lyra/runtime/plusargs.hpp"

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/runtime/runtime_services.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

// LRM 21.6 user_string carries "plusarg_prefix format_spec". The prefix is
// everything up to the first `%`; the letter after `%` (with an optional
// leading `0`) is the conversion specifier.
struct ParsedUserString {
  std::string_view prefix;
  char format_letter;
};

auto ParseUserString(std::string_view user) -> ParsedUserString {
  const auto pct = user.find('%');
  if (pct == std::string_view::npos) {
    return {.prefix = user, .format_letter = '\0'};
  }
  std::size_t letter_pos = pct + 1;
  if (letter_pos < user.size() && user[letter_pos] == '0') {
    ++letter_pos;
  }
  const char letter = letter_pos < user.size() ? user[letter_pos] : '\0';
  return {.prefix = user.substr(0, pct), .format_letter = letter};
}

auto BaseForFormat(char letter) -> std::optional<int> {
  switch (letter) {
    case 'd':
    case 'D':
      return 10;
    case 'o':
    case 'O':
      return 8;
    case 'h':
    case 'H':
    case 'x':
    case 'X':
      return 16;
    case 'b':
    case 'B':
      return 2;
    default:
      return std::nullopt;
  }
}

auto ConvertIntegralRemainder(std::string_view remainder, int base)
    -> std::int64_t {
  // LRM 21.6: an empty remainder stores zero rather than being a parse error.
  // Illegal characters mid-parse stop the scan and the accumulated value
  // stands; LRM 21.6's `'bx` outcome for "illegal characters for the specified
  // conversion" needs the target's shape and is not yet modeled here.
  std::int64_t value = 0;
  for (const char c : remainder) {
    int digit = 0;
    if (c >= '0' && c <= '9') {
      digit = c - '0';
    } else if (c >= 'a' && c <= 'z') {
      digit = (c - 'a') + 10;
    } else if (c >= 'A' && c <= 'Z') {
      digit = (c - 'A') + 10;
    } else {
      return value;
    }
    if (digit >= base) return value;
    value = (value * base) + digit;
  }
  return value;
}

auto MakeIntResult(std::int64_t value) -> value::PackedArray {
  // SV `int` shape: 32-bit signed, 2-state (LRM 6.11.1).
  return value::PackedArray::FromInt(value, 32, true, false);
}

}  // namespace

auto PlusArgsSource::MatchPrefix(std::string_view prefix) const
    -> std::optional<std::string_view> {
  for (const std::string& token : tokens_) {
    const std::string_view content{token};
    if (content.starts_with(prefix)) {
      return content.substr(prefix.size());
    }
  }
  return std::nullopt;
}

auto TestPlusargs(RuntimeServices& services, const value::String& user_string)
    -> value::PackedArray {
  const auto match = services.PlusArgs().MatchPrefix(user_string.View());
  return MakeIntResult(match.has_value() ? 1 : 0);
}

auto ValuePlusargs(
    RuntimeServices& services, const value::String& user_string,
    value::PackedArray& out) -> value::PackedArray {
  const auto parsed = ParseUserString(user_string.View());
  const auto base = BaseForFormat(parsed.format_letter);
  if (!base.has_value()) {
    // %s / %e / %f / %g on an integral target: no match, out untouched.
    return MakeIntResult(0);
  }
  const auto match = services.PlusArgs().MatchPrefix(parsed.prefix);
  if (!match.has_value()) return MakeIntResult(0);
  const std::int64_t converted = ConvertIntegralRemainder(*match, *base);
  out = value::PackedArray::FromInt(converted, out);
  return MakeIntResult(1);
}

auto ValuePlusargs(
    RuntimeServices& services, const value::String& user_string,
    value::String& out) -> value::PackedArray {
  const auto parsed = ParseUserString(user_string.View());
  const char letter = parsed.format_letter;
  if (letter != 's' && letter != 'S') return MakeIntResult(0);
  const auto match = services.PlusArgs().MatchPrefix(parsed.prefix);
  if (!match.has_value()) return MakeIntResult(0);
  out = value::String(std::string(*match));
  return MakeIntResult(1);
}

}  // namespace lyra::runtime
