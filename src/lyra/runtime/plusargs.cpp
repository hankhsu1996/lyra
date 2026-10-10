#include "lyra/runtime/plusargs.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
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

}  // namespace

auto PlusargsFrom(std::span<const std::string> arguments)
    -> std::vector<std::string> {
  std::vector<std::string> plusargs;
  for (const std::string& argument : arguments) {
    if (argument.starts_with("+")) {
      plusargs.emplace_back(argument.substr(1));
    }
  }
  return plusargs;
}

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

auto TestPlusargs(RuntimeEffects& runtime, const value::String& user_string)
    -> value::Int {
  const auto match = runtime.PlusArgs().MatchPrefix(user_string.View());
  return value::Int::FromBool(match.has_value());
}

auto ValuePlusargsInto(
    RuntimeEffects& runtime, const value::String& user_string,
    const value::IntegralView& out) -> value::Int {
  const auto parsed = ParseUserString(user_string.View());
  const auto base = BaseForFormat(parsed.format_letter);
  // %s / %e / %f / %g on an integral target converts nothing.
  if (!base.has_value()) return value::Int::FromInt(0);
  const auto match = runtime.PlusArgs().MatchPrefix(parsed.prefix);
  if (!match.has_value()) return value::Int::FromInt(0);
  value::FromInt(
      out.planes, out.width, ConvertIntegralRemainder(*match, *base));
  return value::Int::FromInt(1);
}

auto ValuePlusargs(
    RuntimeEffects& runtime, const value::String& user_string,
    value::String out) -> value::Tuple<value::Int, value::String> {
  using Completion = value::Tuple<value::Int, value::String>;
  const auto missed = [&out] {
    return Completion{value::Int::FromInt(0), out};
  };
  const auto parsed = ParseUserString(user_string.View());
  const char letter = parsed.format_letter;
  if (letter != 's' && letter != 'S') return missed();
  const auto match = runtime.PlusArgs().MatchPrefix(parsed.prefix);
  if (!match.has_value()) return missed();
  return Completion{value::Int::FromInt(1), value::String(std::string(*match))};
}

}  // namespace lyra::runtime
