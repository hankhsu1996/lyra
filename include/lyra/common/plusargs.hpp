#pragma once

#include <charconv>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>

namespace lyra::common {

// Parse format string: "PREFIX%<spec>" -> (prefix, spec_char)
// Handles %0d -> %d by skipping leading 0.
inline auto ParsePlusargsFormat(std::string_view format)
    -> std::pair<std::string_view, char> {
  auto percent_pos = format.find('%');
  if (percent_pos == std::string_view::npos) {
    return {format, '\0'};
  }

  std::string_view prefix = format.substr(0, percent_pos);
  char spec = '\0';
  size_t spec_pos = percent_pos + 1;
  if (spec_pos < format.size()) {
    spec = format[spec_pos];
    if (spec == '0' && spec_pos + 1 < format.size()) {
      spec = format[spec_pos + 1];
    }
  }
  return {prefix, spec};
}

// Helper to strip '+' prefix from a plusarg
inline auto StripPlusPrefix(std::string_view arg) -> std::string_view {
  if (!arg.empty() && arg.front() == '+') {
    return arg.substr(1);
  }
  return arg;
}

// Check if any plusarg matches the query prefix (for $test$plusargs).
// Returns 1 if found, 0 otherwise.
inline auto TestPlusargs(
    std::span<const std::string> plusargs, std::string_view query) -> int32_t {
  for (const auto& arg : plusargs) {
    std::string_view content = StripPlusPrefix(arg);
    if (content.starts_with(query)) {
      return 1;
    }
  }
  return 0;
}

// Match $value$plusargs with integer output (%d format).
// Returns 1 if match found, 0 otherwise. Writes parsed value to output.
inline auto MatchPlusargsInt(
    std::span<const std::string> plusargs, std::string_view format,
    int32_t* output) -> int32_t {
  auto [prefix, spec] = ParsePlusargsFormat(format);

  if (spec != 'd' && spec != 'D') {
    return 0;
  }

  for (const auto& arg : plusargs) {
    std::string_view content = StripPlusPrefix(arg);
    if (!content.starts_with(prefix)) {
      continue;
    }
    std::string_view remainder = content.substr(prefix.size());

    int32_t parsed_value = 0;
    auto [ptr, ec] = std::from_chars(
        remainder.data(), remainder.data() + remainder.size(), parsed_value);
    if (ec != std::errc{}) {
      parsed_value = 0;
    }
    if (output != nullptr) {
      *output = parsed_value;
    }
    return 1;
  }
  return 0;
}

// Match $value$plusargs with string output (%s format).
// Returns 1 if match found, 0 otherwise. Writes result to output.
inline auto MatchPlusargsString(
    std::span<const std::string> plusargs, std::string_view format,
    std::string* output) -> int32_t {
  auto [prefix, spec] = ParsePlusargsFormat(format);

  if (spec != 's' && spec != 'S') {
    return 0;
  }

  for (const auto& arg : plusargs) {
    std::string_view content = StripPlusPrefix(arg);
    if (!content.starts_with(prefix)) {
      continue;
    }
    std::string_view remainder = content.substr(prefix.size());

    if (output != nullptr) {
      *output = std::string(remainder);
    }
    return 1;
  }
  return 0;
}

}  // namespace lyra::common
