#include "matcher.hpp"

#include <cctype>
#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>

namespace lyra::test {

auto StripAnsi(std::string_view s) -> std::string {
  std::string out;
  out.reserve(s.size());
  std::size_t i = 0;
  while (i < s.size()) {
    if (s[i] == '\x1b' && i + 1 < s.size() && s[i + 1] == '[') {
      std::size_t j = i + 2;
      while (j < s.size()) {
        char c = s[j];
        if ((c >= '0' && c <= '9') || c == ';') {
          ++j;
          continue;
        }
        break;
      }
      if (j < s.size()) {
        ++j;
      }
      i = j;
      continue;
    }
    out.push_back(s[i]);
    ++i;
  }
  return out;
}

auto CheckOutput(
    const std::string& actual, const ExpectedOutput& expected,
    std::string_view context) -> std::optional<std::string> {
  const std::string cleaned = StripAnsi(actual);
  if (expected.exact.has_value()) {
    if (cleaned != *expected.exact) {
      return std::format(
          "{}: exact mismatch\n--- expected ---\n{}\n--- actual ---\n{}",
          context, *expected.exact, cleaned);
    }
  }
  for (const auto& needle : expected.contains) {
    if (cleaned.find(needle) == std::string::npos) {
      return std::format(
          "{}: expected substring not found: \"{}\"", context, needle);
    }
  }
  for (const auto& needle : expected.not_contains) {
    if (cleaned.find(needle) != std::string::npos) {
      return std::format(
          "{}: forbidden substring present: \"{}\"", context, needle);
    }
  }
  return std::nullopt;
}

}  // namespace lyra::test
