#pragma once

#include <algorithm>
#include <string_view>

namespace lyra::support {

// Whether `text` is a simple identifier (LRM 5.6): letters, digits, dollar
// signs and underscores, opening with a letter or an underscore. A keyword
// passes, since which words are keywords depends on the language version.
[[nodiscard]] constexpr auto IsSimpleIdentifier(std::string_view text) -> bool {
  const auto is_letter = [](char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
  };
  const auto is_digit = [](char c) { return c >= '0' && c <= '9'; };
  return !text.empty() && is_letter(text.front()) &&
         std::ranges::all_of(text, [&](char c) {
           return is_letter(c) || is_digit(c) || c == '$';
         });
}

}  // namespace lyra::support
