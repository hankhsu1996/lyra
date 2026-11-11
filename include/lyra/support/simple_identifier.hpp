#pragma once

#include <algorithm>
#include <string_view>

namespace lyra::support {

// Whether `c` may open a simple identifier (LRM 5.6): a letter or an
// underscore.
[[nodiscard]] constexpr auto IsSimpleIdentifierStart(char c) -> bool {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

// Whether `c` may stand in a simple identifier (LRM 5.6): a letter, a digit, a
// dollar sign or an underscore.
[[nodiscard]] constexpr auto IsSimpleIdentifierContinue(char c) -> bool {
  return IsSimpleIdentifierStart(c) || (c >= '0' && c <= '9') || c == '$';
}

// Whether `text` is a simple identifier (LRM 5.6). A keyword passes, since
// which words are keywords depends on the language version.
[[nodiscard]] constexpr auto IsSimpleIdentifier(std::string_view text) -> bool {
  return !text.empty() && IsSimpleIdentifierStart(text.front()) &&
         std::ranges::all_of(text, IsSimpleIdentifierContinue);
}

}  // namespace lyra::support
