#pragma once

#include <algorithm>
#include <cstddef>
#include <string>
#include <string_view>

namespace lyra::backend::cpp {

[[nodiscard]] inline auto Indent(std::size_t level) -> std::string {
  std::string result(level * 2, ' ');
  return result;
}

// A unit or class name spelled as a C++ identifier. A source name is already a
// valid identifier, but a synthesized name may carry a `$` (the design-root
// unit) that C++ does not admit; mapping it to `_` yields the token the emitted
// class, its constructor, and every reference to its type share.
[[nodiscard]] inline auto ToCppName(std::string_view name) -> std::string {
  std::string out{name};
  std::ranges::replace(out, '$', '_');
  return out;
}

}  // namespace lyra::backend::cpp
