#pragma once

#include <cstddef>
#include <string_view>

namespace lyra::test {

// How deep one written file nests its parentheses, braces and brackets at the
// deepest, and the line that depth is reached on. What stands inside a string
// literal is text, not nesting.
struct Nesting {
  std::size_t depth = 0;
  std::size_t line = 0;
};

auto DeepestNesting(std::string_view text) -> Nesting;

}  // namespace lyra::test
