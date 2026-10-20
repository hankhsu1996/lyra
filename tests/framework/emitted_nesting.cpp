#include "tests/framework/emitted_nesting.hpp"

#include <cstddef>
#include <string_view>

namespace lyra::test {

auto DeepestNesting(std::string_view text) -> Nesting {
  Nesting deepest;
  std::size_t depth = 0;
  std::size_t line = 1;
  bool in_string = false;
  bool escaped = false;
  for (const char c : text) {
    if (c == '\n') {
      ++line;
    }
    if (in_string) {
      if (escaped) {
        escaped = false;
      } else if (c == '\\') {
        escaped = true;
      } else if (c == '"') {
        in_string = false;
      }
      continue;
    }
    switch (c) {
      case '"':
        in_string = true;
        break;
      case '(':
      case '[':
      case '{':
        ++depth;
        if (depth > deepest.depth) {
          deepest = {.depth = depth, .line = line};
        }
        break;
      case ')':
      case ']':
      case '}':
        --depth;
        break;
      default:
        break;
    }
  }
  return deepest;
}

}  // namespace lyra::test
