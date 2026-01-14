#pragma once

#include <string>

namespace lyra::common {

// Escape a string for use in C++ string literal.
inline auto EscapeForCppString(const std::string& s) -> std::string {
  std::string result;
  result.reserve(s.size() + (s.size() / 10));  // Estimate some escapes
  for (char c : s) {
    switch (c) {
      case '\\':
        result += "\\\\";
        break;
      case '"':
        result += "\\\"";
        break;
      case '\n':
        result += "\\n";
        break;
      case '\r':
        result += "\\r";
        break;
      case '\t':
        result += "\\t";
        break;
      default:
        result += c;
        break;
    }
  }
  return result;
}

}  // namespace lyra::common
