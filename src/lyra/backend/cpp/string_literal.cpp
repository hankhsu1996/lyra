#include "lyra/backend/cpp/string_literal.hpp"

#include <cstddef>
#include <string_view>

#include "lyra/backend/cpp/target_text.hpp"

namespace lyra::backend::cpp {

namespace {

constexpr std::string_view kHexDigits = "0123456789abcdef";

// A control character is written as the two-digit escape C++ reads back, so the
// width is fixed and both digits come from the table.
void WriteControlEscape(unsigned char c, TargetText& out) {
  out += "\\x";
  out += kHexDigits.substr(static_cast<std::size_t>(c >> 4U), 1);
  out += kHexDigits.substr(static_cast<std::size_t>(c & 0x0FU), 1);
}

}  // namespace

void WriteCStringLiteral(std::string_view s, TargetText& out) {
  out += "\"";
  for (char c : s) {
    switch (c) {
      case '"':
        out += "\\\"";
        break;
      case '\\':
        out += "\\\\";
        break;
      case '\n':
        out += "\\n";
        break;
      case '\t':
        out += "\\t";
        break;
      case '\r':
        out += "\\r";
        break;
      default:
        if (static_cast<unsigned char>(c) < 0x20) {
          WriteControlEscape(static_cast<unsigned char>(c), out);
        } else {
          out += std::string_view{&c, 1};
        }
        break;
    }
  }
  out += "\"";
}

}  // namespace lyra::backend::cpp
