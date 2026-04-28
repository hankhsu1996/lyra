#include "string_literal.hpp"

#include <format>
#include <string>
#include <string_view>

namespace lyra::backend::cpp {

auto RenderCStringLiteral(std::string_view s) -> std::string {
  std::string out;
  out.push_back('"');
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
          out += std::format("\\x{:02x}", static_cast<unsigned char>(c));
        } else {
          out.push_back(c);
        }
        break;
    }
  }
  out.push_back('"');
  return out;
}

auto RenderStdStringLiteral(std::string_view s) -> std::string {
  return "std::string{" + RenderCStringLiteral(s) + "}";
}

}  // namespace lyra::backend::cpp
