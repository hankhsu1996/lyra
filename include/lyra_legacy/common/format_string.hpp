#pragma once

#include <string>

namespace lyra::common {

// Result of extracting format string from first argument of $display-like
// tasks.
struct FormatStringInfo {
  std::string text;                // The extracted string (format or prefix)
  bool is_string_literal = false;  // True if first arg was a string literal
  bool has_format_specifiers = false;  // True if text contains '%'
};

}  // namespace lyra::common
