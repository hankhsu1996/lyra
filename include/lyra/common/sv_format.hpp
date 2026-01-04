#pragma once

#include <format>
#include <stdexcept>
#include <string>
#include <vector>

namespace lyra::common {

// Supported format specifiers for $display
// %d - decimal, %h/%x - hex, %b - binary, %o - octal, %s - string
// %% - literal %
// Throws on unsupported specifiers (e.g., %t, %m, %0d)

struct FormatSpec {
  char spec;        // 'd', 'h', 'x', 'b', 'o', 's'
  size_t position;  // Position in the original string where % was found
};

// Parse a SV format string and extract format specifiers
// Returns the specifiers in order of appearance
inline auto ParseDisplayFormat(const std::string& fmt_str)
    -> std::vector<FormatSpec> {
  std::vector<FormatSpec> specs;
  size_t i = 0;

  while (i < fmt_str.size()) {
    if (fmt_str[i] == '%') {
      if (i + 1 >= fmt_str.size()) {
        throw std::runtime_error("Invalid format string: trailing %");
      }
      char c = fmt_str[i + 1];
      if (c == '%') {
        // %% is an escape, not a format specifier
        i += 2;
      } else if (
          c == 'd' || c == 'h' || c == 'x' || c == 'b' || c == 'o' ||
          c == 's') {
        specs.push_back({c, i});
        i += 2;
      } else if (c >= '0' && c <= '9') {
        throw std::runtime_error(
            std::format(
                "Unsupported format specifier: width modifiers not yet "
                "supported "
                "(found %{}...)",
                c));
      } else {
        throw std::runtime_error(
            std::format("Unsupported format specifier: %{}", c));
      }
    } else {
      ++i;
    }
  }
  return specs;
}

// Transform a SV format string to std::format syntax
// %d -> {}, %h/%x -> {:x}, %b -> {:b}, %o -> {:o}, %s -> {}
inline auto TransformToStdFormat(const std::string& sv_fmt) -> std::string {
  std::string result;
  size_t i = 0;

  while (i < sv_fmt.size()) {
    if (sv_fmt[i] == '%') {
      if (i + 1 >= sv_fmt.size()) {
        throw std::runtime_error("Invalid format string: trailing %");
      }
      char c = sv_fmt[i + 1];
      if (c == '%') {
        result += '%';
        i += 2;
      } else if (c == 'd' || c == 's') {
        result += "{}";
        i += 2;
      } else if (c == 'h' || c == 'x') {
        result += "{:x}";
        i += 2;
      } else if (c == 'b') {
        result += "{:b}";
        i += 2;
      } else if (c == 'o') {
        result += "{:o}";
        i += 2;
      } else if (c >= '0' && c <= '9') {
        throw std::runtime_error(
            std::format(
                "Unsupported format specifier: width modifiers not yet "
                "supported "
                "(found %{}...)",
                c));
      } else {
        throw std::runtime_error(
            std::format("Unsupported format specifier: %{}", c));
      }
    } else {
      result += sv_fmt[i];
      ++i;
    }
  }
  return result;
}

// Check if a string contains format specifiers (has % followed by valid spec)
inline auto HasFormatSpecifiers(const std::string& str) -> bool {
  size_t i = 0;
  while (i < str.size()) {
    if (str[i] == '%' && i + 1 < str.size()) {
      char c = str[i + 1];
      if (c != '%') {  // %% is escape, not a specifier
        return true;
      }
      i += 2;
    } else {
      ++i;
    }
  }
  return false;
}

}  // namespace lyra::common
