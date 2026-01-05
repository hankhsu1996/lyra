#pragma once

#include <cctype>
#include <format>
#include <stdexcept>
#include <string>
#include <vector>

namespace lyra::common {

// Supported format specifiers for $display
// %d - decimal, %h/%x - hex, %b - binary, %o - octal, %s - string, %f - real
// %% - literal %
// Throws on unsupported specifiers (e.g., %t, %m, %0d)

struct FormatSpec {
  char spec;        // 'd', 'h', 'x', 'b', 'o', 's', 'f'
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
      if (fmt_str[i + 1] == '%') {
        // %% is an escape, not a format specifier
        i += 2;
      } else {
        size_t spec_pos = i;
        ++i;  // Consume '%'

        // Optional zero-pad flag and width.
        if (i < fmt_str.size() && fmt_str[i] == '0') {
          ++i;
        }
        while (i < fmt_str.size() && std::isdigit(fmt_str[i])) {
          ++i;
        }

        // Optional precision for real: .<digits>
        if (i < fmt_str.size() && fmt_str[i] == '.') {
          ++i;
          if (i >= fmt_str.size() || !std::isdigit(fmt_str[i])) {
            throw std::runtime_error(
                "Invalid format string: missing precision digits");
          }
          while (i < fmt_str.size() && std::isdigit(fmt_str[i])) {
            ++i;
          }
        }

        if (i >= fmt_str.size()) {
          throw std::runtime_error("Invalid format string: trailing %");
        }

        char c = fmt_str[i];
        if (c == 'd' || c == 'h' || c == 'x' || c == 'b' || c == 'o' ||
            c == 's' || c == 'f') {
          specs.push_back({c, spec_pos});
          ++i;
        } else {
          throw std::runtime_error(
              std::format("Unsupported format specifier: %{}", c));
        }
      }
    } else {
      ++i;
    }
  }
  return specs;
}

// Transform a SV format string to std::format syntax
// %d -> {:d}, %h/%x -> {:x}, %b -> {:b}, %o -> {:o}, %s -> {}, %f -> {:f}
inline auto TransformToStdFormat(const std::string& sv_fmt) -> std::string {
  std::string result;
  size_t i = 0;

  while (i < sv_fmt.size()) {
    if (sv_fmt[i] == '%') {
      if (i + 1 >= sv_fmt.size()) {
        throw std::runtime_error("Invalid format string: trailing %");
      }
      if (sv_fmt[i + 1] == '%') {
        result += '%';
        i += 2;
      } else {
        ++i;  // Consume '%'
        bool zero_pad = false;
        std::string width;
        std::string precision;

        if (i < sv_fmt.size() && sv_fmt[i] == '0') {
          zero_pad = true;
          ++i;
        }

        while (i < sv_fmt.size() && std::isdigit(sv_fmt[i])) {
          width += sv_fmt[i];
          ++i;
        }

        if (i < sv_fmt.size() && sv_fmt[i] == '.') {
          ++i;
          if (i >= sv_fmt.size() || !std::isdigit(sv_fmt[i])) {
            throw std::runtime_error(
                "Invalid format string: missing precision digits");
          }
          while (i < sv_fmt.size() && std::isdigit(sv_fmt[i])) {
            precision += sv_fmt[i];
            ++i;
          }
        }

        if (i >= sv_fmt.size()) {
          throw std::runtime_error("Invalid format string: trailing %");
        }

        char c = sv_fmt[i];
        if (c == 'd') {
          if (zero_pad || !width.empty() || !precision.empty()) {
            throw std::runtime_error(
                "Unsupported format specifier: width/precision only "
                "supported for %f");
          }
          // Use {:d} to ensure bools print as 1/0 instead of true/false
          result += "{:d}";
          ++i;
        } else if (c == 's') {
          if (zero_pad || !width.empty() || !precision.empty()) {
            throw std::runtime_error(
                "Unsupported format specifier: width/precision only "
                "supported for %f");
          }
          result += "{}";
          ++i;
        } else if (c == 'f') {
          std::string spec = "{:";
          if (zero_pad && !width.empty()) {
            spec += "0>";
          }
          if (!width.empty()) {
            spec += width;
          }
          if (!precision.empty()) {
            spec += ".";
            spec += precision;
          }
          spec += "f}";
          result += spec;
          ++i;
        } else if (c == 'h' || c == 'x') {
          if (zero_pad || !width.empty() || !precision.empty()) {
            throw std::runtime_error(
                "Unsupported format specifier: width/precision only "
                "supported for %f");
          }
          result += "{:x}";
          ++i;
        } else if (c == 'b') {
          if (zero_pad || !width.empty() || !precision.empty()) {
            throw std::runtime_error(
                "Unsupported format specifier: width/precision only "
                "supported for %f");
          }
          result += "{:b}";
          ++i;
        } else if (c == 'o') {
          if (zero_pad || !width.empty() || !precision.empty()) {
            throw std::runtime_error(
                "Unsupported format specifier: width/precision only "
                "supported for %f");
          }
          result += "{:o}";
          ++i;
        } else {
          throw std::runtime_error(
              std::format("Unsupported format specifier: %{}", c));
        }
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
