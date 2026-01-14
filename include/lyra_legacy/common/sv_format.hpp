#pragma once

#include <cctype>
#include <format>
#include <stdexcept>
#include <string>
#include <vector>

namespace lyra::common {

// Supported format specifiers for $display
// %d - decimal, %h/%x - hex, %b - binary, %o - octal, %s - string, %f - real
// %t - time (formats time value according to $timeformat settings)
// %% - literal %
// Throws on unsupported specifiers (e.g., %m)

struct FormatSpec {
  char spec;        // 'd', 'h', 'x', 'b', 'o', 's', 'f', 't'
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

        // Optional left-align flag '-'
        if (i < fmt_str.size() && fmt_str[i] == '-') {
          ++i;
        }

        // Optional zero-pad flag and width.
        if (i < fmt_str.size() && fmt_str[i] == '0') {
          ++i;
        }
        while (i < fmt_str.size() && (std::isdigit(fmt_str[i]) != 0)) {
          ++i;
        }

        // Optional precision for real: .<digits>
        if (i < fmt_str.size() && fmt_str[i] == '.') {
          ++i;
          if (i >= fmt_str.size() || (std::isdigit(fmt_str[i]) == 0)) {
            throw std::runtime_error(
                "Invalid format string: missing precision digits");
          }
          while (i < fmt_str.size() && (std::isdigit(fmt_str[i]) != 0)) {
            ++i;
          }
        }

        if (i >= fmt_str.size()) {
          throw std::runtime_error("Invalid format string: trailing %");
        }

        char c = fmt_str[i];
        if (c == 'd' || c == 'h' || c == 'x' || c == 'b' || c == 'o' ||
            c == 's' || c == 'f' || c == 't') {
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
        bool left_align = false;
        bool zero_pad = false;
        std::string width;
        std::string precision;

        // Check for left-align flag '-'
        if (i < sv_fmt.size() && sv_fmt[i] == '-') {
          left_align = true;
          ++i;
        }

        if (i < sv_fmt.size() && sv_fmt[i] == '0') {
          zero_pad = true;
          ++i;
        }

        while (i < sv_fmt.size() && (std::isdigit(sv_fmt[i]) != 0)) {
          width += sv_fmt[i];
          ++i;
        }

        if (i < sv_fmt.size() && sv_fmt[i] == '.') {
          ++i;
          if (i >= sv_fmt.size() || (std::isdigit(sv_fmt[i]) == 0)) {
            throw std::runtime_error(
                "Invalid format string: missing precision digits");
          }
          while (i < sv_fmt.size() && (std::isdigit(sv_fmt[i]) != 0)) {
            precision += sv_fmt[i];
            ++i;
          }
        }

        if (i >= sv_fmt.size()) {
          throw std::runtime_error("Invalid format string: trailing %");
        }

        char c = sv_fmt[i];
        if (c == 'd') {
          if (!precision.empty()) {
            throw std::runtime_error(
                "Unsupported format specifier: precision not supported for %d");
          }
          std::string spec = "{:";
          // Only add zero_pad when there's a width (otherwise %0d = %d)
          if (zero_pad && !width.empty()) {
            spec += "0";
          }
          if (!width.empty()) {
            spec += width;
          }
          // Use {:d} to ensure bools print as 1/0 instead of true/false
          spec += "d}";
          result += spec;
          ++i;
        } else if (c == 's') {
          if (!precision.empty()) {
            throw std::runtime_error(
                "Unsupported format specifier: precision not supported for %s");
          }
          // Build format spec with optional width and alignment
          // zero_pad is ignored for strings (Verilator behavior)
          // SV default is right-align, but std::format default is left-align
          if (!width.empty()) {
            std::string spec = "{:";
            if (!left_align) {
              spec += ">";  // Right-align (SV default) in std::format
            }
            spec += width;
            spec += "}";
            result += spec;
          } else {
            result += "{}";
          }
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
          if (!precision.empty()) {
            throw std::runtime_error(
                "Unsupported format specifier: precision not supported for "
                "%h/%x");
          }
          std::string spec = "{:";
          if (zero_pad && !width.empty()) {
            spec += "0";
          }
          if (!width.empty()) {
            spec += width;
          }
          spec += "x}";
          result += spec;
          ++i;
        } else if (c == 'b') {
          if (!precision.empty()) {
            throw std::runtime_error(
                "Unsupported format specifier: precision not supported for %b");
          }
          std::string spec = "{:";
          if (zero_pad && !width.empty()) {
            spec += "0";
          }
          if (!width.empty()) {
            spec += width;
          }
          spec += "b}";
          result += spec;
          ++i;
        } else if (c == 'o') {
          if (!precision.empty()) {
            throw std::runtime_error(
                "Unsupported format specifier: precision not supported for %o");
          }
          std::string spec = "{:";
          if (zero_pad && !width.empty()) {
            spec += "0";
          }
          if (!width.empty()) {
            spec += width;
          }
          spec += "o}";
          result += spec;
          ++i;
        } else if (c == 't') {
          // %t is special: it doesn't consume an argument from the caller.
          // The formatted time value will be inserted as an extra argument.
          // Just use {} as placeholder - caller is responsible for providing
          // the formatted time string at the right position.
          result += "{}";
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

// Returns a vector of bools indicating whether each format argument needs
// to be cast to int64_t for std::format compatibility. This is needed because
// custom types like Bit<N> don't support width specifiers in std::format.
// Integer format specs (d, h, x, b, o) with width or zero_pad need casting.
inline auto NeedsIntCast(const std::string& sv_fmt) -> std::vector<bool> {
  std::vector<bool> needs_cast;
  size_t i = 0;

  while (i < sv_fmt.size()) {
    if (sv_fmt[i] == '%') {
      if (i + 1 >= sv_fmt.size()) {
        break;
      }
      if (sv_fmt[i + 1] == '%') {
        i += 2;
      } else {
        ++i;  // Consume '%'
        bool zero_pad = false;
        bool has_width = false;

        // Skip left-align flag '-'
        if (i < sv_fmt.size() && sv_fmt[i] == '-') {
          ++i;
        }

        if (i < sv_fmt.size() && sv_fmt[i] == '0') {
          zero_pad = true;
          ++i;
        }

        while (i < sv_fmt.size() && (std::isdigit(sv_fmt[i]) != 0)) {
          has_width = true;
          ++i;
        }

        // Skip precision
        if (i < sv_fmt.size() && sv_fmt[i] == '.') {
          ++i;
          while (i < sv_fmt.size() && (std::isdigit(sv_fmt[i]) != 0)) {
            ++i;
          }
        }

        if (i >= sv_fmt.size()) {
          break;
        }

        char c = sv_fmt[i];
        // %t is formatted as a string, no int cast needed
        // Integer types with width/zero_pad need casting
        bool is_int_type =
            (c == 'd' || c == 'h' || c == 'x' || c == 'b' || c == 'o');
        needs_cast.push_back(is_int_type && (zero_pad || has_width));
        ++i;
      }
    } else {
      ++i;
    }
  }
  return needs_cast;
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
