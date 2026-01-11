#pragma once

#include <charconv>
#include <cstdint>
#include <functional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace lyra::sdk {

/// Result of a $value$plusargs query.
/// Contains the parsed value if match succeeded.
template <typename T>
struct ValuePlusargResult {
  bool matched = false;
  T value{};

  explicit operator bool() const {
    return matched;
  }
};

/// Immutable collection of plusargs, populated at simulation start.
/// Stores raw command-line arguments starting with '+'.
class PlusargsTable {
 public:
  PlusargsTable() = default;
  explicit PlusargsTable(std::vector<std::string> args)
      : args_(std::move(args)) {
  }

  [[nodiscard]] auto Args() const -> std::span<const std::string> {
    return args_;
  }

  [[nodiscard]] auto Empty() const -> bool {
    return args_.empty();
  }

 private:
  std::vector<std::string> args_;
};

/// Query interface for plusargs.
/// Implements LRM 21.6 prefix matching semantics.
class PlusargsQuery {
 public:
  explicit PlusargsQuery(const PlusargsTable& table) : table_(table) {
  }

  /// $test$plusargs: returns 1 if any plusarg has query as prefix, 0 otherwise.
  /// Per LRM: the query matches if the plusarg STARTS WITH the query string.
  [[nodiscard]] auto TestPlusargs(std::string_view query) const -> int32_t {
    for (const auto& arg : table_.get().Args()) {
      std::string_view content = GetContent(arg);
      if (content.starts_with(query)) {
        return 1;
      }
    }
    return 0;
  }

  /// $value$plusargs with %d format: returns matched flag and parsed int value.
  /// Format: "PREFIX%d" - finds plusarg starting with PREFIX, parses remainder
  /// as decimal.
  [[nodiscard]] auto ValuePlusargsInt(std::string_view format_query) const
      -> ValuePlusargResult<int32_t> {
    auto [prefix, format] = ParseFormat(format_query);
    if (format != 'd' && format != 'D') {
      return {.matched = false, .value = 0};
    }

    for (const auto& arg : table_.get().Args()) {
      std::string_view content = GetContent(arg);
      if (content.starts_with(prefix)) {
        std::string_view remainder = content.substr(prefix.size());
        int32_t value = 0;
        auto [ptr, ec] = std::from_chars(
            remainder.data(), remainder.data() + remainder.size(), value);
        if (ec == std::errc{}) {
          return {.matched = true, .value = value};
        }
        // Conversion failed - per LRM, should return 'bx, but we return 0 for
        // simplicity
        return {.matched = true, .value = 0};
      }
    }
    return {.matched = false, .value = 0};
  }

  /// $value$plusargs with %s format: returns matched flag and string value.
  [[nodiscard]] auto ValuePlusargsString(std::string_view format_query) const
      -> ValuePlusargResult<std::string> {
    auto [prefix, format] = ParseFormat(format_query);
    if (format != 's' && format != 'S') {
      return {.matched = false, .value = ""};
    }

    for (const auto& arg : table_.get().Args()) {
      std::string_view content = GetContent(arg);
      if (content.starts_with(prefix)) {
        std::string_view remainder = content.substr(prefix.size());
        return {.matched = true, .value = std::string(remainder)};
      }
    }
    return {.matched = false, .value = ""};
  }

  /// Codegen-friendly overload: writes to output variable and returns match
  /// status. Used by generated C++ code for $value$plusargs. Template parameter
  /// allows both native int32_t and Bit<32, true> types.
  template <typename T>
  auto ValuePlusargsInt(std::string_view format_query, T& out) const
      -> int32_t {
    auto result = ValuePlusargsInt(format_query);
    if (result.matched) {
      out = static_cast<T>(result.value);
    }
    return result.matched ? 1 : 0;
  }

  /// Codegen-friendly overload for string values.
  template <typename T>
  auto ValuePlusargsString(std::string_view format_query, T& out) const
      -> int32_t {
    auto result = ValuePlusargsString(format_query);
    if (result.matched) {
      out = result.value;
    }
    return result.matched ? 1 : 0;
  }

 private:
  std::reference_wrapper<const PlusargsTable> table_;

  /// Extract content after '+' prefix.
  static auto GetContent(std::string_view arg) -> std::string_view {
    if (arg.starts_with('+')) {
      return arg.substr(1);
    }
    return arg;
  }

  /// Parse format string into prefix and format specifier.
  /// Format: "PREFIX%<spec>" -> returns {PREFIX, spec}
  static auto ParseFormat(std::string_view format_query)
      -> std::pair<std::string_view, char> {
    auto pos = format_query.find('%');
    if (pos == std::string_view::npos || pos + 1 >= format_query.size()) {
      return {format_query, '\0'};
    }

    std::string_view prefix = format_query.substr(0, pos);
    char format = format_query[pos + 1];

    // Skip leading '0' in format specifier (e.g., %0d -> %d)
    if (format == '0' && pos + 2 < format_query.size()) {
      format = format_query[pos + 2];
    }

    return {prefix, format};
  }
};

/// Parse plusargs from command-line arguments.
/// Extracts all arguments starting with '+'.
inline auto ParsePlusargsFromArgv(std::span<char*> argv) -> PlusargsTable {
  std::vector<std::string> args;
  // Skip first element (program name)
  for (size_t i = 1; i < argv.size(); ++i) {
    std::string_view arg(argv[i]);
    if (arg.starts_with('+')) {
      args.emplace_back(arg);
    }
  }
  return PlusargsTable(std::move(args));
}

// Thread-local plusargs table for simulation runtime.
// Initialized once at simulation start.
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
inline thread_local PlusargsTable plusargs_table;

/// Initialize plusargs from argc/argv. Call once at simulation start.
inline void InitPlusargs(std::span<char*> argv) {
  plusargs_table = ParsePlusargsFromArgv(argv);
}

/// Initialize plusargs from vector. Used by interpreter.
inline void InitPlusargs(std::vector<std::string> args) {
  plusargs_table = PlusargsTable(std::move(args));
}

/// Get query interface for current plusargs table.
inline auto Plusargs() -> PlusargsQuery {
  return PlusargsQuery(plusargs_table);
}

}  // namespace lyra::sdk
