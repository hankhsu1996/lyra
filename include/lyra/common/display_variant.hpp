#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace lyra::common {

/// Radix for display formatting.
enum class DisplayRadix : uint8_t {
  kDecimal,  // 'd' - $display, $write, etc.
  kHex,      // 'h' - $displayh, $writeh, etc.
  kOctal,    // 'o' - $displayo, $writeo, etc.
  kBinary,   // 'b' - $displayb, $writeb, etc.
};

/// Convert radix enum to format character.
constexpr auto RadixToChar(DisplayRadix r) -> char {
  switch (r) {
    case DisplayRadix::kDecimal:
      return 'd';
    case DisplayRadix::kHex:
      return 'x';  // Use 'x' for std::format compatibility
    case DisplayRadix::kOctal:
      return 'o';
    case DisplayRadix::kBinary:
      return 'b';
  }
  return 'd';  // Unreachable, but silences warnings
}

/// Structured properties for display-like system calls.
/// This is the single source of truth for variant behavior.
struct DisplayVariantProps {
  DisplayRadix radix = DisplayRadix::kDecimal;
  bool append_newline = true;        // $display vs $write
  bool has_file_descriptor = false;  // $fdisplay vs $display
  bool has_output_target = false;    // $sformat, $swrite, $value$plusargs
  bool returns_string = false;       // $sformatf
};

/// Get properties from system call name.
/// Returns nullopt for non-display-like calls.
auto GetDisplayVariantProps(std::string_view name)
    -> std::optional<DisplayVariantProps>;

/// Check if a system call is display-like (has format string semantics).
auto IsDisplayLikeCall(std::string_view name) -> bool;

/// Severity level for $fatal, $error, $warning, $info.
enum class SeverityLevel : uint8_t {
  kInfo,
  kWarning,
  kError,
  kFatal,
};

/// Properties for severity system calls.
struct SeverityProps {
  SeverityLevel level = SeverityLevel::kInfo;
  std::optional<std::string> source_file;
  std::optional<uint32_t> source_line;
};

/// Get severity level from system call name.
/// Returns nullopt for non-severity calls.
auto GetSeverityLevel(std::string_view name) -> std::optional<SeverityLevel>;

}  // namespace lyra::common
