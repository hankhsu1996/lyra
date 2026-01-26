#pragma once

#include <array>
#include <cstdint>
#include <string_view>
#include <variant>

#include "lyra/common/severity.hpp"

namespace lyra {

enum class SystemFunctionReturnType : uint8_t {
  kVoid,
  kString,
};

enum class PrintRadix : uint8_t {
  kDecimal,
  kBinary,
  kOctal,
  kHex,
};

struct DisplayFunctionInfo {
  PrintRadix radix;
  bool append_newline;
  bool is_strobe = false;  // true for $strobe family (deferred to Postponed)
};

enum class TerminationType : uint8_t {
  kFinish,  // $finish - normal termination
  kStop,    // $stop - pause for debugger
  kExit,    // $exit - synonym for $finish
};

struct TerminationFunctionInfo {
  TerminationType type;
  int default_level;  // Default level if not specified (usually 1)
};

// $info, $warning, $error - non-terminating severity messages
struct SeverityFunctionInfo {
  Severity level;
};

// $fatal - terminating severity message (handled specially in statement.cpp)
struct FatalFunctionInfo {};

// $sformatf, $sformat, $swrite, $swriteh, $swriteb, $swriteo
struct SFormatFunctionInfo {
  PrintRadix radix;
  bool has_format_string;  // true for $sformatf/$sformat
  bool has_output_target;  // true for $sformat/$swrite*
};

// $timeformat - sets global time format for %t
struct TimeFormatFunctionInfo {};

using CategoryPayload = std::variant<
    DisplayFunctionInfo, TerminationFunctionInfo, SeverityFunctionInfo,
    FatalFunctionInfo, SFormatFunctionInfo, TimeFormatFunctionInfo>;

struct SystemFunctionInfo {
  std::string_view name;
  uint8_t min_args;
  uint8_t max_args;
  SystemFunctionReturnType return_type;
  CategoryPayload payload;
};

using Ret = SystemFunctionReturnType;

// clang-format off
inline constexpr std::array kSystemFunctions = std::to_array<SystemFunctionInfo>({
  // $display family (stdout, with newline)
  {.name = "$display",  .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = true}},
  {.name = "$displayb", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = true}},
  {.name = "$displayo", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = true}},
  {.name = "$displayh", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = true}},

  // $write family (stdout, no newline)
  {.name = "$write",  .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = false}},
  {.name = "$writeb", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = false}},
  {.name = "$writeo", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = false}},
  {.name = "$writeh", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = false}},

  // $strobe family (stdout, with newline, deferred to Postponed region)
  {.name = "$strobe",  .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = true, .is_strobe = true}},
  {.name = "$strobeb", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = true, .is_strobe = true}},
  {.name = "$strobeo", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = true, .is_strobe = true}},
  {.name = "$strobeh", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = true, .is_strobe = true}},

  // Simulation control tasks
  {.name = "$finish", .min_args = 0, .max_args = 1, .return_type = Ret::kVoid, .payload = TerminationFunctionInfo{.type = TerminationType::kFinish, .default_level = 1}},
  {.name = "$stop",   .min_args = 0, .max_args = 1, .return_type = Ret::kVoid, .payload = TerminationFunctionInfo{.type = TerminationType::kStop,   .default_level = 1}},
  {.name = "$exit",   .min_args = 0, .max_args = 0, .return_type = Ret::kVoid, .payload = TerminationFunctionInfo{.type = TerminationType::kExit,   .default_level = 1}},

  // Severity system tasks (non-terminating)
  {.name = "$info",    .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = SeverityFunctionInfo{.level = Severity::kInfo}},
  {.name = "$warning", .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = SeverityFunctionInfo{.level = Severity::kWarning}},
  {.name = "$error",   .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = SeverityFunctionInfo{.level = Severity::kError}},

  // Fatal severity task (terminating)
  {.name = "$fatal",   .min_args = 0, .max_args = 255, .return_type = Ret::kVoid, .payload = FatalFunctionInfo{}},

  // $sformatf/$sformat/$swrite family (string formatting)
  {.name = "$sformatf", .min_args = 1, .max_args = 255, .return_type = Ret::kString, .payload = SFormatFunctionInfo{.radix = PrintRadix::kDecimal, .has_format_string = true,  .has_output_target = false}},
  {.name = "$sformat",  .min_args = 2, .max_args = 255, .return_type = Ret::kVoid,   .payload = SFormatFunctionInfo{.radix = PrintRadix::kDecimal, .has_format_string = true,  .has_output_target = true}},
  {.name = "$swrite",   .min_args = 1, .max_args = 255, .return_type = Ret::kVoid,   .payload = SFormatFunctionInfo{.radix = PrintRadix::kDecimal, .has_format_string = false, .has_output_target = true}},
  {.name = "$swriteh",  .min_args = 1, .max_args = 255, .return_type = Ret::kVoid,   .payload = SFormatFunctionInfo{.radix = PrintRadix::kHex,     .has_format_string = false, .has_output_target = true}},
  {.name = "$swriteb",  .min_args = 1, .max_args = 255, .return_type = Ret::kVoid,   .payload = SFormatFunctionInfo{.radix = PrintRadix::kBinary,  .has_format_string = false, .has_output_target = true}},
  {.name = "$swriteo",  .min_args = 1, .max_args = 255, .return_type = Ret::kVoid,   .payload = SFormatFunctionInfo{.radix = PrintRadix::kOctal,   .has_format_string = false, .has_output_target = true}},

  // Time formatting task
  {.name = "$timeformat", .min_args = 0, .max_args = 4, .return_type = Ret::kVoid, .payload = TimeFormatFunctionInfo{}},
});
// clang-format on

constexpr auto FindSystemFunction(std::string_view name)
    -> const SystemFunctionInfo* {
  for (const auto& info : kSystemFunctions) {
    if (info.name == name) {
      return &info;
    }
  }
  return nullptr;
}

constexpr auto IsSystemFunctionSupported(std::string_view name) -> bool {
  return FindSystemFunction(name) != nullptr;
}

}  // namespace lyra
