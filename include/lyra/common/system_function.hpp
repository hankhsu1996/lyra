#pragma once

#include <array>
#include <cstdint>
#include <string_view>
#include <variant>

#include "lyra/common/severity.hpp"

namespace lyra {

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

// $monitor/$monitorb/$monitoro/$monitorh - persistent change-triggered display
struct MonitorFunctionInfo {
  PrintRadix radix;
};

// $monitoron/$monitoroff - control monitor enabled state
struct MonitorControlFunctionInfo {
  bool enable;  // true=$monitoron, false=$monitoroff
};

// Time query functions ($time, $stime, $realtime)
enum class TimeKind : uint8_t { kTime64, kSTime32, kRealTime };
struct TimeFunctionInfo {
  TimeKind kind;
};

// File I/O functions ($fopen, $fclose, $fflush, $fgetc, $ungetc, $fgets,
// $fread)
enum class FileIoKind : uint8_t {
  kOpen,
  kClose,
  kFlush,
  kGetc,
  kUngetc,
  kGets,
  kRead
};
struct FileIoFunctionInfo {
  FileIoKind kind;
};

// Memory file I/O ($readmemh/b, $writememh/b)
enum class MemIoKind : uint8_t { kReadMemH, kReadMemB, kWriteMemH, kWriteMemB };
struct MemIoFunctionInfo {
  MemIoKind kind;
};

struct MemIoTraitsResult {
  bool is_read;
  bool is_hex;
};
constexpr auto MemIoTraits(MemIoKind kind) -> MemIoTraitsResult {
  switch (kind) {
    case MemIoKind::kReadMemH:
      return {.is_read = true, .is_hex = true};
    case MemIoKind::kReadMemB:
      return {.is_read = true, .is_hex = false};
    case MemIoKind::kWriteMemH:
      return {.is_read = false, .is_hex = true};
    case MemIoKind::kWriteMemB:
      return {.is_read = false, .is_hex = false};
  }
  // Unreachable - all enum values handled
  return {.is_read = false, .is_hex = false};
}

// Plusargs functions ($test$plusargs, $value$plusargs)
enum class PlusargsKind : uint8_t { kTest, kValue };
struct PlusargsFunctionInfo {
  PlusargsKind kind;
};

// Random functions ($random, $urandom)
enum class RandomKind : uint8_t { kRandom, kUrandom };
struct RandomFunctionInfo {
  RandomKind kind;
};

// $system - execute shell command (IEEE 1800-2023 20.18.1)
struct SystemCmdFunctionInfo {};

// $printtimescale
struct PrintTimescaleFunctionInfo {};

using SystemCallPayload = std::variant<
    DisplayFunctionInfo, TerminationFunctionInfo, SeverityFunctionInfo,
    FatalFunctionInfo, SFormatFunctionInfo, TimeFormatFunctionInfo,
    MonitorFunctionInfo, MonitorControlFunctionInfo, TimeFunctionInfo,
    FileIoFunctionInfo, MemIoFunctionInfo, PlusargsFunctionInfo,
    PrintTimescaleFunctionInfo, RandomFunctionInfo, SystemCmdFunctionInfo>;

struct SystemFunctionInfo {
  std::string_view name;
  uint8_t min_args;
  uint8_t max_args;
  SystemCallPayload payload;
};

// clang-format off
inline constexpr std::array kSystemFunctions = std::to_array<SystemFunctionInfo>({
  // $display family (stdout, with newline)
  {.name = "$display",  .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = true}},
  {.name = "$displayb", .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = true}},
  {.name = "$displayo", .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = true}},
  {.name = "$displayh", .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = true}},

  // $write family (stdout, no newline)
  {.name = "$write",  .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = false}},
  {.name = "$writeb", .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = false}},
  {.name = "$writeo", .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = false}},
  {.name = "$writeh", .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = false}},

  // $fdisplay family (file-directed, with newline)
  {.name = "$fdisplay",  .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = true}},
  {.name = "$fdisplayb", .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = true}},
  {.name = "$fdisplayo", .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = true}},
  {.name = "$fdisplayh", .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = true}},

  // $fwrite family (file-directed, no newline)
  {.name = "$fwrite",  .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = false}},
  {.name = "$fwriteb", .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = false}},
  {.name = "$fwriteo", .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = false}},
  {.name = "$fwriteh", .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = false}},

  // $strobe family (stdout, with newline, deferred to Postponed region)
  {.name = "$strobe",  .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = true, .is_strobe = true}},
  {.name = "$strobeb", .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = true, .is_strobe = true}},
  {.name = "$strobeo", .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = true, .is_strobe = true}},
  {.name = "$strobeh", .min_args = 0, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = true, .is_strobe = true}},

  // $fstrobe family (file-directed, with newline, deferred to Postponed region)
  {.name = "$fstrobe",  .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kDecimal, .append_newline = true, .is_strobe = true}},
  {.name = "$fstrobeb", .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kBinary,  .append_newline = true, .is_strobe = true}},
  {.name = "$fstrobeo", .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kOctal,   .append_newline = true, .is_strobe = true}},
  {.name = "$fstrobeh", .min_args = 1, .max_args = 255, .payload = DisplayFunctionInfo{.radix = PrintRadix::kHex,     .append_newline = true, .is_strobe = true}},

  // Simulation control tasks
  {.name = "$finish", .min_args = 0, .max_args = 1, .payload = TerminationFunctionInfo{.type = TerminationType::kFinish, .default_level = 1}},
  {.name = "$stop",   .min_args = 0, .max_args = 1, .payload = TerminationFunctionInfo{.type = TerminationType::kStop,   .default_level = 1}},
  {.name = "$exit",   .min_args = 0, .max_args = 0, .payload = TerminationFunctionInfo{.type = TerminationType::kExit,   .default_level = 1}},

  // Severity system tasks (non-terminating)
  {.name = "$info",    .min_args = 0, .max_args = 255, .payload = SeverityFunctionInfo{.level = Severity::kInfo}},
  {.name = "$warning", .min_args = 0, .max_args = 255, .payload = SeverityFunctionInfo{.level = Severity::kWarning}},
  {.name = "$error",   .min_args = 0, .max_args = 255, .payload = SeverityFunctionInfo{.level = Severity::kError}},

  // Fatal severity task (terminating)
  {.name = "$fatal", .min_args = 0, .max_args = 255, .payload = FatalFunctionInfo{}},

  // $sformatf/$sformat/$swrite family (string formatting)
  {.name = "$sformatf", .min_args = 1, .max_args = 255, .payload = SFormatFunctionInfo{.radix = PrintRadix::kDecimal, .has_format_string = true,  .has_output_target = false}},
  {.name = "$sformat",  .min_args = 2, .max_args = 255, .payload = SFormatFunctionInfo{.radix = PrintRadix::kDecimal, .has_format_string = true,  .has_output_target = true}},
  {.name = "$swrite",   .min_args = 1, .max_args = 255, .payload = SFormatFunctionInfo{.radix = PrintRadix::kDecimal, .has_format_string = false, .has_output_target = true}},
  {.name = "$swriteh",  .min_args = 1, .max_args = 255, .payload = SFormatFunctionInfo{.radix = PrintRadix::kHex,     .has_format_string = false, .has_output_target = true}},
  {.name = "$swriteb",  .min_args = 1, .max_args = 255, .payload = SFormatFunctionInfo{.radix = PrintRadix::kBinary,  .has_format_string = false, .has_output_target = true}},
  {.name = "$swriteo",  .min_args = 1, .max_args = 255, .payload = SFormatFunctionInfo{.radix = PrintRadix::kOctal,   .has_format_string = false, .has_output_target = true}},

  // Time formatting task
  {.name = "$timeformat", .min_args = 0, .max_args = 4, .payload = TimeFormatFunctionInfo{}},

  // $monitor family (persistent change-triggered display)
  {.name = "$monitor",    .min_args = 0, .max_args = 255, .payload = MonitorFunctionInfo{.radix = PrintRadix::kDecimal}},
  {.name = "$monitorb",   .min_args = 0, .max_args = 255, .payload = MonitorFunctionInfo{.radix = PrintRadix::kBinary}},
  {.name = "$monitoro",   .min_args = 0, .max_args = 255, .payload = MonitorFunctionInfo{.radix = PrintRadix::kOctal}},
  {.name = "$monitorh",   .min_args = 0, .max_args = 255, .payload = MonitorFunctionInfo{.radix = PrintRadix::kHex}},
  {.name = "$monitoron",  .min_args = 0, .max_args = 0,   .payload = MonitorControlFunctionInfo{.enable = true}},
  {.name = "$monitoroff", .min_args = 0, .max_args = 0,   .payload = MonitorControlFunctionInfo{.enable = false}},

  // $fmonitor family (file-directed persistent change-triggered display)
  {.name = "$fmonitor",  .min_args = 1, .max_args = 255, .payload = MonitorFunctionInfo{.radix = PrintRadix::kDecimal}},
  {.name = "$fmonitorb", .min_args = 1, .max_args = 255, .payload = MonitorFunctionInfo{.radix = PrintRadix::kBinary}},
  {.name = "$fmonitoro", .min_args = 1, .max_args = 255, .payload = MonitorFunctionInfo{.radix = PrintRadix::kOctal}},
  {.name = "$fmonitorh", .min_args = 1, .max_args = 255, .payload = MonitorFunctionInfo{.radix = PrintRadix::kHex}},

  // Time query functions
  {.name = "$time",     .min_args = 0, .max_args = 0, .payload = TimeFunctionInfo{.kind = TimeKind::kTime64}},
  {.name = "$stime",    .min_args = 0, .max_args = 0, .payload = TimeFunctionInfo{.kind = TimeKind::kSTime32}},
  {.name = "$realtime", .min_args = 0, .max_args = 0, .payload = TimeFunctionInfo{.kind = TimeKind::kRealTime}},

  // File I/O
  {.name = "$fopen",  .min_args = 1, .max_args = 2, .payload = FileIoFunctionInfo{.kind = FileIoKind::kOpen}},
  {.name = "$fclose", .min_args = 1, .max_args = 1, .payload = FileIoFunctionInfo{.kind = FileIoKind::kClose}},
  {.name = "$fflush", .min_args = 0, .max_args = 1, .payload = FileIoFunctionInfo{.kind = FileIoKind::kFlush}},
  {.name = "$fgetc",  .min_args = 1, .max_args = 1, .payload = FileIoFunctionInfo{.kind = FileIoKind::kGetc}},
  {.name = "$ungetc", .min_args = 2, .max_args = 2, .payload = FileIoFunctionInfo{.kind = FileIoKind::kUngetc}},
  {.name = "$fgets",  .min_args = 2, .max_args = 2, .payload = FileIoFunctionInfo{.kind = FileIoKind::kGets}},
  {.name = "$fread",  .min_args = 2, .max_args = 4, .payload = FileIoFunctionInfo{.kind = FileIoKind::kRead}},

  // Memory file I/O
  {.name = "$readmemh",  .min_args = 2, .max_args = 4, .payload = MemIoFunctionInfo{.kind = MemIoKind::kReadMemH}},
  {.name = "$readmemb",  .min_args = 2, .max_args = 4, .payload = MemIoFunctionInfo{.kind = MemIoKind::kReadMemB}},
  {.name = "$writememh", .min_args = 2, .max_args = 4, .payload = MemIoFunctionInfo{.kind = MemIoKind::kWriteMemH}},
  {.name = "$writememb", .min_args = 2, .max_args = 4, .payload = MemIoFunctionInfo{.kind = MemIoKind::kWriteMemB}},

  // Plusargs functions
  {.name = "$test$plusargs",  .min_args = 1, .max_args = 1, .payload = PlusargsFunctionInfo{.kind = PlusargsKind::kTest}},
  {.name = "$value$plusargs", .min_args = 2, .max_args = 2, .payload = PlusargsFunctionInfo{.kind = PlusargsKind::kValue}},

  // Timescale printing
  {.name = "$printtimescale", .min_args = 0, .max_args = 1, .payload = PrintTimescaleFunctionInfo{}},

  // Random functions
  {.name = "$random",  .min_args = 0, .max_args = 0, .payload = RandomFunctionInfo{.kind = RandomKind::kRandom}},
  {.name = "$urandom", .min_args = 0, .max_args = 0, .payload = RandomFunctionInfo{.kind = RandomKind::kUrandom}},

  // Shell command ($system)
  {.name = "$system", .min_args = 0, .max_args = 1, .payload = SystemCmdFunctionInfo{}},
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
