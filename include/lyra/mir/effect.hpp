#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

// A single format operation within a display/write call.
// Either a literal string or a formatted value.
struct FormatOp {
  FormatKind kind;
  std::optional<Operand> value;  // nullopt for kLiteral
  std::string literal;           // only valid when kind == kLiteral
  TypeId type;           // for width/signedness lookup (invalid for kLiteral)
  FormatModifiers mods;  // width, precision, flags
  int8_t module_timeunit_power =
      -9;  // For kTime: unit of value (from scope's timescale)
};

// DisplayEffect represents a $display/$write/$fdisplay/$fwrite family system
// task. When descriptor is absent, output goes to stdout.
struct DisplayEffect {
  PrintKind print_kind;
  std::vector<FormatOp> ops;
  std::optional<Operand> descriptor;  // nullopt = stdout
};

// SeverityEffect represents $info/$warning/$error system tasks.
// These print severity-prefixed messages and continue execution.
struct SeverityEffect {
  Severity level;
  std::vector<FormatOp> ops;
};

// MemIOEffect represents $readmemh/$readmemb/$writememh/$writememb system
// tasks. These read/write memory files to/from unpacked array variables.
struct MemIOEffect {
  bool is_read = false;  // true = readmem, false = writemem
  bool is_hex = false;   // true = hex, false = binary
  PlaceId target;        // The memory array (written for read, read for write)
  TypeId target_type;    // Array type (for element width/count)
  Operand filename;      // String operand
  std::optional<Operand> start_addr;
  std::optional<Operand> end_addr;
};

// SystemTfEffect: Generic effect-only system TFs.
// Covers simple system TFs where payload is just opcode + operands.
// Operands are stored here (Effect pattern).
struct SystemTfEffect {
  SystemTfOpcode opcode;
  std::vector<Operand> args;
};

// StrobeEffect: $strobe system task (IEEE 1800-2023 §21.2.2).
// Unlike $display (immediate output), $strobe defers printing to the Postponed
// region and re-evaluates expressions using end-of-timestep values.
// The thunk is a synthetic MIR function that reads fresh values and prints.
struct StrobeEffect {
  FunctionId thunk;  // Synthetic function that performs the print
};

// $timeformat: sets global time format for %t.
// All values are compile-time constants resolved in AST->HIR.
struct TimeFormatEffect {
  int8_t units = 127;  // Time unit power (127 = use global precision)
  int precision = 0;   // Decimal digits
  std::string suffix;  // Appended string
  int min_width = 20;  // Minimum field width
};

// $monitor: persistent change-triggered display (IEEE 1800 §21.2.3).
// The setup_thunk performs initial print and registers the check_thunk with
// runtime. The check_thunk FunctionId is passed to runtime at registration,
// not stored in MIR.
struct MonitorEffect {
  FunctionId setup_thunk;            // Initial print + registration
  FunctionId check_thunk;            // Check for changes and print
  PrintKind print_kind;              // For print end call
  std::vector<FormatOp> format_ops;  // For LLVM comparison code generation
  std::vector<uint32_t> offsets;     // Per-operand offset into prev_buffer
  std::vector<uint32_t> byte_sizes;  // Per-operand byte size
  uint32_t prev_buffer_size = 0;     // Total size of prev_values buffer
};

// $monitoron/$monitoroff: control monitor enabled state.
// No-op if no active monitor (graceful handling).
struct MonitorControlEffect {
  bool enable;  // true=$monitoron, false=$monitoroff
};

struct FillPackedEffect {
  PlaceId target;
  Operand fill_value;
  uint32_t unit_bits = 0;
  uint32_t count = 0;
  uint32_t total_bits = 0;
};

// EffectOp is the variant of all effect operations.
// Effect operations produce side effects but no value.
// Note: Builtin methods are now unified as Rvalue (kBuiltinCall), not Effect.
using EffectOp = std::variant<
    DisplayEffect, SeverityEffect, MemIOEffect, TimeFormatEffect,
    SystemTfEffect, StrobeEffect, MonitorEffect, MonitorControlEffect,
    FillPackedEffect>;

}  // namespace lyra::mir
