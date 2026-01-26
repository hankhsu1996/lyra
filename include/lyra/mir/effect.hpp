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
  std::vector<Operand> args;
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

// $timeformat: sets global time format for %t.
// All values are compile-time constants resolved in AST->HIR.
struct TimeFormatEffect {
  int8_t units = 127;  // Time unit power (127 = use global precision)
  int precision = 0;   // Decimal digits
  std::string suffix;  // Appended string
  int min_width = 20;  // Minimum field width
};

// EffectOp is the variant of all effect operations.
// Effect operations produce side effects but no value.
// Note: Builtin methods are now unified as Rvalue (kBuiltinCall), not Effect.
using EffectOp = std::variant<
    DisplayEffect, SeverityEffect, MemIOEffect, TimeFormatEffect,
    SystemTfEffect>;

}  // namespace lyra::mir
