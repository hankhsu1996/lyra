#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/common/severity.hpp"
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
};

// DisplayEffect represents a $display/$write family system task.
// These are immediate observable effects that print to stdout.
struct DisplayEffect {
  PrintKind print_kind;
  std::vector<FormatOp> ops;
};

// SeverityEffect represents $info/$warning/$error system tasks.
// These print severity-prefixed messages and continue execution.
struct SeverityEffect {
  Severity level;
  std::vector<Operand> args;
};

// EffectOp is the variant of all effect operations.
// Effect operations produce side effects but no value.
// Note: Builtin methods are now unified as Rvalue (kBuiltinCall), not Effect.
using EffectOp = std::variant<DisplayEffect, SeverityEffect>;

}  // namespace lyra::mir
