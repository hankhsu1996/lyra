#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// A single format operation within a display/write call.
// Either a literal string or a formatted value.
struct FormatOp {
  FormatKind kind;
  std::optional<ExpressionId> value;  // nullopt for kLiteral
  std::string literal;                // only valid when kind == kLiteral

  auto operator==(const FormatOp&) const -> bool = default;
};

struct DisplaySystemCallData {
  PrintKind print_kind;
  std::vector<FormatOp> ops;

  auto operator==(const DisplaySystemCallData&) const -> bool = default;
};

struct SeveritySystemCallData {
  Severity level;
  std::vector<ExpressionId> args;

  auto operator==(const SeveritySystemCallData&) const -> bool = default;
};

using SystemCallExpressionData =
    std::variant<DisplaySystemCallData, SeveritySystemCallData>;

}  // namespace lyra::hir
