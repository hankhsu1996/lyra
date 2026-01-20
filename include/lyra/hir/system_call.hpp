#pragma once

#include <variant>
#include <vector>

#include "lyra/common/severity.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

struct DisplaySystemCallData {
  PrintRadix radix;
  bool append_newline;
  std::vector<ExpressionId> args;

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
