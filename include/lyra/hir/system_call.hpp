#pragma once

#include <variant>
#include <vector>

#include "lyra/common/system_function.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

struct DisplaySystemCallData {
  PrintRadix radix;
  bool append_newline;
  std::vector<ExpressionId> args;

  auto operator==(const DisplaySystemCallData&) const -> bool = default;
};

using SystemCallExpressionData = std::variant<DisplaySystemCallData>;

}  // namespace lyra::hir
