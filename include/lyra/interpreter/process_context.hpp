#pragma once

#include <vector>

#include "lyra/interpreter/call_frame.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/interpreter/variable_table.hpp"
#include "lyra/lir/module.hpp"

namespace lyra::interpreter {

class ProcessContext {
 public:
  ProcessContext() = default;

  ProcessVariableTable variable_table;
  TempTable temp_table;

  /// Call stack for function invocations.
  /// Empty when executing process code, non-empty when inside functions.
  std::vector<CallFrame> call_stack;

  /// Returns the current function being executed, or nullptr if in process
  /// code.
  [[nodiscard]] auto CurrentFunction() const -> const lir::Function* {
    return call_stack.empty() ? nullptr : call_stack.back().function;
  }
};

}  // namespace lyra::interpreter
