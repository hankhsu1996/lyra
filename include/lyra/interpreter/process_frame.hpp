#pragma once

#include <vector>

#include "lyra/interpreter/call_frame.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/interpreter/variable_table.hpp"
#include "lyra/lir/module.hpp"

namespace lyra::interpreter {

/// ProcessFrame is the interpreter's analog of a coroutine frame.
///
/// In C++ codegen, processes become coroutines and local variables live in
/// the coroutine frame (heap-allocated, persists across suspension).
/// ProcessFrame mirrors this: it holds all per-process execution state that
/// must survive across delay/event suspension.
///
/// When a process suspends (@(posedge clk), #delay), the frame is stored
/// with the pending event and restored when the process resumes.
class ProcessFrame {
 public:
  ProcessFrame() = default;

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
