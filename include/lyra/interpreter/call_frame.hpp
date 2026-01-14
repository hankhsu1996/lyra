#pragma once

#include <cstddef>
#include <optional>
#include <unordered_map>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/module.hpp"

namespace lyra::interpreter {

/// Represents a function invocation on the call stack.
/// Each frame stores local variables, temps, return address, and result
/// destination.
///
/// Lifetime contract: The function pointer must outlive the frame.
/// Function::temps is immutable after lowering; frame captures size at
/// construction.
struct CallFrame {
  /// Construct a call frame for the given function.
  /// Function must outlive this frame.
  explicit CallFrame(const lir::Function* func)
      : function(func), temp_table(func->temps.size()) {
  }

  /// Function being executed (non-null, outlives frame)
  const lir::Function* function;

  /// Return address: block index to resume after return
  size_t return_block_index = 0;

  /// Return address: instruction index to resume after return
  size_t return_instruction_index = 0;

  /// Where to store return value (if any) - stored in caller's temp table
  std::optional<lir::TempRef> return_value_dest;

  /// Local variable storage (parameters + locals)
  /// Key: SymbolId from the function's parameter/local variable
  /// Value: runtime value stored
  std::unordered_map<common::SymbolId, RuntimeValue> local_variables;

  /// Function-local temp storage (for reentrant/recursive calls)
  /// Each call frame has its own temps to avoid conflicts
  TempTable temp_table;

  /// Captured variables for closures (e.g., $monitor prev values).
  /// Unlike local_variables (which use slang symbols), captures use string
  /// names. For persistent closures, this map persists across calls, matching
  /// C++ mutable lambda capture semantics.
  std::unordered_map<std::string, RuntimeValue> captures;
};

}  // namespace lyra::interpreter
