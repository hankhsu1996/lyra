#pragma once

#include <cstdint>
#include <unordered_map>
#include <vector>

#include "lyra/interpreter/call_frame.hpp"
#include "lyra/interpreter/runtime_value.hpp"
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
///
/// Lifetime contract: The process pointer must outlive the frame.
/// Process::temps is immutable after lowering; frame captures size at
/// construction.
class ProcessFrame {
 public:
  /// Construct a process frame for the given process.
  /// Process must outlive this frame.
  explicit ProcessFrame(const lir::Process* proc)
      : process(proc), temp_table(proc->temps.size()) {
  }

  /// The process being executed (non-null, outlives frame)
  const lir::Process* process;

  ProcessVariableTable variable_table;
  TempTable temp_table;

  /// Call stack for function invocations.
  /// Empty when executing process code, non-empty when inside functions.
  std::vector<CallFrame> call_stack;

  /// Anonymous storage arena for kAllocate results.
  /// Maps allocation IDs to their RuntimeValue storage.
  /// Storage is allocated here when kAllocate produces Pointer<T>,
  /// and accessed through Address::AllocRoot in ResolveForRead/Write.
  std::unordered_map<uint64_t, RuntimeValue> anonymous_storage;
  uint64_t next_alloc_id = 0;

  /// Allocate anonymous storage and return its ID.
  auto AllocateAnonymous(RuntimeValue initial) -> uint64_t {
    auto id = next_alloc_id++;
    anonymous_storage[id] = std::move(initial);
    return id;
  }

  /// Read from anonymous storage.
  [[nodiscard]] auto ReadAnonymous(uint64_t id) const -> const RuntimeValue& {
    return anonymous_storage.at(id);
  }

  /// Write to anonymous storage.
  auto WriteAnonymous(uint64_t id, RuntimeValue value) -> void {
    anonymous_storage.at(id) = std::move(value);
  }

  /// Returns the current function being executed, or nullptr if in process
  /// code.
  [[nodiscard]] auto CurrentFunction() const -> const lir::Function* {
    return call_stack.empty() ? nullptr : call_stack.back().function;
  }
};

}  // namespace lyra::interpreter
