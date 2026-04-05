#pragma once

namespace llvm {
class Value;
}  // namespace llvm

namespace lyra::lowering::mir_to_llvm {

// Function-entry-local execution mode carrier.
// Resolved once at function entry from the callable's ABI contract and
// threaded through statement/effect/call lowering for the active function.
// NOT stored on Context -- this is a local fact, not ambient state.
struct ActiveExecutionMode {
  // Caller's active process_id, or null for non-process entry.
  // Non-null when the callable received process_id via hidden param
  // (user functions with accepts_process_ownership) or process entry setup.
  llvm::Value* process_id = nullptr;
};

}  // namespace lyra::lowering::mir_to_llvm
