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
  // Caller's active decision owner, or null for contexts without one.
  // Non-null when the callable received the owner via hidden param
  // (user functions with accepts_decision_owner) or process entry setup.
  llvm::Value* decision_owner_id = nullptr;
};

}  // namespace lyra::lowering::mir_to_llvm
