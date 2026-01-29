#pragma once

#include <llvm/IR/Value.h>

namespace lyra::lowering::mir_to_llvm {

// A compute result that can be either 2-state or 4-state.
// This is the interface between op implementations and storage finalization.
struct ComputeResult {
  llvm::Value* value;    // Always present (the value plane)
  llvm::Value* unknown;  // null for 2-state, non-null for 4-state

  [[nodiscard]] auto IsFourState() const -> bool {
    return unknown != nullptr;
  }

  static auto TwoState(llvm::Value* v) -> ComputeResult {
    return {.value = v, .unknown = nullptr};
  }

  static auto FourState(llvm::Value* v, llvm::Value* u) -> ComputeResult {
    return {.value = v, .unknown = u};
  }
};

}  // namespace lyra::lowering::mir_to_llvm
