#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Result of evaluating an rvalue - contains both value and unknown planes.
// For 2-state types, unknown is nullptr.
// For 4-state types, unknown contains the unknown bit plane.
struct RvalueValue {
  llvm::Value* value;    // Value plane (always present)
  llvm::Value* unknown;  // Unknown plane (nullptr for 2-state)

  static auto TwoState(llvm::Value* v) -> RvalueValue {
    return {.value = v, .unknown = nullptr};
  }
  static auto FourState(llvm::Value* v, llvm::Value* u) -> RvalueValue {
    return {.value = v, .unknown = u};
  }
  [[nodiscard]] auto IsFourState() const -> bool {
    return unknown != nullptr;
  }
};

// Evaluate rvalue and return computed LLVM value.
// Does NOT store to any place - caller must handle storage.
auto LowerRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type)
    -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
