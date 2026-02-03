#pragma once

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
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

  // Pack this RvalueValue into a single llvm::Value* suitable for temp binding.
  // For 2-state: returns value directly.
  // For 4-state: returns a {value, unknown} struct.
  //
  // This is the canonical representation for ValueTemps. Using this method
  // ensures consistent packing across all sites that bind temps.
  [[nodiscard]] auto PackForTemp(llvm::IRBuilder<>& builder) const
      -> llvm::Value* {
    if (!IsFourState()) {
      return value;
    }
    // Pack into {value, unknown} struct
    llvm::Type* val_ty = value->getType();
    auto* struct_ty =
        llvm::StructType::get(val_ty->getContext(), {val_ty, val_ty});
    llvm::Value* packed = llvm::UndefValue::get(struct_ty);
    packed = builder.CreateInsertValue(packed, value, 0);
    packed = builder.CreateInsertValue(packed, unknown, 1);
    return packed;
  }
};

// Evaluate rvalue and return computed LLVM value.
// Does NOT store to any place - caller must handle storage.
auto LowerRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type)
    -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
