#pragma once

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/temp_value.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

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

  // Convert this RvalueValue into a TempValue for temp binding.
  // Domain is derived from whether unknown is nullptr.
  [[nodiscard]] auto ToTempValue(TypeId declared_type) const -> TempValue {
    if (IsFourState()) {
      return {declared_type, ValueDomain::kFourState, value, unknown};
    }
    return {declared_type, ValueDomain::kTwoState, value, nullptr};
  }
};

// The SOLE raw reconstruction helper for temp-backed values. All
// temp-to-LLVM raw packing MUST go through this function.
// Pure representation adapter: TempValue -> legacy raw llvm::Value*.
// For kTwoState: returns tv.value (scalar).
// For kFourState: packs {tv.value, tv.unknown} into struct.
auto BuildRawValueFromTempValue(llvm::IRBuilder<>& builder, const TempValue& tv)
    -> llvm::Value*;

// Evaluate rvalue and return computed LLVM value.
// Does NOT store to any place - caller must handle storage.
auto LowerRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type)
    -> Result<RvalueValue>;

// Resolver-aware overload.
auto LowerRvalue(
    Context& context, SlotAccessResolver& resolver, const mir::Rvalue& rvalue,
    TypeId result_type) -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
