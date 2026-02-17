#include "lyra/llvm_backend/compute/four_state_ops.hpp"

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/type_query.hpp"

namespace lyra::lowering::mir_to_llvm {

auto IsTypeFourState(
    const TypeArena& types, TypeId type_id, bool force_two_state) -> bool {
  const Type& type = types[type_id];
  if (!IsPacked(type)) {
    return false;
  }
  return mir_to_llvm::IsPackedFourState(type, types, force_two_state);
}

auto ExtractFourState(llvm::IRBuilderBase& builder, llvm::Value* struct_val)
    -> FourStateValue {
  auto* val = builder.CreateExtractValue(struct_val, 0, "fs.val");
  auto* unk = builder.CreateExtractValue(struct_val, 1, "fs.unk");
  return {.value = val, .unknown = unk};
}

auto ExtractFourStateOrZero(llvm::IRBuilderBase& builder, llvm::Value* val)
    -> FourStateValue {
  if (val->getType()->isStructTy()) {
    return ExtractFourState(builder, val);
  }
  auto* zero = llvm::ConstantInt::get(val->getType(), 0);
  return {.value = val, .unknown = zero};
}

auto PackFourState(
    llvm::IRBuilderBase& builder, llvm::StructType* struct_type,
    llvm::Value* val, llvm::Value* unk) -> llvm::Value* {
  llvm::Value* result = llvm::UndefValue::get(struct_type);
  result = builder.CreateInsertValue(result, val, 0, "fs.pack.val");
  result = builder.CreateInsertValue(result, unk, 1, "fs.pack.unk");
  return result;
}

auto MakeKnown(
    llvm::IRBuilderBase& builder, llvm::StructType* struct_type,
    llvm::Value* val) -> llvm::Value* {
  auto* elem_type = GetFourStateElemIntType(struct_type);
  auto* zero = llvm::ConstantInt::get(elem_type, 0);
  return PackFourState(builder, struct_type, val, zero);
}

auto GetFourStateElemIntType(llvm::StructType* struct_type)
    -> llvm::IntegerType* {
  return llvm::cast<llvm::IntegerType>(struct_type->getElementType(0));
}

auto FourStateAnd(
    llvm::IRBuilderBase& builder, FourStateValue lhs, FourStateValue rhs)
    -> FourStateValue {
  // IEEE 1800 AND truth table: 0 & X = 0 (known-zero dominates)
  // known0 = ~value & ~unknown (bit is definitely 0)
  // known1 = value & ~unknown (bit is definitely 1)
  auto* not_lhs_val = builder.CreateNot(lhs.value, "and4.nlv");
  auto* not_lhs_unk = builder.CreateNot(lhs.unknown, "and4.nlu");
  auto* not_rhs_val = builder.CreateNot(rhs.value, "and4.nrv");
  auto* not_rhs_unk = builder.CreateNot(rhs.unknown, "and4.nru");

  auto* known0_lhs = builder.CreateAnd(not_lhs_val, not_lhs_unk, "and4.k0l");
  auto* known1_lhs = builder.CreateAnd(lhs.value, not_lhs_unk, "and4.k1l");
  auto* known0_rhs = builder.CreateAnd(not_rhs_val, not_rhs_unk, "and4.k0r");
  auto* known1_rhs = builder.CreateAnd(rhs.value, not_rhs_unk, "and4.k1r");

  // Result is 1 only when both operands are known-1
  auto* result_val = builder.CreateAnd(known1_lhs, known1_rhs, "and4.val");

  // Result is unknown when neither operand forces a known result:
  // - Not known-0 on either side (known-0 dominates to produce 0)
  // - Not known-1 on both sides (that produces 1)
  auto* either_k0 = builder.CreateOr(known0_lhs, known0_rhs, "and4.ek0");
  auto* not_either_k0 = builder.CreateNot(either_k0, "and4.nek0");
  auto* result_unk = builder.CreateAnd(
      not_either_k0, builder.CreateNot(result_val), "and4.unk");

  return {.value = result_val, .unknown = result_unk};
}

auto FourStateOr(
    llvm::IRBuilderBase& builder, FourStateValue lhs, FourStateValue rhs)
    -> FourStateValue {
  // IEEE 1800 OR truth table: 1 | X = 1 (known-one dominates)
  auto* not_lhs_val = builder.CreateNot(lhs.value, "or4.nlv");
  auto* not_lhs_unk = builder.CreateNot(lhs.unknown, "or4.nlu");
  auto* not_rhs_val = builder.CreateNot(rhs.value, "or4.nrv");
  auto* not_rhs_unk = builder.CreateNot(rhs.unknown, "or4.nru");

  auto* known0_lhs = builder.CreateAnd(not_lhs_val, not_lhs_unk, "or4.k0l");
  auto* known1_lhs = builder.CreateAnd(lhs.value, not_lhs_unk, "or4.k1l");
  auto* known0_rhs = builder.CreateAnd(not_rhs_val, not_rhs_unk, "or4.k0r");
  auto* known1_rhs = builder.CreateAnd(rhs.value, not_rhs_unk, "or4.k1r");

  // Result is 1 when either operand is known-1
  auto* result_val = builder.CreateOr(known1_lhs, known1_rhs, "or4.val");

  // Result is unknown when:
  // - Not known-1 on either side (known-1 dominates to produce 1)
  // - Not known-0 on both sides (that produces 0)
  auto* both_k0 = builder.CreateAnd(known0_lhs, known0_rhs, "or4.bk0");
  auto* not_result_val = builder.CreateNot(result_val, "or4.nrv");
  auto* result_unk =
      builder.CreateAnd(not_result_val, builder.CreateNot(both_k0), "or4.unk");

  return {.value = result_val, .unknown = result_unk};
}

auto FourStateXor(
    llvm::IRBuilderBase& builder, FourStateValue lhs, FourStateValue rhs)
    -> FourStateValue {
  // XOR: any unknown bit propagates (no dominant value)
  auto* combined_unk = builder.CreateOr(lhs.unknown, rhs.unknown, "xor4.unk");
  auto* xor_val = builder.CreateXor(lhs.value, rhs.value, "xor4.raw");
  // Maintain invariant: value & unknown == 0
  auto* result_val =
      builder.CreateAnd(xor_val, builder.CreateNot(combined_unk), "xor4.val");
  return {.value = result_val, .unknown = combined_unk};
}

auto FourStateNot(llvm::IRBuilderBase& builder, FourStateValue src)
    -> FourStateValue {
  // NOT: invert known bits, preserve unknown
  // ~(known-0) = known-1, ~(known-1) = known-0, ~X = X
  auto* not_val = builder.CreateNot(src.value, "not4.inv");
  // Maintain invariant: value & unknown == 0
  auto* result_val =
      builder.CreateAnd(not_val, builder.CreateNot(src.unknown), "not4.val");
  return {.value = result_val, .unknown = src.unknown};
}

}  // namespace lyra::lowering::mir_to_llvm
