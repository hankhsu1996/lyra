#include "lyra/llvm_backend/instruction_compute_aggregate.hpp"

#include <cstddef>
#include <cstdint>
#include <format>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void LowerUnpackedArrayAggregate(
    Context& context, const mir::Compute& compute) {
  auto& builder = context.GetBuilder();

  llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
  llvm::Type* arr_type = context.GetPlaceLlvmType(compute.target);
  llvm::Type* elem_type = arr_type->getArrayElementType();

  llvm::Value* aggregate = llvm::UndefValue::get(arr_type);
  for (size_t i = 0; i < compute.value.operands.size(); ++i) {
    llvm::Value* elem =
        LowerOperandAsStorage(context, compute.value.operands[i], elem_type);
    aggregate =
        builder.CreateInsertValue(aggregate, elem, {static_cast<unsigned>(i)});
  }
  builder.CreateStore(aggregate, target_ptr);
}

void LowerUnpackedStructAggregate(
    Context& context, const mir::Compute& compute, const Type& target_type) {
  auto& builder = context.GetBuilder();
  const auto& struct_info = target_type.AsUnpackedStruct();

  if (compute.value.operands.size() != struct_info.fields.size()) {
    throw common::InternalError(
        "LowerUnpackedStructAggregate",
        std::format(
            "struct aggregate operand count {} != field count {}",
            compute.value.operands.size(), struct_info.fields.size()));
  }

  llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
  llvm::Type* struct_type = context.GetPlaceLlvmType(compute.target);

  llvm::Value* aggregate = llvm::UndefValue::get(struct_type);
  for (size_t i = 0; i < compute.value.operands.size(); ++i) {
    llvm::Type* field_type = llvm::cast<llvm::StructType>(struct_type)
                                 ->getElementType(static_cast<unsigned>(i));
    llvm::Value* field_val =
        LowerOperandAsStorage(context, compute.value.operands[i], field_type);
    aggregate = builder.CreateInsertValue(
        aggregate, field_val, {static_cast<unsigned>(i)});
  }
  builder.CreateStore(aggregate, target_ptr);
}

void LowerQueueAggregate(
    Context& context, const mir::Compute& compute, const Type& target_type) {
  auto& builder = context.GetBuilder();

  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());

  size_t n = compute.value.operands.size();
  uint32_t max_bound = target_type.AsQueue().max_bound;
  if (max_bound > 0 && n > static_cast<size_t>(max_bound) + 1) {
    n = static_cast<size_t>(max_bound) + 1;
  }

  TypeId elem_type_id = target_type.AsQueue().element_type;
  auto elem_ops = context.GetElemOpsForType(elem_type_id);

  llvm::Value* handle = builder.CreateCall(
      context.GetLyraDynArrayNew(),
      {llvm::ConstantInt::get(i64_ty, n),
       llvm::ConstantInt::get(i32_ty, elem_ops.elem_size), elem_ops.clone_fn,
       elem_ops.destroy_fn},
      "q.lit.new");

  for (size_t i = 0; i < n; ++i) {
    llvm::Value* elem_ptr = builder.CreateCall(
        context.GetLyraDynArrayElementPtr(),
        {handle, llvm::ConstantInt::get(i64_ty, i)}, "q.lit.ep");
    llvm::Value* val = LowerOperandAsStorage(
        context, compute.value.operands[i], elem_ops.elem_llvm_type);

    if (elem_ops.needs_clone) {
      auto* clone_fn = llvm::cast<llvm::Function>(elem_ops.clone_fn);
      auto* temp =
          builder.CreateAlloca(elem_ops.elem_llvm_type, nullptr, "q.lit.tmp");
      builder.CreateStore(val, temp);
      builder.CreateCall(clone_fn, {elem_ptr, temp});
    } else {
      builder.CreateStore(val, elem_ptr);
    }
  }

  llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
  auto* old = builder.CreateLoad(ptr_ty, target_ptr, "q.lit.old");
  builder.CreateCall(context.GetLyraDynArrayRelease(), {old});
  builder.CreateStore(handle, target_ptr);
}

}  // namespace

void LowerAggregate(
    Context& context, const mir::Compute& compute,
    const mir::AggregateRvalueInfo& /*info*/) {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const Type& target_type =
      types[mir::TypeOfPlace(types, arena[compute.target])];

  switch (target_type.Kind()) {
    case TypeKind::kUnpackedArray:
      LowerUnpackedArrayAggregate(context, compute);
      return;
    case TypeKind::kUnpackedStruct:
      LowerUnpackedStructAggregate(context, compute, target_type);
      return;
    case TypeKind::kQueue:
      LowerQueueAggregate(context, compute, target_type);
      return;
    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
          context.GetCurrentOrigin(),
          std::format(
              "aggregate for type {} not supported", ToString(target_type)));
  }
}

}  // namespace lyra::lowering::mir_to_llvm
