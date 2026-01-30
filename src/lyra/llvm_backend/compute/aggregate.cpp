#include "lyra/llvm_backend/compute/aggregate.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto LowerUnpackedArrayAggregateValue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();

  auto arr_type_result = BuildLlvmTypeForTypeId(context, result_type);
  if (!arr_type_result) return std::unexpected(arr_type_result.error());
  llvm::Type* arr_type = *arr_type_result;
  llvm::Type* elem_type = arr_type->getArrayElementType();

  llvm::Value* aggregate = llvm::UndefValue::get(arr_type);
  for (size_t i = 0; i < rvalue.operands.size(); ++i) {
    auto elem_result =
        LowerOperandAsStorage(context, rvalue.operands[i], elem_type);
    if (!elem_result) return std::unexpected(elem_result.error());
    llvm::Value* elem = *elem_result;
    aggregate =
        builder.CreateInsertValue(aggregate, elem, {static_cast<unsigned>(i)});
  }
  return aggregate;
}

auto LowerUnpackedStructAggregateValue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type,
    const Type& target_type) -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();
  const auto& struct_info = target_type.AsUnpackedStruct();

  if (rvalue.operands.size() != struct_info.fields.size()) {
    throw common::InternalError(
        "LowerUnpackedStructAggregateValue",
        std::format(
            "struct aggregate operand count {} != field count {}",
            rvalue.operands.size(), struct_info.fields.size()));
  }

  auto struct_type_result = BuildLlvmTypeForTypeId(context, result_type);
  if (!struct_type_result) return std::unexpected(struct_type_result.error());
  llvm::Type* struct_type = *struct_type_result;

  llvm::Value* aggregate = llvm::UndefValue::get(struct_type);
  for (size_t i = 0; i < rvalue.operands.size(); ++i) {
    llvm::Type* field_type = llvm::cast<llvm::StructType>(struct_type)
                                 ->getElementType(static_cast<unsigned>(i));
    auto field_val_result =
        LowerOperandAsStorage(context, rvalue.operands[i], field_type);
    if (!field_val_result) return std::unexpected(field_val_result.error());
    llvm::Value* field_val = *field_val_result;
    aggregate = builder.CreateInsertValue(
        aggregate, field_val, {static_cast<unsigned>(i)});
  }
  return aggregate;
}

auto LowerQueueAggregateValue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& target_type = types[result_type];

  auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());

  size_t n = rvalue.operands.size();
  uint32_t max_bound = target_type.AsQueue().max_bound;
  if (max_bound > 0 && n > static_cast<size_t>(max_bound) + 1) {
    n = static_cast<size_t>(max_bound) + 1;
  }

  TypeId elem_type_id = target_type.AsQueue().element_type;
  auto elem_ops_result = context.GetElemOpsForType(elem_type_id);
  if (!elem_ops_result) return std::unexpected(elem_ops_result.error());
  auto elem_ops = *elem_ops_result;

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
    auto val_result = LowerOperandAsStorage(
        context, rvalue.operands[i], elem_ops.elem_llvm_type);
    if (!val_result) return std::unexpected(val_result.error());
    llvm::Value* val = *val_result;

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

  return handle;
}

}  // namespace

auto LowerAggregateRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type,
    const mir::AggregateRvalueInfo& /*info*/) -> Result<RvalueValue> {
  const auto& types = context.GetTypeArena();
  const Type& target_type = types[result_type];

  Result<llvm::Value*> result_or_err =
      std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          context.GetCurrentOrigin(),
          std::format(
              "aggregate for type {} not supported", ToString(target_type)),
          UnsupportedCategory::kType));

  switch (target_type.Kind()) {
    case TypeKind::kUnpackedArray:
      result_or_err =
          LowerUnpackedArrayAggregateValue(context, rvalue, result_type);
      break;
    case TypeKind::kUnpackedStruct:
      result_or_err = LowerUnpackedStructAggregateValue(
          context, rvalue, result_type, target_type);
      break;
    case TypeKind::kQueue:
      result_or_err = LowerQueueAggregateValue(context, rvalue, result_type);
      break;
    default:
      break;
  }

  if (!result_or_err) return std::unexpected(result_or_err.error());
  return RvalueValue::TwoState(*result_or_err);
}

}  // namespace lyra::lowering::mir_to_llvm
