#include "lyra/llvm_backend/compute/array_query.hpp"

#include <cstdint>
#include <expected>
#include <utility>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/array_query_kind.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

struct DimResult {
  llvm::Value* value;
  llvm::Value* unknown;  // nullptr if 2-state
};

// Compute array query result for a fixed-size dimension (all compile-time).
// LRM 20.7 semantics:
//   increment = 1 if left >= right, -1 if left < right
//   low  = min(left, right)
//   high = max(left, right)
//   size = high - low + 1
auto ComputeFixedDim(
    ArrayQuerySysFnKind kind, int32_t left, int32_t right,
    llvm::LLVMContext& llvm_ctx) -> DimResult {
  int32_t increment = (left >= right) ? 1 : -1;
  int32_t low = (increment == -1) ? left : right;
  int32_t high = (increment == -1) ? right : left;
  int32_t size = high - low + 1;

  int32_t result = 0;
  switch (kind) {
    case ArrayQuerySysFnKind::kLeft:
      result = left;
      break;
    case ArrayQuerySysFnKind::kRight:
      result = right;
      break;
    case ArrayQuerySysFnKind::kLow:
      result = low;
      break;
    case ArrayQuerySysFnKind::kHigh:
      result = high;
      break;
    case ArrayQuerySysFnKind::kIncrement:
      result = increment;
      break;
    case ArrayQuerySysFnKind::kSize:
      result = size;
      break;
    case ArrayQuerySysFnKind::kDimensions:
    case ArrayQuerySysFnKind::kUnpackedDimensions:
      break;
  }

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* val =
      llvm::ConstantInt::get(i32_ty, static_cast<uint64_t>(result), true);
  return {.value = val, .unknown = nullptr};
}

// Compute array query result for a variable-sized dimension (runtime IR).
// Dynamic arrays/queues: left=0, right=size-1, increment=-1.
// MIR hardcodes increment=-1 for variable-sized (interp_rvalue.cpp:1048).
auto ComputeRuntimeDim(
    llvm::IRBuilder<>& b, ArrayQuerySysFnKind kind, llvm::Value* size_i64,
    llvm::LLVMContext& llvm_ctx) -> DimResult {
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* size_i32 = b.CreateTrunc(size_i64, i32_ty, "aq.size.trunc");
  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  auto* one = llvm::ConstantInt::get(i32_ty, 1);
  auto* neg_one = llvm::ConstantInt::getSigned(i32_ty, -1);

  // right = size - 1 (modular: empty container wraps to -1)
  auto* right = b.CreateSub(size_i32, one, "aq.right");

  llvm::Value* result = nullptr;
  switch (kind) {
    case ArrayQuerySysFnKind::kLeft:
      result = zero;
      break;
    case ArrayQuerySysFnKind::kRight:
      result = right;
      break;
    case ArrayQuerySysFnKind::kLow:
      // increment=-1 => low = left = 0
      result = zero;
      break;
    case ArrayQuerySysFnKind::kHigh:
      // increment=-1 => high = right = size-1
      result = right;
      break;
    case ArrayQuerySysFnKind::kIncrement:
      result = neg_one;
      break;
    case ArrayQuerySysFnKind::kSize:
      result = size_i32;
      break;
    case ArrayQuerySysFnKind::kDimensions:
    case ArrayQuerySysFnKind::kUnpackedDimensions:
      result = zero;
      break;
  }

  return {.value = result, .unknown = nullptr};
}

auto MakeXResult(bool is_four_state, llvm::LLVMContext& llvm_ctx) -> DimResult {
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  if (is_four_state) {
    auto* all_ones = llvm::ConstantInt::getSigned(i32_ty, -1);
    return {.value = zero, .unknown = all_ones};
  }
  // 2-state: X coerces to 0 (matches MIR behavior)
  return {.value = zero, .unknown = nullptr};
}

}  // namespace

auto LowerArrayQueryRvalue(
    Context& context, const CuFacts& facts, const mir::Rvalue& rvalue,
    const mir::ArrayQueryRvalueInfo& info, TypeId result_type)
    -> Result<RvalueValue> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerArrayQueryRvalue(
      context, facts, canonical, rvalue, info, result_type);
}

auto LowerArrayQueryRvalue(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::Rvalue& rvalue, const mir::ArrayQueryRvalueInfo& info,
    TypeId result_type) -> Result<RvalueValue> {
  auto& builder = context.GetBuilder();
  const auto& types = *facts.types;
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  bool is_four_state = false;
  const auto& res_type = types[result_type];
  if (res_type.Kind() == TypeKind::kIntegral ||
      res_type.Kind() == TypeKind::kPackedStruct ||
      res_type.Kind() == TypeKind::kEnum ||
      res_type.Kind() == TypeKind::kPackedArray) {
    is_four_state = IsPackedFourState(facts, res_type);
  }

  if (info.kind == ArrayQuerySysFnKind::kDimensions) {
    auto* val = llvm::ConstantInt::get(i32_ty, info.total_dims);
    return RvalueValue::TwoState(val);
  }
  if (info.kind == ArrayQuerySysFnKind::kUnpackedDimensions) {
    auto* val = llvm::ConstantInt::get(i32_ty, info.unpacked_dims);
    return RvalueValue::TwoState(val);
  }

  auto dim_raw_or_err =
      LowerOperandRaw(context, facts, resolver, rvalue.operands[1]);
  if (!dim_raw_or_err) return std::unexpected(dim_raw_or_err.error());
  llvm::Value* dim_raw = *dim_raw_or_err;

  llvm::Value* dim_value = dim_raw;
  llvm::Value* dim_unknown = nullptr;

  if (llvm::isa<llvm::StructType>(dim_raw->getType())) {
    dim_value = builder.CreateExtractValue(dim_raw, 0, "aq.dim.val");
    dim_unknown = builder.CreateExtractValue(dim_raw, 1, "aq.dim.unk");
  }

  auto* dim_val_type = dim_value->getType();
  if (dim_val_type->getIntegerBitWidth() != 32) {
    dim_value = builder.CreateSExtOrTrunc(dim_value, i32_ty, "aq.dim.sext");
  }

  llvm::BasicBlock* x_unknown_bb = nullptr;
  llvm::BasicBlock* continue_bb = nullptr;
  llvm::BasicBlock* merge_bb = nullptr;

  if (dim_unknown != nullptr) {
    auto* unk_type = dim_unknown->getType();
    auto* zero_unk = llvm::ConstantInt::get(unk_type, 0);
    auto* has_unknown =
        builder.CreateICmpNE(dim_unknown, zero_unk, "aq.dim.hasunk");

    auto* func = builder.GetInsertBlock()->getParent();
    x_unknown_bb = llvm::BasicBlock::Create(llvm_ctx, "aq.dim.xunk", func);
    continue_bb = llvm::BasicBlock::Create(llvm_ctx, "aq.dim.known", func);
    merge_bb = llvm::BasicBlock::Create(llvm_ctx, "aq.merge", func);

    builder.CreateCondBr(has_unknown, x_unknown_bb, continue_bb);

    builder.SetInsertPoint(x_unknown_bb);
    builder.CreateBr(merge_bb);

    builder.SetInsertPoint(continue_bb);
  }

  llvm::Value* result_val = nullptr;
  llvm::Value* result_unk = nullptr;

  if (auto* const_dim = llvm::dyn_cast<llvm::ConstantInt>(dim_value)) {
    auto dim = static_cast<int32_t>(const_dim->getSExtValue());

    if (dim < 1 || std::cmp_greater(dim, info.total_dims)) {
      auto x = MakeXResult(is_four_state, llvm_ctx);
      result_val = x.value;
      result_unk = x.unknown;
    } else {
      const auto& dim_info = info.dims.at(static_cast<size_t>(dim - 1));

      if (dim > 1 && dim_info.is_variable_sized) {
        auto x = MakeXResult(is_four_state, llvm_ctx);
        result_val = x.value;
        result_unk = x.unknown;
      } else if (dim_info.is_variable_sized) {
        auto handle_or_err =
            LowerOperand(context, facts, resolver, rvalue.operands[0]);
        if (!handle_or_err) return std::unexpected(handle_or_err.error());
        auto* size = builder.CreateCall(
            context.GetLyraDynArraySize(), {*handle_or_err}, "aq.da.size");
        auto dr = ComputeRuntimeDim(builder, info.kind, size, llvm_ctx);
        result_val = dr.value;
        result_unk = dr.unknown;
      } else {
        auto dr =
            ComputeFixedDim(info.kind, dim_info.left, dim_info.right, llvm_ctx);
        result_val = dr.value;
        result_unk = dr.unknown;
      }
    }
  } else {
    auto x_default = MakeXResult(is_four_state, llvm_ctx);
    result_val = x_default.value;
    result_unk = x_default.unknown;

    for (int32_t d = info.total_dims; d >= 1; --d) {
      const auto& dim_info = info.dims.at(static_cast<size_t>(d - 1));

      DimResult dr = {};
      if (dim_info.is_variable_sized && d > 1) {
        dr = MakeXResult(is_four_state, llvm_ctx);
      } else if (dim_info.is_variable_sized) {
        auto handle_or_err =
            LowerOperand(context, facts, resolver, rvalue.operands[0]);
        if (!handle_or_err) return std::unexpected(handle_or_err.error());
        auto* runtime_size = builder.CreateCall(
            context.GetLyraDynArraySize(), {*handle_or_err}, "aq.da.size");
        dr = ComputeRuntimeDim(builder, info.kind, runtime_size, llvm_ctx);
      } else {
        dr =
            ComputeFixedDim(info.kind, dim_info.left, dim_info.right, llvm_ctx);
      }

      auto* cmp = builder.CreateICmpEQ(
          dim_value, llvm::ConstantInt::get(i32_ty, d), "aq.dim.eq");
      result_val =
          builder.CreateSelect(cmp, dr.value, result_val, "aq.sel.val");
      if (result_unk != nullptr) {
        // Local representation repair: coerce nullptr dr.unknown to zero
        // for LLVM select type compatibility. This does NOT promote the
        // result to kFourState semantically.
        llvm::Value* dr_unk = dr.unknown;
        if (dr_unk == nullptr) {
          dr_unk = llvm::ConstantInt::get(i32_ty, 0);
        }
        result_unk =
            builder.CreateSelect(cmp, dr_unk, result_unk, "aq.sel.unk");
      }
    }
  }

  if (merge_bb != nullptr) {
    auto* known_bb = builder.GetInsertBlock();
    builder.CreateBr(merge_bb);

    builder.SetInsertPoint(merge_bb);
    auto x_result = MakeXResult(is_four_state, llvm_ctx);

    auto* phi_val = builder.CreatePHI(i32_ty, 2, "aq.phi.val");
    phi_val->addIncoming(x_result.value, x_unknown_bb);
    phi_val->addIncoming(result_val, known_bb);
    result_val = phi_val;

    if (result_unk != nullptr) {
      auto* phi_unk = builder.CreatePHI(i32_ty, 2, "aq.phi.unk");
      phi_unk->addIncoming(x_result.unknown, x_unknown_bb);
      phi_unk->addIncoming(result_unk, known_bb);
      result_unk = phi_unk;
    }
  }

  if (result_unk != nullptr) {
    return RvalueValue::FourState(result_val, result_unk);
  }
  return RvalueValue::TwoState(result_val);
}

}  // namespace lyra::lowering::mir_to_llvm
