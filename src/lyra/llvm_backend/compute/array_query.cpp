#include "lyra/llvm_backend/compute/array_query.hpp"

#include <cstdint>
#include <expected>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/array_query_kind.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
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
    ArrayQuerySysFnKind kind, int32_t left, int32_t right, bool is_four_state,
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
  auto* unk = is_four_state ? llvm::ConstantInt::get(i32_ty, 0) : nullptr;
  return {.value = val, .unknown = unk};
}

// Compute array query result for a variable-sized dimension (runtime IR).
// Dynamic arrays/queues: left=0, right=size-1, increment=-1.
// MIR hardcodes increment=-1 for variable-sized (interp_rvalue.cpp:1048).
auto ComputeRuntimeDim(
    llvm::IRBuilder<>& b, ArrayQuerySysFnKind kind, llvm::Value* size_i64,
    bool is_four_state, llvm::LLVMContext& llvm_ctx) -> DimResult {
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

  auto* unk = is_four_state ? llvm::ConstantInt::get(i32_ty, 0) : nullptr;
  return {.value = result, .unknown = unk};
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
    Context& context, const mir::Rvalue& rvalue,
    const mir::ArrayQueryRvalueInfo& info, TypeId result_type)
    -> Result<RvalueValue> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  bool is_four_state = false;
  const auto& res_type = types[result_type];
  if (res_type.Kind() == TypeKind::kIntegral ||
      res_type.Kind() == TypeKind::kPackedStruct ||
      res_type.Kind() == TypeKind::kEnum ||
      res_type.Kind() == TypeKind::kPackedArray) {
    is_four_state = IsPackedFourState(res_type, types);
  }

  // $dimensions / $unpacked_dimensions: always compile-time constants
  if (info.kind == ArrayQuerySysFnKind::kDimensions) {
    auto* val = llvm::ConstantInt::get(i32_ty, info.total_dims);
    if (is_four_state) {
      return RvalueValue::FourState(val, llvm::ConstantInt::get(i32_ty, 0));
    }
    return RvalueValue::TwoState(val);
  }
  if (info.kind == ArrayQuerySysFnKind::kUnpackedDimensions) {
    auto* val = llvm::ConstantInt::get(i32_ty, info.unpacked_dims);
    if (is_four_state) {
      return RvalueValue::FourState(val, llvm::ConstantInt::get(i32_ty, 0));
    }
    return RvalueValue::TwoState(val);
  }

  // Lower dim operand (operands[1] is always present, default = 1)
  auto dim_raw_or_err = LowerOperandRaw(context, rvalue.operands[1]);
  if (!dim_raw_or_err) return std::unexpected(dim_raw_or_err.error());
  llvm::Value* dim_raw = *dim_raw_or_err;

  // Extract value plane and check for unknown bits (4-state dim operand)
  llvm::Value* dim_value = dim_raw;
  llvm::Value* dim_unknown = nullptr;

  if (llvm::isa<llvm::StructType>(dim_raw->getType())) {
    // 4-state: {value, unknown}
    dim_value = builder.CreateExtractValue(dim_raw, 0, "aq.dim.val");
    dim_unknown = builder.CreateExtractValue(dim_raw, 1, "aq.dim.unk");
  }

  // sext to i32: matches MIR sign-extension (interp_rvalue.cpp:994-1000)
  auto* dim_val_type = dim_value->getType();
  if (dim_val_type->getIntegerBitWidth() != 32) {
    dim_value = builder.CreateSExtOrTrunc(dim_value, i32_ty, "aq.dim.sext");
  }

  // If dim has unknown bits, branch to X result
  llvm::BasicBlock* x_unknown_bb = nullptr;
  llvm::BasicBlock* continue_bb = nullptr;
  llvm::BasicBlock* merge_bb = nullptr;

  if (dim_unknown != nullptr) {
    // Check if any unknown bit is set
    auto* unk_type = dim_unknown->getType();
    auto* zero_unk = llvm::ConstantInt::get(unk_type, 0);
    auto* has_unknown =
        builder.CreateICmpNE(dim_unknown, zero_unk, "aq.dim.hasunk");

    auto* func = builder.GetInsertBlock()->getParent();
    x_unknown_bb = llvm::BasicBlock::Create(llvm_ctx, "aq.dim.xunk", func);
    continue_bb = llvm::BasicBlock::Create(llvm_ctx, "aq.dim.known", func);
    merge_bb = llvm::BasicBlock::Create(llvm_ctx, "aq.merge", func);

    builder.CreateCondBr(has_unknown, x_unknown_bb, continue_bb);

    // X result for unknown dim (values used by PHI at merge)
    builder.SetInsertPoint(x_unknown_bb);
    builder.CreateBr(merge_bb);

    builder.SetInsertPoint(continue_bb);
  }

  // Now compute the result for known dim
  llvm::Value* result_val = nullptr;
  llvm::Value* result_unk = nullptr;

  // Check for constant dim (fast path)
  if (auto* const_dim = llvm::dyn_cast<llvm::ConstantInt>(dim_value)) {
    auto dim = static_cast<int32_t>(const_dim->getSExtValue());

    if (dim < 1 || dim > static_cast<int32_t>(info.total_dims)) {
      auto x = MakeXResult(is_four_state, llvm_ctx);
      result_val = x.value;
      result_unk = x.unknown;
    } else {
      const auto& dim_info = info.dims.at(static_cast<size_t>(dim - 1));

      if (dim > 1 && dim_info.is_variable_sized) {
        // IEEE 20.7.1: dim > 1 on variable-sized returns X
        auto x = MakeXResult(is_four_state, llvm_ctx);
        result_val = x.value;
        result_unk = x.unknown;
      } else if (dim_info.is_variable_sized) {
        // dim == 1, variable-sized: get runtime size
        auto handle_or_err = LowerOperand(context, rvalue.operands[0]);
        if (!handle_or_err) return std::unexpected(handle_or_err.error());
        auto* size = builder.CreateCall(
            context.GetLyraDynArraySize(), {*handle_or_err}, "aq.da.size");
        auto dr = ComputeRuntimeDim(
            builder, info.kind, size, is_four_state, llvm_ctx);
        result_val = dr.value;
        result_unk = dr.unknown;
      } else {
        auto dr = ComputeFixedDim(
            info.kind, dim_info.left, dim_info.right, is_four_state, llvm_ctx);
        result_val = dr.value;
        result_unk = dr.unknown;
      }
    }
  } else {
    // Runtime dim: build select chain
    // Start with X default (out-of-range)
    auto x_default = MakeXResult(is_four_state, llvm_ctx);
    result_val = x_default.value;
    result_unk = x_default.unknown;

    for (int32_t d = info.total_dims; d >= 1; --d) {
      const auto& dim_info = info.dims.at(static_cast<size_t>(d - 1));

      DimResult dr = {};
      if (dim_info.is_variable_sized && d > 1) {
        // IEEE 20.7.1: dim > 1 on variable-sized returns X
        dr = MakeXResult(is_four_state, llvm_ctx);
      } else if (dim_info.is_variable_sized) {
        // Only dim 1 can be variable-sized and queried at runtime.
        // operands[0] is a dynamic-array/queue handle only in this case.
        auto handle_or_err = LowerOperand(context, rvalue.operands[0]);
        if (!handle_or_err) return std::unexpected(handle_or_err.error());
        auto* runtime_size = builder.CreateCall(
            context.GetLyraDynArraySize(), {*handle_or_err}, "aq.da.size");
        dr = ComputeRuntimeDim(
            builder, info.kind, runtime_size, is_four_state, llvm_ctx);
      } else {
        dr = ComputeFixedDim(
            info.kind, dim_info.left, dim_info.right, is_four_state, llvm_ctx);
      }

      auto* cmp = builder.CreateICmpEQ(
          dim_value, llvm::ConstantInt::get(i32_ty, d), "aq.dim.eq");
      result_val =
          builder.CreateSelect(cmp, dr.value, result_val, "aq.sel.val");
      if (is_four_state) {
        result_unk =
            builder.CreateSelect(cmp, dr.unknown, result_unk, "aq.sel.unk");
      }
    }
  }

  // Merge with unknown-dim X path if needed
  if (merge_bb != nullptr) {
    auto* known_bb = builder.GetInsertBlock();
    builder.CreateBr(merge_bb);

    builder.SetInsertPoint(merge_bb);
    auto x_result = MakeXResult(is_four_state, llvm_ctx);

    auto* phi_val = builder.CreatePHI(i32_ty, 2, "aq.phi.val");
    phi_val->addIncoming(x_result.value, x_unknown_bb);
    phi_val->addIncoming(result_val, known_bb);
    result_val = phi_val;

    if (is_four_state) {
      auto* phi_unk = builder.CreatePHI(i32_ty, 2, "aq.phi.unk");
      phi_unk->addIncoming(x_result.unknown, x_unknown_bb);
      phi_unk->addIncoming(result_unk, known_bb);
      result_unk = phi_unk;
    }
  }

  if (is_four_state) {
    return RvalueValue::FourState(result_val, result_unk);
  }
  return RvalueValue::TwoState(result_val);
}

}  // namespace lyra::lowering::mir_to_llvm
