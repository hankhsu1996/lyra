#include "lyra/llvm_backend/compute/string.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <variant>
#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/compute/ops.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/format_lowering.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Create an empty string handle via LyraStringFromLiteral("", 0).
// Returns a newly-owned handle (refcount=1).
auto CreateEmptyString(Context& context) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());
  auto* empty_data = builder.CreateGlobalStringPtr("");
  auto* empty_len = llvm::ConstantInt::get(i64_ty, 0);
  return builder.CreateCall(
      context.GetLyraStringFromLiteral(), {empty_data, empty_len}, "str.empty");
}

// Value kind enum matching RuntimeFormatValueKind in runtime
enum class RuntimeFormatValueKind : int32_t {
  kIntegral = 0,
  kReal = 1,
  kString = 2,
};

}  // namespace

auto LowerStringBinaryOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* result_type)
    -> Result<llvm::Value*> {
  CanonicalSlotAccess canonical(context);
  return LowerStringBinaryOp(context, canonical, info, operands, result_type);
}

auto LowerStringConcatValue(
    Context& context, const mir::ConcatRvalueInfo& info,
    const std::vector<mir::Operand>& operands) -> Result<llvm::Value*> {
  CanonicalSlotAccess canonical(context);
  return LowerStringConcatValue(context, canonical, info, operands);
}

auto LowerStringReplicateValue(
    Context& context, const mir::ReplicateRvalueInfo& info,
    const std::vector<mir::Operand>& operands) -> Result<llvm::Value*> {
  CanonicalSlotAccess canonical(context);
  return LowerStringReplicateValue(context, canonical, info, operands);
}

auto LowerSFormatRvalueValue(
    Context& context, const CuFacts& facts, const mir::SFormatRvalueInfo& info,
    const std::vector<mir::Operand>& operands) -> Result<llvm::Value*> {
  CanonicalSlotAccess canonical(context);
  return LowerSFormatRvalueValue(context, facts, canonical, info, operands);
}

auto LowerStringBinaryOp(
    Context& context, SlotAccessResolver& resolver,
    const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* result_type)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();

  if (!IsComparisonOp(info.op)) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        std::format(
            "string operation not supported (only comparisons): {}",
            mir::ToString(info.op)),
        UnsupportedCategory::kOperation));
  }

  auto lhs_or_err = LowerOperand(context, resolver, operands[0]);
  if (!lhs_or_err) return std::unexpected(lhs_or_err.error());
  llvm::Value* lhs = *lhs_or_err;
  auto rhs_or_err = LowerOperand(context, resolver, operands[1]);
  if (!rhs_or_err) return std::unexpected(rhs_or_err.error());
  llvm::Value* rhs = *rhs_or_err;

  if (std::holds_alternative<Constant>(operands[0].payload)) {
    context.RegisterOwnedTemp(lhs);
  }
  if (std::holds_alternative<Constant>(operands[1].payload)) {
    context.RegisterOwnedTemp(rhs);
  }

  llvm::Value* cmp_result =
      builder.CreateCall(context.GetLyraStringCmp(), {lhs, rhs}, "strcmp");

  auto* zero = llvm::ConstantInt::get(cmp_result->getType(), 0);
  llvm::Value* bool_result = nullptr;

  switch (info.op) {
    case mir::BinaryOp::kEqual:
      bool_result = builder.CreateICmpEQ(cmp_result, zero, "str.eq");
      break;
    case mir::BinaryOp::kNotEqual:
      bool_result = builder.CreateICmpNE(cmp_result, zero, "str.ne");
      break;
    case mir::BinaryOp::kLessThan:
    case mir::BinaryOp::kLessThanSigned:
      bool_result = builder.CreateICmpSLT(cmp_result, zero, "str.lt");
      break;
    case mir::BinaryOp::kLessThanEqual:
    case mir::BinaryOp::kLessThanEqualSigned:
      bool_result = builder.CreateICmpSLE(cmp_result, zero, "str.le");
      break;
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kGreaterThanSigned:
      bool_result = builder.CreateICmpSGT(cmp_result, zero, "str.gt");
      break;
    case mir::BinaryOp::kGreaterThanEqual:
    case mir::BinaryOp::kGreaterThanEqualSigned:
      bool_result = builder.CreateICmpSGE(cmp_result, zero, "str.ge");
      break;
    default:
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          context.GetCurrentOrigin(),
          std::format(
              "unsupported string comparison: {}", mir::ToString(info.op)),
          UnsupportedCategory::kOperation));
  }

  return builder.CreateZExt(bool_result, result_type, "str.cmp.ext");
}

auto LowerStringConcatValue(
    Context& context, SlotAccessResolver& resolver,
    const mir::ConcatRvalueInfo& /*info*/,
    const std::vector<mir::Operand>& operands) -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

  auto count = static_cast<int64_t>(operands.size());

  std::vector<llvm::Value*> handles;
  handles.reserve(operands.size());
  for (const auto& operand : operands) {
    auto handle_or_err = LowerOperand(context, resolver, operand);
    if (!handle_or_err) return std::unexpected(handle_or_err.error());
    llvm::Value* handle = *handle_or_err;
    handles.push_back(handle);

    if (std::holds_alternative<Constant>(operand.payload)) {
      context.RegisterOwnedTemp(handle);
    }
  }

  auto* array_alloca =
      builder.CreateAlloca(ptr_ty, llvm::ConstantInt::get(i64_ty, count));
  for (size_t i = 0; i < handles.size(); ++i) {
    auto* slot = builder.CreateGEP(
        ptr_ty, array_alloca, {llvm::ConstantInt::get(i64_ty, i)});
    builder.CreateStore(handles[i], slot);
  }

  llvm::Value* result = builder.CreateCall(
      context.GetLyraStringConcat(),
      {array_alloca, llvm::ConstantInt::get(i64_ty, count)}, "str.concat");

  return result;
}

auto LowerStringReplicateValue(
    Context& context, SlotAccessResolver& resolver,
    const mir::ReplicateRvalueInfo& info,
    const std::vector<mir::Operand>& operands) -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

  auto count = static_cast<int64_t>(info.count);

  auto handle_or_err = LowerOperand(context, resolver, operands[0]);
  if (!handle_or_err) return std::unexpected(handle_or_err.error());
  llvm::Value* handle = *handle_or_err;

  if (std::holds_alternative<Constant>(operands[0].payload)) {
    context.RegisterOwnedTemp(handle);
  }

  auto* array_alloca =
      builder.CreateAlloca(ptr_ty, llvm::ConstantInt::get(i64_ty, count));
  for (int64_t i = 0; i < count; ++i) {
    auto* slot = builder.CreateGEP(
        ptr_ty, array_alloca, {llvm::ConstantInt::get(i64_ty, i)});
    builder.CreateStore(handle, slot);
  }

  llvm::Value* result = builder.CreateCall(
      context.GetLyraStringConcat(),
      {array_alloca, llvm::ConstantInt::get(i64_ty, count)}, "str.repeat");

  return result;
}

auto LowerSFormatRvalueValue(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::SFormatRvalueInfo& info,
    const std::vector<mir::Operand>& operands) -> Result<llvm::Value*> {
  if (info.has_runtime_format) {
    auto& builder = context.GetBuilder();
    auto& llvm_ctx = context.GetLlvmContext();
    const auto& types = *facts.types;

    auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
    auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
    auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
    auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

    auto fmt_or_err = LowerOperand(context, resolver, operands[0]);
    if (!fmt_or_err) return std::unexpected(fmt_or_err.error());
    llvm::Value* fmt_handle = *fmt_or_err;

    auto arg_count = static_cast<int64_t>(operands.size() - 1);
    auto* count_val = llvm::ConstantInt::get(i64_ty, arg_count);

    auto* data_array =
        builder.CreateAlloca(ptr_ty, llvm::ConstantInt::get(i64_ty, arg_count));
    auto* width_array =
        builder.CreateAlloca(i32_ty, llvm::ConstantInt::get(i64_ty, arg_count));
    auto* signed_array =
        builder.CreateAlloca(i8_ty, llvm::ConstantInt::get(i64_ty, arg_count));
    auto* kind_array =
        builder.CreateAlloca(i32_ty, llvm::ConstantInt::get(i64_ty, arg_count));

    for (size_t i = 1; i < operands.size(); ++i) {
      size_t idx = i - 1;
      auto* idx_val = llvm::ConstantInt::get(i64_ty, idx);

      TypeId type_id = GetOperandTypeId(facts, context, operands[i]);
      const Type& ty = types[type_id];

      RuntimeFormatValueKind kind = RuntimeFormatValueKind::kIntegral;
      int32_t width = 32;
      bool is_signed = false;

      if (ty.Kind() == TypeKind::kString) {
        kind = RuntimeFormatValueKind::kString;
        width = 0;
      } else if (ty.Kind() == TypeKind::kReal) {
        kind = RuntimeFormatValueKind::kReal;
        width = 64;
      } else if (ty.Kind() == TypeKind::kShortReal) {
        kind = RuntimeFormatValueKind::kReal;
        width = 32;
      } else if (ty.Kind() == TypeKind::kIntegral) {
        kind = RuntimeFormatValueKind::kIntegral;
        width = static_cast<int32_t>(ty.AsIntegral().bit_width);
        is_signed = ty.AsIntegral().is_signed;
      } else if (IsPacked(ty)) {
        kind = RuntimeFormatValueKind::kIntegral;
        width = static_cast<int32_t>(PackedBitWidth(ty, types));
        is_signed = IsPackedSigned(ty, types);
      }

      auto* width_slot = builder.CreateGEP(i32_ty, width_array, {idx_val});
      builder.CreateStore(llvm::ConstantInt::get(i32_ty, width), width_slot);

      auto* signed_slot = builder.CreateGEP(i8_ty, signed_array, {idx_val});
      builder.CreateStore(
          llvm::ConstantInt::get(i8_ty, is_signed ? 1 : 0), signed_slot);

      auto* kind_slot = builder.CreateGEP(i32_ty, kind_array, {idx_val});
      builder.CreateStore(
          llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(kind)),
          kind_slot);

      auto* data_slot = builder.CreateGEP(ptr_ty, data_array, {idx_val});
      if (kind == RuntimeFormatValueKind::kString) {
        auto val_or_err = LowerOperand(context, resolver, operands[i]);
        if (!val_or_err) return std::unexpected(val_or_err.error());
        builder.CreateStore(*val_or_err, data_slot);
      } else {
        auto val_or_err = LowerOperand(context, resolver, operands[i]);
        if (!val_or_err) return std::unexpected(val_or_err.error());
        llvm::Value* val = *val_or_err;

        llvm::Type* storage_ty = nullptr;
        if (kind == RuntimeFormatValueKind::kReal) {
          storage_ty = val->getType();
        } else {
          if (width <= 8) {
            storage_ty = llvm::Type::getInt8Ty(llvm_ctx);
          } else if (width <= 16) {
            storage_ty = llvm::Type::getInt16Ty(llvm_ctx);
          } else if (width <= 32) {
            storage_ty = llvm::Type::getInt32Ty(llvm_ctx);
          } else {
            storage_ty = llvm::Type::getInt64Ty(llvm_ctx);
          }
        }

        auto* alloca = builder.CreateAlloca(storage_ty);
        if (kind == RuntimeFormatValueKind::kIntegral &&
            val->getType() != storage_ty) {
          if (val->getType()->getIntegerBitWidth() >
              storage_ty->getIntegerBitWidth()) {
            val = builder.CreateTrunc(val, storage_ty);
          } else if (is_signed) {
            val = builder.CreateSExt(val, storage_ty);
          } else {
            val = builder.CreateZExt(val, storage_ty);
          }
        }
        builder.CreateStore(val, alloca);
        builder.CreateStore(alloca, data_slot);
      }
    }

    llvm::Value* result = builder.CreateCall(
        context.GetLyraStringFormatRuntime(),
        {fmt_handle, data_array, width_array, signed_array, kind_array,
         count_val},
        "sformat.runtime");

    return result;
  }

  if (info.ops.empty()) {
    if (operands.empty()) {
      return CreateEmptyString(context);
    }
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "$swrite/$swriteh/$swriteb/$swriteo auto-format with values not "
        "supported in LLVM backend",
        UnsupportedCategory::kFeature));
  }

  auto& builder = context.GetBuilder();

  auto validate_result = ValidateFormatOps(context, info.ops);
  if (!validate_result) return std::unexpected(validate_result.error());

  llvm::Value* buf =
      builder.CreateCall(context.GetLyraStringFormatStart(), {}, "sformat.buf");

  for (const auto& op : info.ops) {
    auto result = LowerFormatOpToBuffer(context, buf, op);
    if (!result) return std::unexpected(result.error());
  }

  llvm::Value* result_handle = builder.CreateCall(
      context.GetLyraStringFormatFinish(), {buf}, "sformat.result");

  return result_handle;
}

}  // namespace lyra::lowering::mir_to_llvm
