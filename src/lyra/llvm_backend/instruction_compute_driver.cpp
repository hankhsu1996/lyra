#include "lyra/llvm_backend/instruction_compute_driver.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <variant>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/compute_result.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/four_state_utils.hpp"
#include "lyra/llvm_backend/instruction_compute_2state.hpp"
#include "lyra/llvm_backend/instruction_compute_4state.hpp"
#include "lyra/llvm_backend/instruction_compute_ops.hpp"
#include "lyra/llvm_backend/instruction_compute_rvalue.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto LowerBinaryRvalue(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerBinaryRvalue4State(context, info, operands, packed_context);
  }
  return LowerBinaryRvalue2State(context, info, operands, packed_context);
}

auto LowerUnaryRvalue(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerUnaryRvalue4State(context, info, operands, packed_context);
  }
  return LowerUnaryRvalue2State(context, info, operands, packed_context);
}

auto LowerConcatRvalue(
    Context& context, const mir::ConcatRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerConcatRvalue4State(context, info, operands, packed_context);
  }
  return LowerConcatRvalue2State(context, info, operands, packed_context);
}

auto LowerIndexValidity(
    Context& context, const mir::IndexValidityRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  // IndexValidity always returns 2-state (bool)
  return LowerIndexValidity2State(context, info, operands, packed_context);
}

auto LowerGuardedUse(
    Context& context, const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerGuardedUse4State(context, info, operands, packed_context);
  }
  return LowerGuardedUse2State(context, info, operands, packed_context);
}

auto LowerRuntimeQuery(
    Context& context, const mir::RuntimeQueryRvalueInfo& info,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerRuntimeQuery4State(context, info, packed_context);
  }
  return LowerRuntimeQuery2State(context, info, packed_context);
}

auto LowerUserCall(
    Context& context, const mir::UserCallRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerUserCall4State(context, info, operands, packed_context);
  }
  return LowerUserCall2State(context, info, operands, packed_context);
}

}  // namespace

auto ApplyWidthMaskToResult(
    Context& context, const ComputeResult& result, uint32_t semantic_width)
    -> ComputeResult {
  auto* masked_value = ApplyWidthMask(context, result.value, semantic_width);
  if (result.IsFourState()) {
    auto* masked_unknown =
        ApplyWidthMask(context, result.unknown, semantic_width);
    return ComputeResult::FourState(masked_value, masked_unknown);
  }
  return ComputeResult::TwoState(masked_value);
}

auto FinalizeCompute(
    Context& context, mir::PlaceId target, const ComputeResult& result,
    uint32_t semantic_width, llvm::StructType* struct_type) -> Result<void> {
  auto& builder = context.GetBuilder();

  // Apply width mask
  auto masked = ApplyWidthMaskToResult(context, result, semantic_width);

  // Get target pointer
  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());

  // Pack if 4-state target
  if (struct_type != nullptr) {
    auto* packed =
        PackFourState(builder, struct_type, masked.value, masked.unknown);
    builder.CreateStore(packed, *target_ptr_or_err);
  } else {
    builder.CreateStore(masked.value, *target_ptr_or_err);
  }
  return {};
}

auto LowerPackedCoreRvalue(Context& context, const mir::Compute& compute)
    -> Result<void> {
  // Validate and get type info
  auto type_info_or_err = ValidateAndGetTypeInfo(context, compute.target);
  if (!type_info_or_err) return std::unexpected(type_info_or_err.error());
  PlaceTypeInfo type_info = *type_info_or_err;

  // Get storage type and element type
  auto storage_type_or_err = context.GetPlaceLlvmType(compute.target);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  llvm::StructType* struct_type = nullptr;
  llvm::Type* elem_type = storage_type;
  if (type_info.is_four_state) {
    struct_type = llvm::cast<llvm::StructType>(storage_type);
    elem_type = struct_type->getElementType(0);
  }

  // Create packed compute context
  PackedComputeContext packed_context{
      .storage_type = storage_type,
      .element_type = elem_type,
      .bit_width = type_info.bit_width,
      .is_four_state = type_info.is_four_state,
  };

  // Dispatch to rvalue-specific handler (wrappers handle 2s/4s dispatch)
  auto result_or_err = std::visit(
      common::Overloaded{
          [&](const mir::BinaryRvalueInfo& info) -> Result<ComputeResult> {
            return LowerBinaryRvalue(
                context, info, compute.value.operands, packed_context);
          },
          [&](const mir::UnaryRvalueInfo& info) -> Result<ComputeResult> {
            return LowerUnaryRvalue(
                context, info, compute.value.operands, packed_context);
          },
          [&](const mir::ConcatRvalueInfo& info) -> Result<ComputeResult> {
            return LowerConcatRvalue(
                context, info, compute.value.operands, packed_context);
          },
          [&](const mir::IndexValidityRvalueInfo& info)
              -> Result<ComputeResult> {
            return LowerIndexValidity(
                context, info, compute.value.operands, packed_context);
          },
          [&](const mir::GuardedUseRvalueInfo& info) -> Result<ComputeResult> {
            return LowerGuardedUse(
                context, info, compute.value.operands, packed_context);
          },
          [&](const mir::RuntimeQueryRvalueInfo& info)
              -> Result<ComputeResult> {
            return LowerRuntimeQuery(context, info, packed_context);
          },
          [&](const mir::UserCallRvalueInfo& info) -> Result<ComputeResult> {
            return LowerUserCall(
                context, info, compute.value.operands, packed_context);
          },
          [&](const auto& /*info*/) -> Result<ComputeResult> {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    std::format(
                        "unsupported rvalue kind in packed core path: {}",
                        mir::GetRvalueKind(compute.value.info)),
                    UnsupportedCategory::kFeature));
          },
      },
      compute.value.info);

  if (!result_or_err) return std::unexpected(result_or_err.error());

  return FinalizeCompute(
      context, compute.target, *result_or_err, type_info.bit_width,
      struct_type);
}

}  // namespace lyra::lowering::mir_to_llvm
