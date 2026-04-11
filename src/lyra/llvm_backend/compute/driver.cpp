#include "lyra/llvm_backend/compute/driver.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <variant>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/compute/four_state.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/ops.hpp"
#include "lyra/llvm_backend/compute/result.hpp"
#include "lyra/llvm_backend/compute/rvalue.hpp"
#include "lyra/llvm_backend/compute/two_state.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto LowerBinaryRvalue(
    Context& context, SlotAccessResolver& resolver,
    const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerBinaryRvalue4State(
        context, resolver, info, operands, packed_context);
  }
  return LowerBinaryRvalue2State(
      context, resolver, info, operands, packed_context);
}

auto LowerUnaryRvalue(
    Context& context, SlotAccessResolver& resolver,
    const mir::UnaryRvalueInfo& info, const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerUnaryRvalue4State(
        context, resolver, info, operands, packed_context);
  }
  return LowerUnaryRvalue2State(
      context, resolver, info, operands, packed_context);
}

auto LowerConcatRvalue(
    Context& context, SlotAccessResolver& resolver,
    const mir::ConcatRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerConcatRvalue4State(
        context, resolver, info, operands, packed_context);
  }
  return LowerConcatRvalue2State(
      context, resolver, info, operands, packed_context);
}

auto LowerReplicateRvalue(
    Context& context, SlotAccessResolver& resolver,
    const mir::ReplicateRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerReplicateRvalue4State(
        context, resolver, info, operands, packed_context);
  }
  return LowerReplicateRvalue2State(
      context, resolver, info, operands, packed_context);
}

auto LowerIsKnown(
    Context& context, SlotAccessResolver& resolver,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  return LowerIsKnown2State(context, resolver, operands, packed_context);
}

auto LowerIndexInRange(
    Context& context, SlotAccessResolver& resolver,
    const mir::IndexInRangeRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  return LowerIndexInRange2State(
      context, resolver, info, operands, packed_context);
}

auto LowerGuardedUse(
    Context& context, SlotAccessResolver& resolver,
    const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerGuardedUse4State(
        context, resolver, info, operands, packed_context);
  }
  return LowerGuardedUse2State(
      context, resolver, info, operands, packed_context);
}

// RuntimeQuery does not read module-slot operands (queries runtime state).
auto LowerRuntimeQuery(
    Context& context, const mir::RuntimeQueryRvalueInfo& info,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  if (packed_context.is_four_state) {
    return LowerRuntimeQuery4State(context, info, packed_context);
  }
  return LowerRuntimeQuery2State(context, info, packed_context);
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
    Context& context, const ComputeResult& result, uint32_t semantic_width,
    llvm::StructType* struct_type) -> llvm::Value* {
  auto& builder = context.GetBuilder();

  // Apply width mask
  auto masked = ApplyWidthMaskToResult(context, result, semantic_width);

  // Pack if 4-state target
  if (struct_type != nullptr) {
    return PackFourState(builder, struct_type, masked.value, masked.unknown);
  }
  return masked.value;
}

auto LowerPackedCoreRvalue(
    Context& context, const CuFacts& facts, const mir::Rvalue& rvalue,
    TypeId result_type) -> Result<RvalueValue> {
  CanonicalSlotAccess canonical(context);
  return LowerPackedCoreRvalue(context, facts, canonical, rvalue, result_type);
}
auto LowerPackedCoreRvalue(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::Rvalue& rvalue, TypeId result_type) -> Result<RvalueValue> {
  auto type_info_or_err = GetTypeInfoFromType(facts, context, result_type);
  if (!type_info_or_err) return std::unexpected(type_info_or_err.error());
  PlaceTypeInfo type_info = *type_info_or_err;

  auto storage_type_or_err = GetLlvmTypeForType(facts, context, result_type);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  llvm::Type* elem_type = storage_type;
  if (type_info.is_four_state) {
    auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
    elem_type = struct_type->getElementType(0);
  }

  // Operand-domain-aware override: when all operands are provably 2-state,
  // lower through the 2-state path even for 4-state target types. This
  // produces TwoState results that flow correctly through the temp contract.
  //
  // Excluded kinds:
  //   GuardedUse: reads info.place (not in rvalue.operands), which may be
  //     genuinely 4-state. AreAllOperandsTwoState only checks operands[0]
  //     (the validity flag), not the guarded place.
  //   RuntimeQuery: has no operands, already returns TwoState independently.
  if (type_info.is_four_state &&
      !std::holds_alternative<mir::GuardedUseRvalueInfo>(rvalue.info) &&
      !std::holds_alternative<mir::RuntimeQueryRvalueInfo>(rvalue.info) &&
      AreAllOperandsTwoState(facts, context, rvalue.operands)) {
    type_info.is_four_state = false;
    storage_type = elem_type;
  }

  PackedComputeContext packed_context{
      .facts = &facts,
      .storage_type = storage_type,
      .element_type = elem_type,
      .bit_width = type_info.bit_width,
      .is_four_state = type_info.is_four_state,
  };

  auto result_or_err = std::visit(
      common::Overloaded{
          [&](const mir::BinaryRvalueInfo& info) -> Result<ComputeResult> {
            return LowerBinaryRvalue(
                context, resolver, info, rvalue.operands, packed_context);
          },
          [&](const mir::UnaryRvalueInfo& info) -> Result<ComputeResult> {
            return LowerUnaryRvalue(
                context, resolver, info, rvalue.operands, packed_context);
          },
          [&](const mir::ConcatRvalueInfo& info) -> Result<ComputeResult> {
            return LowerConcatRvalue(
                context, resolver, info, rvalue.operands, packed_context);
          },
          [&](const mir::ReplicateRvalueInfo& info) -> Result<ComputeResult> {
            return LowerReplicateRvalue(
                context, resolver, info, rvalue.operands, packed_context);
          },
          [&](const mir::IsKnownRvalueInfo&) -> Result<ComputeResult> {
            return LowerIsKnown(
                context, resolver, rvalue.operands, packed_context);
          },
          [&](const mir::IndexInRangeRvalueInfo& info)
              -> Result<ComputeResult> {
            return LowerIndexInRange(
                context, resolver, info, rvalue.operands, packed_context);
          },
          [&](const mir::GuardedUseRvalueInfo& info) -> Result<ComputeResult> {
            return LowerGuardedUse(
                context, resolver, info, rvalue.operands, packed_context);
          },
          [&](const mir::RuntimeQueryRvalueInfo& info)
              -> Result<ComputeResult> {
            return LowerRuntimeQuery(context, info, packed_context);
          },
          [&](const auto& /*info*/) -> Result<ComputeResult> {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    std::format(
                        "unsupported rvalue kind in packed core path: {}",
                        mir::GetRvalueKind(rvalue.info)),
                    UnsupportedCategory::kFeature));
          },
      },
      rvalue.info);

  if (!result_or_err) return std::unexpected(result_or_err.error());

  auto masked =
      ApplyWidthMaskToResult(context, *result_or_err, type_info.bit_width);

  if (masked.IsFourState()) {
    return RvalueValue::FourState(masked.value, masked.unknown);
  }
  return RvalueValue::TwoState(masked.value);
}

}  // namespace lyra::lowering::mir_to_llvm
