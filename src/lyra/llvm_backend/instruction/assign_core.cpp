#include "lyra/llvm_backend/instruction/assign_core.hpp"

#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

auto ResolveDestType(
    Context& context, const CuFacts& facts, const mir::WriteTarget& dest)
    -> TypeId {
  if (const auto* place = std::get_if<mir::PlaceId>(&dest)) {
    return mir::TypeOfPlace(*facts.types, context.LookupPlace(*place));
  }
  return context.GetExternalRefType(std::get<mir::ExternalRefId>(dest));
}

// No-resolver TypeId overload.
auto LowerRhsRaw(
    Context& context, const CuFacts& facts, const mir::RightHandSide& rhs,
    TypeId target_type) -> Result<llvm::Value*> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerRhsRaw(context, facts, canonical, rhs, target_type);
}

// No-resolver PlaceId overload.
auto LowerRhsRaw(
    Context& context, const CuFacts& facts, const mir::RightHandSide& rhs,
    mir::PlaceId target) -> Result<llvm::Value*> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerRhsRaw(context, facts, canonical, rhs, target);
}

// PlaceId overload: derives TypeId from Place and forwards.
auto LowerRhsRaw(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::RightHandSide& rhs, mir::PlaceId target)
    -> Result<llvm::Value*> {
  const auto& types = *facts.types;
  TypeId target_type = mir::TypeOfPlace(types, context.LookupPlace(target));
  return LowerRhsRaw(context, facts, resolver, rhs, target_type);
}

auto LowerRhsRaw(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::RightHandSide& rhs, TypeId target_type) -> Result<llvm::Value*> {
  return std::visit(
      common::Overloaded{
          [&](const mir::Operand& operand) -> Result<llvm::Value*> {
            auto raw_or_err =
                LowerOperandRaw(context, facts, resolver, operand);
            if (!raw_or_err) return std::unexpected(raw_or_err.error());
            llvm::Value* raw = *raw_or_err;
            const auto& types = *facts.types;
            if (!raw->getType()->isStructTy() && IsPacked(types[target_type]) &&
                IsPackedFourState(facts, types[target_type])) {
              auto* llvm_type = GetLlvmTypeForTypeId(
                  context.GetLlvmContext(), target_type, types,
                  facts.force_two_state);
              auto* struct_type = llvm::cast<llvm::StructType>(llvm_type);
              auto* elem_type = struct_type->getElementType(0);
              auto& builder = context.GetBuilder();
              auto* value = builder.CreateZExtOrTrunc(raw, elem_type);
              auto* zero = llvm::ConstantInt::get(elem_type, 0);
              return PackFourState(builder, struct_type, value, zero);
            }
            return raw;
          },
          [&](const mir::Rvalue& rvalue) -> Result<llvm::Value*> {
            const auto& types = *facts.types;
            auto rv_result =
                LowerRvalue(context, facts, resolver, rvalue, target_type);
            if (!rv_result) return std::unexpected(rv_result.error());
            if (IsPacked(types[target_type]) &&
                IsPackedFourState(facts, types[target_type])) {
              auto* llvm_type = GetLlvmTypeForTypeId(
                  context.GetLlvmContext(), target_type, types,
                  facts.force_two_state);
              auto* struct_type = llvm::cast<llvm::StructType>(llvm_type);
              auto* elem_type = struct_type->getElementType(0);
              auto& builder = context.GetBuilder();
              llvm::Value* value =
                  builder.CreateZExtOrTrunc(rv_result->value, elem_type);
              llvm::Value* unknown = rv_result->unknown;
              if (unknown == nullptr) {
                unknown = llvm::ConstantInt::get(elem_type, 0);
              } else {
                unknown = builder.CreateZExtOrTrunc(unknown, elem_type);
              }
              return PackFourState(builder, struct_type, value, unknown);
            }
            return rv_result->value;
          },
      },
      rhs);
}

using PackedRValue = lyra::lowering::mir_to_llvm::PackedRValue;

auto LowerRhsToPackedRValue(
    Context& context, const CuFacts& facts, const mir::RightHandSide& rhs,
    uint32_t semantic_bits, TypeId result_type) -> Result<PackedRValue> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerRhsToPackedRValue(
      context, facts, canonical, rhs, semantic_bits, result_type);
}

auto LowerRhsToPackedRValue(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::RightHandSide& rhs, uint32_t semantic_bits, TypeId result_type)
    -> Result<PackedRValue> {
  return std::visit(
      common::Overloaded{
          [&](const mir::Operand& operand) -> Result<PackedRValue> {
            // Store planning is a semantic boundary: it must consume
            // TempValue directly for temp operands to preserve the
            // domain signal (unk == nullptr for kTwoState). Going
            // through LowerOperandRaw would lose this because
            // LowerOperandRaw is a domain-based raw adapter (scalar
            // for kTwoState) while BuildPackedRValueFromRaw infers
            // from shape. Direct TempValue access is the correct path.
            if (const auto* temp_id =
                    std::get_if<mir::TempId>(&operand.payload)) {
              const auto& tv = context.ReadTempValue(temp_id->value);
              PackedRValue result;
              result.val = tv.value;
              result.unk = tv.unknown;
              result.semantic_bits = semantic_bits;
              return result;
            }
            auto raw = LowerOperandRaw(context, facts, resolver, operand);
            if (!raw) return std::unexpected(raw.error());
            return BuildPackedRValueFromRaw(context, *raw, semantic_bits);
          },
          [&](const mir::Rvalue& rvalue) -> Result<PackedRValue> {
            auto rv_result =
                LowerRvalue(context, facts, resolver, rvalue, result_type);
            if (!rv_result) return std::unexpected(rv_result.error());
            // Invariant: rv_result->unknown is either nullptr (provably
            // 2-state) or a non-null SSA value (4-state). A 4-state
            // expression MAY legitimately produce ConstantInt(0) for the
            // unknown plane (all bits known). That is semantically
            // distinct from nullptr (provably 2-state), and both are
            // valid. No coercion is performed here.
            PackedRValue result;
            result.val = rv_result->value;
            result.unk = rv_result->unknown;
            result.semantic_bits = semantic_bits;
            return result;
          },
      },
      rhs);
}

}  // namespace lyra::lowering::mir_to_llvm::detail
