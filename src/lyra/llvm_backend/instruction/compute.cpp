#include "lyra/llvm_backend/instruction/compute.hpp"

#include <cstddef>
#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Forward declaration for mutual recursion
auto StoreManagedStructLiteralToPtr(
    Context& context, llvm::Value* target_ptr, TypeId struct_type_id,
    const std::vector<mir::Operand>& operands) -> Result<void>;

// Store a single field from an operand, handling managed types recursively.
auto StoreFieldFromOperand(
    Context& context, llvm::Value* field_ptr, TypeId field_type_id,
    const mir::Operand& operand) -> Result<void> {
  const auto& types = context.GetTypeArena();
  const Type& field_type = types[field_type_id];

  if (field_type.Kind() == TypeKind::kString) {
    // String field: compute operand → store with lifecycle (no notify)
    auto val_or_err = LowerOperand(context, operand);
    if (!val_or_err) return std::unexpected(val_or_err.error());

    // For constants, the operand produces a freshly-owned handle (from
    // LyraStringFromLiteral). For places, we need to determine ownership.
    OwnershipPolicy policy = std::holds_alternative<Constant>(operand.payload)
                                 ? OwnershipPolicy::kMove
                                 : OwnershipPolicy::kClone;
    detail::CommitStringField(context, field_ptr, *val_or_err, policy);
    return {};
  }

  if (field_type.Kind() == TypeKind::kUnpackedStruct &&
      NeedsFieldByField(field_type_id, types)) {
    // Nested managed struct: currently not supported
    // This requires access to ConstantArena to resolve StructConstant field
    // references. For now, return unsupported.
    // TODO(hankhsu): Add support when ConstantArena access is available
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "nested struct with managed fields in aggregate literal",
        UnsupportedCategory::kType));
  }

  // Plain field: compute operand → store (no notify)
  auto val_or_err = LowerOperand(context, operand);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  detail::CommitPlainField(context, field_ptr, *val_or_err);
  return {};
}

// Store struct literal fields to target pointer. Internal function for
// recursion.
auto StoreManagedStructLiteralToPtr(
    Context& context, llvm::Value* target_ptr, TypeId struct_type_id,
    const std::vector<mir::Operand>& operands) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& struct_type = types[struct_type_id];
  const auto& struct_info = struct_type.AsUnpackedStruct();

  // Get LLVM struct type for GEP operations
  auto llvm_struct_type_result =
      BuildLlvmTypeForTypeId(context, struct_type_id);
  if (!llvm_struct_type_result)
    return std::unexpected(llvm_struct_type_result.error());
  auto* llvm_struct = llvm::cast<llvm::StructType>(*llvm_struct_type_result);

  // Store each field from its operand
  for (size_t i = 0; i < struct_info.fields.size(); ++i) {
    const auto& field = struct_info.fields[i];
    const auto& operand = operands[i];
    auto field_idx = static_cast<unsigned>(i);

    llvm::Value* field_ptr =
        builder.CreateStructGEP(llvm_struct, target_ptr, field_idx);

    auto result =
        StoreFieldFromOperand(context, field_ptr, field.type, operand);
    if (!result) return result;
  }

  return {};
}

// Store an aggregate literal field-by-field for structs containing managed
// types. Each field is computed from its operand and stored with lifecycle
// handling. Notifications are suppressed per-field; root notify is done once
// at the end.
auto LowerManagedStructLiteral(
    Context& context, const mir::Compute& compute, TypeId struct_type_id)
    -> Result<void> {
  // Get target pointer
  auto target_ptr_result = context.GetPlacePointer(compute.target);
  if (!target_ptr_result) return std::unexpected(target_ptr_result.error());
  llvm::Value* target_ptr = *target_ptr_result;

  // Store fields recursively
  auto result = StoreManagedStructLiteralToPtr(
      context, target_ptr, struct_type_id, compute.value.operands);
  if (!result) return result;

  // Single root notification after all fields are stored
  CommitNotifyAggregateIfDesignSlot(context, compute.target);

  return {};
}

}  // namespace

auto LowerComputeInstruction(Context& context, const mir::Compute& compute)
    -> Result<void> {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  TypeId result_type = mir::TypeOfPlace(types, arena[compute.target]);
  const Type& type = types[result_type];

  // Check for managed aggregate literal: handle field-by-field in instruction
  // layer rather than building LLVM aggregate in compute/ layer.
  if (std::holds_alternative<mir::AggregateRvalueInfo>(compute.value.info) &&
      NeedsFieldByField(result_type, types)) {
    if (type.Kind() == TypeKind::kUnpackedStruct) {
      return LowerManagedStructLiteral(context, compute, result_type);
    }
    // TODO(hankhsu): Add LowerManagedArrayLiteral when needed
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(), "managed array literal not yet supported",
        UnsupportedCategory::kType));
  }

  // Standard path: evaluate rvalue in compute/, store via commit/
  llvm::Value* unknown = nullptr;
  auto value_or_err = LowerRvalue(
      context, compute.value, compute.target, result_type, &unknown);
  if (!value_or_err) return std::unexpected(value_or_err.error());
  llvm::Value* value = *value_or_err;

  // For packed 4-state, pack value + unknown into struct before storing
  if (IsPacked(type) && IsPackedFourState(type, types)) {
    auto storage_type_or_err = context.GetPlaceLlvmType(compute.target);
    if (!storage_type_or_err)
      return std::unexpected(storage_type_or_err.error());
    auto* struct_type = llvm::cast<llvm::StructType>(*storage_type_or_err);
    // If unknown is null (2-state source), create zero unknown plane
    if (unknown == nullptr) {
      unknown = llvm::ConstantInt::get(value->getType(), 0);
    }
    value = PackFourState(context.GetBuilder(), struct_type, value, unknown);
  }

  // For packed types, use raw store (no ownership semantics)
  if (IsPacked(type)) {
    CommitPackedValueRaw(context, compute.target, value);
    return {};
  }

  // For POD unpacked aggregates (no managed fields), use simple store.
  // These don't go through CommitValue because CommitPackedValue can't handle
  // multi-field unpacked structs (it expects 2-element 4-state structs).
  if ((type.Kind() == TypeKind::kUnpackedStruct ||
       type.Kind() == TypeKind::kUnpackedArray) &&
      !NeedsFieldByField(result_type, types)) {
    auto target_ptr_result = context.GetPlacePointer(compute.target);
    if (!target_ptr_result) return std::unexpected(target_ptr_result.error());
    context.GetBuilder().CreateStore(value, *target_ptr_result);
    // Notify if design slot (aggregates use aggregate notify)
    CommitNotifyAggregateIfDesignSlot(context, compute.target);
    return {};
  }

  // For managed types (string, container, aggregates with managed fields),
  // use CommitValue which handles Destroy internally
  return CommitValue(
      context, compute.target, value, result_type, OwnershipPolicy::kMove);
}

}  // namespace lyra::lowering::mir_to_llvm
