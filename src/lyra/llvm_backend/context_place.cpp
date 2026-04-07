#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <variant>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/storage_boundary.hpp"
#include "lyra/llvm_backend/value_repr.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

auto Context::GetPlacePointer(mir::PlaceId place_id) -> Result<llvm::Value*> {
  return ComputePlacePointer(LookupPlace(place_id), place_id);
}

auto Context::ComputePlacePointer(
    const mir::Place& resolved, mir::PlaceId original_place_id)
    -> Result<llvm::Value*> {
  // Get base pointer from root.
  llvm::Value* ptr = nullptr;
  if (resolved.root.kind == mir::PlaceRoot::Kind::kModuleSlot ||
      resolved.root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
    ptr = GetSlotRootPointer(resolved.root);
  } else {
    // Local/Temp places: check place_alias_ first (inout managed params),
    // then place_storage_ (regular allocas), then frame (processes).
    //
    // Storage is keyed by root identity (kind + id), NOT PlaceId.
    PlaceRootKey root_key{.kind = resolved.root.kind, .id = resolved.root.id};

    // Check for aliased storage first (inout managed params)
    auto alias_it = place_alias_.find(root_key);
    if (alias_it != place_alias_.end()) {
      ptr = alias_it->second;
    } else {
      auto it = place_storage_.find(root_key);
      if (it != place_storage_.end()) {
        ptr = it->second;
      } else if (frame_ptr_ != nullptr) {
        uint32_t field_index = GetFrameFieldIndex(original_place_id);
        ptr = builder_.CreateStructGEP(
            GetProcessFrameType(), frame_ptr_, field_index, "frame_slot_ptr");
      } else {
        // User function: PlaceCollector/prologue must preallocate all storage.
        // If we reach here, the storage was never allocated.
        throw common::InternalError(
            "GetPlacePointer",
            std::format(
                "missing preallocated storage for PlaceId {} in user function; "
                "PlaceCollector/prologue must allocate all locals/temps",
                original_place_id.value));
      }
    }
  }

  // Apply projections (index into arrays, stop at BitRange)
  TypeId current_type = resolved.root.type;
  for (const auto& proj : resolved.projections) {
    if (std::holds_alternative<mir::BitRangeProjection>(proj.info)) {
      break;
    }

    // FieldProjection: struct field access
    if (const auto* field = std::get_if<mir::FieldProjection>(&proj.info)) {
      const Type& cur_type = types_[current_type];
      if (cur_type.Kind() != TypeKind::kUnpackedStruct) {
        throw common::InternalError(
            "GetPlacePointer", std::format(
                                   "FieldProjection on non-struct type: {}",
                                   static_cast<int>(cur_type.Kind())));
      }
      const auto& struct_info = cur_type.AsUnpackedStruct();

      auto field_idx = static_cast<size_t>(field->field_index);
      if (field_idx >= struct_info.fields.size()) {
        throw common::InternalError(
            "GetPlacePointer",
            std::format(
                "field index {} out of bounds (struct has {} fields)",
                field_idx, struct_info.fields.size()));
      }

      auto struct_type_result = BuildLlvmTypeForTypeId(*this, current_type);
      if (!struct_type_result)
        return std::unexpected(struct_type_result.error());
      llvm::Type* struct_type = *struct_type_result;
      ptr = builder_.CreateStructGEP(
          struct_type, ptr, static_cast<unsigned>(field->field_index),
          "struct_field_ptr");
      current_type = struct_info.fields[field_idx].type;
      continue;
    }

    // UnionMemberProjection: union member access (offset is always 0, type view
    // changes)
    if (const auto* umem =
            std::get_if<mir::UnionMemberProjection>(&proj.info)) {
      const Type& cur_type = types_[current_type];
      if (cur_type.Kind() != TypeKind::kUnpackedUnion) {
        throw common::InternalError(
            "GetPlacePointer",
            std::format(
                "UnionMemberProjection on non-union type: {}",
                static_cast<int>(cur_type.Kind())));
      }
      const auto& union_info = cur_type.AsUnpackedUnion();

      if (umem->member_index >= union_info.members.size()) {
        throw common::InternalError(
            "GetPlacePointer",
            std::format(
                "union member index {} out of bounds (union has {} members)",
                umem->member_index, union_info.members.size()));
      }

      // Update the type to the member type. The pointer remains unchanged
      // (offset is always 0), but subsequent operations will interpret it
      // as the member type.
      current_type = union_info.members[umem->member_index].type;
      continue;
    }

    const auto* idx = std::get_if<mir::IndexProjection>(&proj.info);
    if (idx == nullptr) {
      throw common::InternalError(
          "GetPlacePointer", "unsupported projection kind in LLVM backend");
    }

    const Type& cur_type = types_[current_type];
    if (cur_type.Kind() == TypeKind::kDynamicArray ||
        cur_type.Kind() == TypeKind::kQueue) {
      // Load the handle, call ElementPtr
      auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
      llvm::Value* handle = builder_.CreateLoad(ptr_ty, ptr, "da.handle");
      auto index_result = LowerOperand(*this, idx->index);
      if (!index_result) return std::unexpected(index_result.error());
      llvm::Value* index = builder_.CreateSExtOrTrunc(
          *index_result, llvm::Type::getInt64Ty(*llvm_context_), "da.idx");
      ptr = builder_.CreateCall(
          GetLyraDynArrayElementPtr(), {handle, index}, "da.elem_ptr");
      current_type = (cur_type.Kind() == TypeKind::kQueue)
                         ? cur_type.AsQueue().element_type
                         : cur_type.AsDynamicArray().element_type;
    } else {
      // Unpacked array: GEP into fixed array
      auto array_type_result = BuildLlvmTypeForTypeId(*this, current_type);
      if (!array_type_result) return std::unexpected(array_type_result.error());
      llvm::Type* array_type = *array_type_result;
      auto index_result = LowerOperand(*this, idx->index);
      if (!index_result) return std::unexpected(index_result.error());
      ptr = builder_.CreateGEP(
          array_type, ptr, {builder_.getInt32(0), *index_result},
          "array_elem_ptr");
      current_type = cur_type.AsUnpackedArray().element_type;
    }
  }

  return ptr;
}

auto Context::GetWriteTarget(mir::PlaceId place_id) -> Result<WriteTarget> {
  mir::Place resolved = LookupPlace(place_id);

  // Compute pointer from the place
  auto ptr_or_err = ComputePlacePointer(resolved, place_id);
  if (!ptr_or_err) return std::unexpected(ptr_or_err.error());

  // Determine canonical_signal_id from root
  std::optional<SignalCoordExpr> signal_id;
  uint32_t dirty_off = 0;
  uint32_t dirty_size = 0;
  if (resolved.root.kind == mir::PlaceRoot::Kind::kModuleSlot ||
      resolved.root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
    mir::SignalRef sig{
        .scope = (resolved.root.kind == mir::PlaceRoot::Kind::kModuleSlot)
                     ? mir::SignalRef::Scope::kModuleLocal
                     : mir::SignalRef::Scope::kDesignGlobal,
        .id = static_cast<uint32_t>(resolved.root.id),
    };
    // Mutation-target: resolve to storage owner for dirty-mark identity.
    signal_id = EmitMutationTargetSignalCoord(sig);
    auto resolver = [this](const mir::Operand& op) -> std::optional<uint64_t> {
      if (op.kind != mir::Operand::Kind::kUseTemp) return std::nullopt;
      auto temp_id = std::get<mir::TempId>(op.payload);
      if (!HasTemp(temp_id.value)) return std::nullopt;
      auto* ci = TryGetTempConstantInt(temp_id.value);
      if (ci != nullptr) {
        return ci->getZExtValue();
      }
      return std::nullopt;
    };
    const SlotStorageSpec* slot_spec_ptr = nullptr;
    if (resolved.root.kind == mir::PlaceRoot::Kind::kModuleSlot &&
        GetSpecSlotInfo() != nullptr) {
      auto local_slot = static_cast<uint32_t>(resolved.root.id);
      slot_spec_ptr = &GetSpecSlotInfo()->GetSlotSpec(local_slot, GetLayout());
    } else {
      auto slot_id = common::SlotId{static_cast<uint32_t>(resolved.root.id)};
      slot_spec_ptr = &GetDesignSlotStorageSpec(slot_id);
    }
    const auto& spec = *slot_spec_ptr;
    const auto& spec_arena = GetDesignStorageSpecArena();
    auto range = ResolveByteRange(spec, spec_arena, resolved, resolver);
    if (range.kind == RangeKind::kPrecise) {
      dirty_off = range.byte_offset;
      dirty_size = range.byte_size;
    }
  }

  // Resolve the canonical mutation signal once. Carried through
  // WriteTarget for both static contract and runtime observer queries.
  auto mutation_sig = ResolveMutationSignalRef(place_id);
  bool static_propagation = true;
  if (mutation_sig.has_value()) {
    static_propagation = RequiresStaticDirtyPropagation(*mutation_sig);
  }

  return WriteTarget{
      .ptr = *ptr_or_err,
      .canonical_signal_id = signal_id,
      .dirty_off = dirty_off,
      .dirty_size = dirty_size,
      .mutation_signal = mutation_sig,
      .requires_static_dirty_propagation = static_propagation,
  };
}

auto Context::ResolveMutationSignalRef(mir::PlaceId place_id) const
    -> std::optional<mir::SignalRef> {
  const mir::Place& resolved = LookupPlace(place_id);
  if (resolved.root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
    return mir::SignalRef{
        .scope = mir::SignalRef::Scope::kModuleLocal,
        .id = static_cast<uint32_t>(resolved.root.id)};
  }
  if (resolved.root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
    return mir::SignalRef{
        .scope = mir::SignalRef::Scope::kDesignGlobal,
        .id = static_cast<uint32_t>(resolved.root.id)};
  }
  return std::nullopt;
}

auto Context::GetMutationTargetSignalCoord(mir::PlaceId place_id)
    -> std::optional<SignalCoordExpr> {
  auto sig = ResolveMutationSignalRef(place_id);
  if (!sig.has_value()) return std::nullopt;
  return EmitMutationTargetSignalCoord(*sig);
}

auto Context::ResolveDesignGlobalSlotId(const mir::PlaceRoot& root)
    -> common::SlotId {
  if (root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
    return common::SlotId{static_cast<uint32_t>(root.id)};
  }
  throw common::InternalError(
      "ResolveDesignGlobalSlotId",
      std::format(
          "module-local root (kind={}) has no design-global identity; "
          "module-scoped code must use specialization-local addressing",
          static_cast<int>(root.kind)));
}

auto Context::ResolveDesignGlobalSlotId(const mir::SignalRef& sig)
    -> common::SlotId {
  if (sig.scope == mir::SignalRef::Scope::kDesignGlobal) {
    return common::SlotId{sig.id};
  }
  throw common::InternalError(
      "ResolveDesignGlobalSlotId",
      std::format(
          "module-local signal (id={}) has no design-global identity; "
          "module-scoped code must use specialization-local addressing",
          sig.id));
}

auto Context::ResolveDesignGlobalSlotId(const mir::ScopedSlotRef& ref)
    -> common::SlotId {
  if (ref.scope == mir::ScopedSlotRef::Scope::kDesignGlobal) {
    return common::SlotId{ref.id};
  }
  throw common::InternalError(
      "ResolveDesignGlobalSlotId",
      std::format(
          "module-local scoped slot (id={}) has no design-global identity; "
          "module-scoped code must use specialization-local addressing",
          ref.id));
}

auto Context::EmitSignalCoord(const mir::SignalRef& sig) -> SignalCoordExpr {
  if (sig.scope == mir::SignalRef::Scope::kModuleLocal) {
    if (slot_addressing_ == SlotAddressingMode::kSpecializationLocal) {
      return SignalCoordExpr::Local(sig.id);
    }
    throw common::InternalError(
        "EmitSignalCoord",
        std::format(
            "module-local signal (id={}) in design-global addressing mode; "
            "module-scoped code must use specialization-local addressing",
            sig.id));
  }
  // R5: Design-global signals that are actually instance-owned must emit
  // local identity with a resolved target instance pointer. This happens
  // for design-level connection processes writing to child port signals.
  if (sig.id >= layout_.num_package_slots) {
    auto owner = ResolveInstanceOwnedFlatSlot(layout_, sig.id);
    auto& builder = GetBuilder();
    auto* i32_ty = llvm::Type::getInt32Ty(GetLlvmContext());
    auto* target_inst = builder.CreateCall(
        GetLyraResolveInstancePtr(),
        {GetEnginePointer(),
         llvm::ConstantInt::get(i32_ty, owner.instance_id.value)});
    return SignalCoordExpr::LocalWithInstance(
        owner.local_signal_id.value, owner.instance_id, target_inst);
  }
  return SignalCoordExpr::Global(sig.id);
}

namespace {

// Validate a design-global signal ref is in the layout.
auto ValidateDesignGlobalSignal(
    const mir::SignalRef& sig, const DesignLayout& design) -> mir::SignalRef {
  auto slot_id = common::SlotId{sig.id};
  if (!design.ContainsSlot(slot_id)) {
    throw common::InternalError(
        "ValidateDesignGlobalSignal",
        std::format("design-global signal {} not in layout", sig.id));
  }
  return sig;
}

}  // namespace

auto Context::EmitMutationTargetSignalCoord(const mir::SignalRef& sig)
    -> SignalCoordExpr {
  if (sig.scope == mir::SignalRef::Scope::kDesignGlobal) {
    return EmitSignalCoord(ValidateDesignGlobalSignal(sig, layout_.design));
  }
  if (sig.scope == mir::SignalRef::Scope::kModuleLocal) {
    return EmitSignalCoord(sig);
  }
  throw common::InternalError(
      "EmitMutationTargetSignalCoord", "unknown signal scope");
}

auto Context::GetOwnedHandleLlvmType() -> llvm::StructType* {
  return llvm::StructType::get(
      *llvm_context_, {llvm::PointerType::getUnqual(*llvm_context_),
                       llvm::Type::getInt64Ty(*llvm_context_)});
}

auto Context::EmitInlineSlotPtr(uint32_t local_slot_id) -> llvm::Value* {
  if (this_ptr_ == nullptr) {
    throw common::InternalError(
        "EmitInlineSlotPtr",
        "this_ptr not set (module-local access requires shared-body context)");
  }
  if (spec_slot_info_ == nullptr) {
    throw common::InternalError("EmitInlineSlotPtr", "spec_slot_info not set");
  }
  if (local_slot_id >= spec_slot_info_->SlotCount()) {
    throw common::InternalError(
        "EmitInlineSlotPtr", std::format(
                                 "local_slot_id {} out of range (count={})",
                                 local_slot_id, spec_slot_info_->SlotCount()));
  }
  if (spec_slot_info_->IsOwnedContainer(local_slot_id)) {
    throw common::InternalError(
        "EmitInlineSlotPtr",
        std::format(
            "slot {} is kOwnedContainer, use EmitOwnedHandlePtr instead",
            local_slot_id));
  }
  auto offset = spec_slot_info_->inline_offsets[local_slot_id];
  auto* i8_ty = llvm::Type::getInt8Ty(*llvm_context_);
  return builder_.CreateGEP(
      i8_ty, this_ptr_, builder_.getInt64(offset.value), "inline_slot_ptr");
}

auto Context::EmitOwnedHandlePtr(uint32_t local_slot_id) -> llvm::Value* {
  if (this_ptr_ == nullptr) {
    throw common::InternalError(
        "EmitOwnedHandlePtr",
        "this_ptr not set (module-local access requires shared-body context)");
  }
  if (spec_slot_info_ == nullptr) {
    throw common::InternalError("EmitOwnedHandlePtr", "spec_slot_info not set");
  }
  if (local_slot_id >= spec_slot_info_->SlotCount()) {
    throw common::InternalError(
        "EmitOwnedHandlePtr", std::format(
                                  "local_slot_id {} out of range (count={})",
                                  local_slot_id, spec_slot_info_->SlotCount()));
  }
  if (!spec_slot_info_->IsOwnedContainer(local_slot_id)) {
    throw common::InternalError(
        "EmitOwnedHandlePtr",
        std::format(
            "slot {} is kInlineValue, use EmitInlineSlotPtr instead",
            local_slot_id));
  }
  auto offset = spec_slot_info_->inline_offsets[local_slot_id];
  auto* i8_ty = llvm::Type::getInt8Ty(*llvm_context_);
  return builder_.CreateGEP(
      i8_ty, this_ptr_, builder_.getInt64(offset.value), "owned_handle_ptr");
}

auto Context::EmitLoadOwnedDataPtr(llvm::Value* handle_ptr) -> llvm::Value* {
  // handle_ptr points to an OwnedStorageHandle in the inline region.
  // With opaque pointers, CreateStructGEP uses handle_ty for field offset
  // computation only -- no pointer type cast is needed or possible.
  auto* handle_ty = GetOwnedHandleLlvmType();
  auto* data_field =
      builder_.CreateStructGEP(handle_ty, handle_ptr, 0, "owned_data_gep");
  return builder_.CreateLoad(
      llvm::PointerType::getUnqual(*llvm_context_), data_field,
      "owned_data_ptr");
}

auto Context::GetModuleSlotPointer(uint32_t local_slot_id) -> llvm::Value* {
  if (spec_slot_info_ == nullptr) {
    throw common::InternalError(
        "GetModuleSlotPointer", "spec_slot_info not set");
  }
  if (local_slot_id >= spec_slot_info_->SlotCount()) {
    throw common::InternalError(
        "GetModuleSlotPointer",
        std::format(
            "local_slot_id {} out of range (count={})", local_slot_id,
            spec_slot_info_->SlotCount()));
  }

  auto access = spec_slot_info_->access_kinds[local_slot_id];
  switch (access) {
    case SpecSlotAccessKind::kOwnedContainer: {
      auto* handle_ptr = EmitOwnedHandlePtr(local_slot_id);
      return EmitLoadOwnedDataPtr(handle_ptr);
    }
    case SpecSlotAccessKind::kOwnedInline:
      return EmitInlineSlotPtr(local_slot_id);
  }
  throw common::InternalError(
      "GetModuleSlotPointer", "unreachable: unknown SpecSlotAccessKind");
}

auto Context::GetDesignGlobalSlotPointer(uint32_t global_slot_id)
    -> llvm::Value* {
  // When engine is available (simulation context), resolve through the
  // runtime slot resolver. This handles both design-global and
  // instance-owned slots correctly via SlotMeta domain dispatch.
  if (engine_ptr_ != nullptr) {
    return builder_.CreateCall(
        GetLyraResolveSlotPtr(),
        {engine_ptr_, builder_.getInt32(global_slot_id)}, "resolved_slot_ptr");
  }
  // Init context (no engine): fall back to design_ptr + arena offset.
  // Only valid for package/global slots during initialization.
  if (design_ptr_ == nullptr) {
    throw common::InternalError(
        "GetDesignGlobalSlotPointer", "design pointer not set");
  }
  uint64_t offset = GetDesignSlotByteOffset(common::SlotId{global_slot_id});
  return builder_.CreateGEP(
      llvm::Type::getInt8Ty(*llvm_context_), design_ptr_,
      builder_.getInt64(offset), "design_global_slot_ptr");
}

auto Context::GetSlotRootPointer(const mir::PlaceRoot& root) -> llvm::Value* {
  if (root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
    if (slot_addressing_ == SlotAddressingMode::kSpecializationLocal) {
      return GetModuleSlotPointer(static_cast<uint32_t>(root.id));
    }
    throw common::InternalError(
        "GetSlotRootPointer",
        std::format(
            "module-local slot (id={}) in design-global addressing mode; "
            "module-scoped code must use specialization-local addressing",
            root.id));
  }
  if (root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
    return GetDesignGlobalSlotPointer(static_cast<uint32_t>(root.id));
  }
  throw common::InternalError(
      "GetSlotRootPointer", "expected kModuleSlot or kDesignGlobal root");
}

auto Context::GetSignalSlotPointer(const mir::SignalRef& sig) -> llvm::Value* {
  if (sig.scope == mir::SignalRef::Scope::kModuleLocal) {
    if (slot_addressing_ == SlotAddressingMode::kSpecializationLocal) {
      return GetModuleSlotPointer(sig.id);
    }
    throw common::InternalError(
        "GetSignalSlotPointer",
        std::format(
            "module-local signal (id={}) in design-global addressing mode; "
            "module-scoped code must use specialization-local addressing",
            sig.id));
  }
  return GetDesignGlobalSlotPointer(sig.id);
}

auto Context::GetStorageRootPointer(mir::PlaceId place_id) -> llvm::Value* {
  return GetSlotRootPointer(LookupPlace(place_id).root);
}

auto Context::GetPlaceLlvmType(mir::PlaceId place_id) -> Result<llvm::Type*> {
  const auto& place = LookupPlace(place_id);

  TypeId type_id = mir::TypeOfPlace(types_, place);
  const Type& type = types_[type_id];

  if (type.Kind() == TypeKind::kIntegral) {
    uint32_t bit_width = type.AsIntegral().bit_width;
    if (IsPackedFourState(type)) {
      return GetBackingFourStateType(*llvm_context_, bit_width);
    }
    return GetBackingLlvmType(*llvm_context_, bit_width);
  }
  if (type.Kind() == TypeKind::kReal) {
    return llvm::Type::getDoubleTy(*llvm_context_);
  }
  if (type.Kind() == TypeKind::kShortReal) {
    return llvm::Type::getFloatTy(*llvm_context_);
  }
  if (type.Kind() == TypeKind::kString ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue ||
      type.Kind() == TypeKind::kAssociativeArray ||
      type.Kind() == TypeKind::kChandle) {
    return llvm::PointerType::getUnqual(*llvm_context_);
  }
  if (IsPacked(type)) {
    auto width = PackedBitWidth(type, types_);
    if (IsPackedFourState(type)) {
      return GetBackingFourStateType(*llvm_context_, width);
    }
    return GetBackingLlvmType(*llvm_context_, width);
  }

  if (type.Kind() == TypeKind::kUnpackedArray ||
      type.Kind() == TypeKind::kUnpackedStruct ||
      type.Kind() == TypeKind::kUnpackedUnion) {
    return BuildLlvmTypeForTypeId(*this, type_id);
  }

  return std::unexpected(
      GetDiagnosticContext().MakeUnsupported(
          current_origin_,
          std::format("type not yet supported: {}", ToString(type)),
          UnsupportedCategory::kType));
}

auto Context::GetPlaceLlvmType4State(uint32_t bit_width) -> llvm::StructType* {
  return GetBackingFourStateType(*llvm_context_, bit_width);
}

auto Context::HasBitRangeProjection(mir::PlaceId place_id) const -> bool {
  const auto& place = LookupPlace(place_id);
  return !place.projections.empty() &&
         std::holds_alternative<mir::BitRangeProjection>(
             place.projections.back().info);
}

auto Context::GetBitRangeProjection(mir::PlaceId place_id) const
    -> const mir::BitRangeProjection& {
  const auto& place = LookupPlace(place_id);
  return std::get<mir::BitRangeProjection>(place.projections.back().info);
}

auto Context::GetPlaceBaseType(mir::PlaceId place_id) -> Result<llvm::Type*> {
  const auto& place = LookupPlace(place_id);

  // The base type is root.type after applying all non-BitRange projections.
  // BitRange is always the last projection, so just use root.type for the
  // common case (no intermediate projections before the BitRange).
  TypeId base_type_id = place.root.type;
  for (const auto& proj : place.projections) {
    if (std::holds_alternative<mir::BitRangeProjection>(proj.info)) {
      break;
    }
    const Type& t = types_[base_type_id];
    if (t.Kind() == TypeKind::kUnpackedArray) {
      base_type_id = t.AsUnpackedArray().element_type;
    } else if (t.Kind() == TypeKind::kDynamicArray) {
      base_type_id = t.AsDynamicArray().element_type;
    } else if (t.Kind() == TypeKind::kQueue) {
      base_type_id = t.AsQueue().element_type;
    } else if (t.Kind() == TypeKind::kUnpackedStruct) {
      const auto* field = std::get_if<mir::FieldProjection>(&proj.info);
      if (field != nullptr) {
        base_type_id = t.AsUnpackedStruct()
                           .fields[static_cast<size_t>(field->field_index)]
                           .type;
      }
    } else if (t.Kind() == TypeKind::kUnpackedUnion) {
      const auto* umem = std::get_if<mir::UnionMemberProjection>(&proj.info);
      if (umem != nullptr) {
        base_type_id = t.AsUnpackedUnion().members[umem->member_index].type;
      }
    }
  }

  return BuildLlvmTypeForTypeId(*this, base_type_id);
}

// Shared logic: if the type is 4-state packed, load via canonical storage
// helper (plane-separated byte GEP). Otherwise return nullopt to let the
// caller fall through to a typed LLVM load.
auto Context::TryLoadCanonicalFourStateValue(llvm::Value* ptr, const Type& type)
    -> std::optional<llvm::Value*> {
  // Only packed types and integrals can be 4-state packed.
  // Must check before calling IsPackedFourState which requires a packed type.
  uint32_t bit_width = 0;
  if (IsPacked(type)) {
    if (!IsPackedFourState(type)) return std::nullopt;
    bit_width = PackedBitWidth(type, types_);
  } else if (type.Kind() == TypeKind::kIntegral) {
    if (!IsPackedFourState(type)) return std::nullopt;
    bit_width = type.AsIntegral().bit_width;
  } else {
    return std::nullopt;
  }

  return EmitLoadFourStateFromCanonical(
      builder_, *llvm_context_, ptr, bit_width);
}

auto Context::LoadPlaceValue(mir::PlaceId place_id) -> Result<llvm::Value*> {
  auto ptr_result = GetPlacePointer(place_id);
  if (!ptr_result) return std::unexpected(ptr_result.error());

  const auto& place = LookupPlace(place_id);
  TypeId type_id = mir::TypeOfPlace(types_, place);
  const Type& type = types_[type_id];

  // Canonical 4-state load only for design storage (module slots, design
  // globals). Process-local variables use LLVM struct layout which differs
  // from the flat canonical byte layout for wide types (e.g., {i80, i80}
  // struct places element 1 at offset 16 due to alignment, while canonical
  // layout places it at offset 10).
  auto root_kind = place.root.kind;
  if (root_kind == mir::PlaceRoot::Kind::kModuleSlot ||
      root_kind == mir::PlaceRoot::Kind::kDesignGlobal) {
    if (auto canonical = TryLoadCanonicalFourStateValue(*ptr_result, type)) {
      return *canonical;
    }
  }

  auto type_result = GetPlaceLlvmType(place_id);
  if (!type_result) return std::unexpected(type_result.error());
  return builder_.CreateLoad(*type_result, *ptr_result, "load");
}

auto Context::LoadPlaceBaseValue(mir::PlaceId place_id)
    -> Result<llvm::Value*> {
  auto ptr_result = GetPlacePointer(place_id);
  if (!ptr_result) return std::unexpected(ptr_result.error());

  const auto& place = LookupPlace(place_id);
  TypeId base_type_id = mir::TypeOfPlaceBase(types_, place);
  const Type& type = types_[base_type_id];

  auto root_kind = place.root.kind;
  if (root_kind == mir::PlaceRoot::Kind::kModuleSlot ||
      root_kind == mir::PlaceRoot::Kind::kDesignGlobal) {
    if (auto canonical = TryLoadCanonicalFourStateValue(*ptr_result, type)) {
      return *canonical;
    }
  }

  auto type_result = GetPlaceBaseType(place_id);
  if (!type_result) return std::unexpected(type_result.error());
  return builder_.CreateLoad(*type_result, *ptr_result, "base");
}

auto Context::ComposeBitRange(mir::PlaceId place_id)
    -> Result<ComposedBitRange> {
  const auto& place = LookupPlace(place_id);

  // Invariant: BitRangeProjections must form a contiguous suffix.
  // Once we see a BitRange, no non-BitRange projections may follow.
  bool seen_bitrange = false;
  for (const auto& proj : place.projections) {
    bool is_bitrange =
        std::holds_alternative<mir::BitRangeProjection>(proj.info);
    if (seen_bitrange && !is_bitrange) {
      throw common::InternalError(
          "ComposeBitRange",
          "non-BitRange projection after BitRange violates suffix invariant");
    }
    seen_bitrange = seen_bitrange || is_bitrange;
  }

  // Canonicalize all offsets to i32 (MIR bit offsets are always i32).
  auto* offset_ty = llvm::Type::getInt32Ty(*llvm_context_);
  llvm::Value* total_offset = nullptr;
  uint32_t width = 0;
  for (const auto& proj : place.projections) {
    if (!std::holds_alternative<mir::BitRangeProjection>(proj.info)) {
      continue;
    }
    const auto& br = std::get<mir::BitRangeProjection>(proj.info);
    auto br_offset_result = LowerOperand(*this, br.bit_offset);
    if (!br_offset_result) return std::unexpected(br_offset_result.error());
    llvm::Value* br_offset =
        builder_.CreateZExtOrTrunc(*br_offset_result, offset_ty);
    if (total_offset == nullptr) {
      total_offset = br_offset;
    } else {
      total_offset = builder_.CreateAdd(total_offset, br_offset);
    }
    width = br.width;
  }
  if (total_offset == nullptr) {
    throw common::InternalError(
        "ComposeBitRange", "called on place with no BitRangeProjection");
  }
  return ComposedBitRange{.offset = total_offset, .width = width};
}

auto Context::GetCurrentBodyRealizationInfo() const
    -> const Layout::BodyRealizationInfo& {
  if (spec_slot_info_ == nullptr) {
    throw common::InternalError(
        "Context::GetCurrentBodyRealizationInfo",
        "spec_slot_info_ not set (not in module-body codegen context)");
  }
  auto idx = spec_slot_info_->body_realization_info_index;
  if (idx == SpecSlotInfo::kInvalidBodyInfoIndex) {
    throw common::InternalError(
        "Context::GetCurrentBodyRealizationInfo",
        std::format(
            "body_realization_info_index not set for body {}",
            spec_slot_info_->body_id.value));
  }
  if (idx >= layout_.body_realization_infos.size()) {
    throw common::InternalError(
        "Context::GetCurrentBodyRealizationInfo",
        std::format(
            "body_realization_info_index {} out of range (size={})", idx,
            layout_.body_realization_infos.size()));
  }
  const auto& info = layout_.body_realization_infos[idx];
  if (info.body_id != spec_slot_info_->body_id) {
    throw common::InternalError(
        "Context::GetCurrentBodyRealizationInfo",
        std::format(
            "body identity mismatch: SpecSlotInfo body={}, "
            "BodyRealizationInfo body={}",
            spec_slot_info_->body_id.value, info.body_id.value));
  }
  return info;
}

auto Context::RequiresBehavioralDirtyPropagation(
    const mir::SignalRef& sig) const -> bool {
  if (sig.scope == mir::SignalRef::Scope::kModuleLocal) {
    const auto& body_info = GetCurrentBodyRealizationInfo();
    auto local_slot = static_cast<uint32_t>(sig.id);
    if (local_slot >= body_info.slot_has_behavioral_trigger.size()) {
      throw common::InternalError(
          "Context::RequiresBehavioralDirtyPropagation",
          std::format(
              "module-local slot {} out of range for body {} "
              "(behavioral trigger bitmap size={})",
              local_slot, GetSpecSlotInfo()->body_id.value,
              body_info.slot_has_behavioral_trigger.size()));
    }
    return body_info.slot_has_behavioral_trigger[local_slot];
  }
  if (sig.scope == mir::SignalRef::Scope::kDesignGlobal) {
    const auto& layout = GetLayout();
    auto slot = static_cast<uint32_t>(sig.id);
    if (slot >= layout.slot_has_design_behavioral_trigger.size()) {
      throw common::InternalError(
          "Context::RequiresBehavioralDirtyPropagation",
          std::format(
              "slot {} out of range for design behavioral trigger bitmap "
              "(size={})",
              slot, layout.slot_has_design_behavioral_trigger.size()));
    }
    return layout.slot_has_design_behavioral_trigger[slot];
  }
  throw common::InternalError(
      "Context::RequiresBehavioralDirtyPropagation",
      std::format("unknown signal scope {}", static_cast<int>(sig.scope)));
}

auto Context::RequiresConnectionNotification(const mir::SignalRef& sig) const
    -> bool {
  if (sig.scope == mir::SignalRef::Scope::kModuleLocal) {
    if (connection_notification_mask_ == nullptr) {
      throw common::InternalError(
          "Context::RequiresConnectionNotification",
          "module-local signal queried without connection notification mask");
    }
    auto local_slot = static_cast<uint32_t>(sig.id);
    return connection_notification_mask_->IsRequired(local_slot);
  }
  if (sig.scope == mir::SignalRef::Scope::kDesignGlobal) {
    const auto& layout = GetLayout();
    auto slot = static_cast<uint32_t>(sig.id);
    if (slot >= layout.slot_has_connection_trigger.size()) {
      throw common::InternalError(
          "Context::RequiresConnectionNotification",
          std::format(
              "slot {} out of range for connection trigger bitmap (size={})",
              slot, layout.slot_has_connection_trigger.size()));
    }
    return layout.slot_has_connection_trigger[slot];
  }
  throw common::InternalError(
      "Context::RequiresConnectionNotification",
      std::format("unknown signal scope {}", static_cast<int>(sig.scope)));
}

auto Context::RequiresStaticDirtyPropagation(const mir::SignalRef& sig) const
    -> bool {
  return RequiresBehavioralDirtyPropagation(sig) ||
         RequiresConnectionNotification(sig);
}

auto Context::GetLegacyRuntimeSignalSlot(const mir::SignalRef& sig) const
    -> uint32_t {
  const auto& layout = GetLayout();
  uint32_t owner_slot = 0;

  if (sig.scope == mir::SignalRef::Scope::kModuleLocal) {
    if (GetSpecSlotInfo() == nullptr) {
      throw common::InternalError(
          "Context::GetLegacyRuntimeSignalSlot",
          "module-local signal queried without active "
          "specialization context");
    }
    const auto& spec = *GetSpecSlotInfo();
    auto local_slot = static_cast<uint32_t>(sig.id);
    owner_slot = layout_.ResolveLegacyRepresentativeDesignSlot(
        spec.body_realization_info_index, local_slot);
  } else if (sig.scope == mir::SignalRef::Scope::kDesignGlobal) {
    owner_slot = static_cast<uint32_t>(sig.id);
  } else {
    throw common::InternalError(
        "Context::GetLegacyRuntimeSignalSlot", "unknown signal scope");
  }

  if (owner_slot >= layout.design.slots.size()) {
    throw common::InternalError(
        "Context::GetLegacyRuntimeSignalSlot",
        std::format(
            "owner slot {} out of range (design has {} slots)", owner_slot,
            layout.design.slots.size()));
  }
  return owner_slot;
}

auto Context::EmitIsTraceObservedOwnerSlot(uint32_t owner_slot)
    -> llvm::Value* {
  if (owner_slot >= layout_.design.slots.size()) {
    throw common::InternalError(
        "Context::EmitIsTraceObservedOwnerSlot",
        std::format(
            "owner slot {} out of range (design has {} slots)", owner_slot,
            layout_.design.slots.size()));
  }
  auto& builder = GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(GetLlvmContext());
  // Trace observation query uses the canonical storage-owner slot id,
  // which is always in the design-global coordination space (resolved
  // at compile time from the storage layout).
  return builder.CreateCall(
      GetLyraIsTraceObservedGlobal(),
      {GetEnginePointer(), llvm::ConstantInt::get(i32_ty, owner_slot)});
}

auto Context::EmitIsTraceObserved(const mir::SignalRef& sig) -> llvm::Value* {
  return EmitIsTraceObservedOwnerSlot(GetLegacyRuntimeSignalSlot(sig));
}

}  // namespace lyra::lowering::mir_to_llvm
