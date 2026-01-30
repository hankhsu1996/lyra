#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <utility>
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
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

auto Context::ResolveAliases(mir::PlaceId place_id) -> mir::Place {
  const mir::Place& original = arena_[place_id];

  // Only kDesign roots can be aliased
  if (original.root.kind != mir::PlaceRoot::Kind::kDesign) {
    return original;
  }

  mir::SlotId slot{static_cast<uint32_t>(original.root.id)};
  auto alias_it = design_.alias_map.find(slot);
  if (alias_it == design_.alias_map.end()) {
    return original;  // Not aliased
  }

  // Flatten alias chain with cycle detection
  std::unordered_set<mir::SlotId> visited;
  visited.insert(slot);
  mir::SlotId start_slot = slot;  // For error reporting

  mir::Place resolved = original;
  std::vector<mir::Projection> accumulated_projections = original.projections;

  while (alias_it != design_.alias_map.end()) {
    mir::PlaceId target_place_id = alias_it->second;
    const mir::Place& target = arena_[target_place_id];

    // Invariant: alias_map only maps to kDesign roots
    if (target.root.kind != mir::PlaceRoot::Kind::kDesign) {
      throw common::InternalError(
          "ResolveAliases",
          std::format(
              "alias target for slot {} is not a design slot", slot.value));
    }

    // Prepend target's projections (target.proj comes before our accumulated)
    std::vector<mir::Projection> new_projections = target.projections;
    new_projections.insert(
        new_projections.end(), accumulated_projections.begin(),
        accumulated_projections.end());
    accumulated_projections = std::move(new_projections);

    // Update resolved root
    resolved.root = target.root;

    mir::SlotId target_slot{static_cast<uint32_t>(target.root.id)};

    // Cycle detection
    if (visited.contains(target_slot)) {
      throw common::InternalError(
          "ResolveAliases",
          std::format(
              "alias cycle detected: slot {} -> ... -> slot {}",
              start_slot.value, target_slot.value));
    }
    visited.insert(target_slot);

    alias_it = design_.alias_map.find(target_slot);
  }

  resolved.projections = std::move(accumulated_projections);
  return resolved;
}

auto Context::GetPlacePointer(mir::PlaceId place_id) -> Result<llvm::Value*> {
  // Resolve aliases first, then delegate to the internal helper
  mir::Place resolved = ResolveAliases(place_id);
  return ComputePlacePointer(resolved, place_id);
}

auto Context::ComputePlacePointer(
    const mir::Place& resolved, mir::PlaceId original_place_id)
    -> Result<llvm::Value*> {
  // Get base pointer from root (resolved is already alias-resolved)
  llvm::Value* ptr = nullptr;
  if (resolved.root.kind == mir::PlaceRoot::Kind::kDesign) {
    if (design_ptr_ == nullptr) {
      throw common::InternalError("llvm_backend", "design pointer not set");
    }
    auto slot_id = mir::SlotId{static_cast<uint32_t>(resolved.root.id)};
    uint32_t field_index = GetDesignFieldIndex(slot_id);
    ptr = builder_.CreateStructGEP(
        GetDesignStateType(), design_ptr_, field_index, "design_slot_ptr");
  } else {
    // Local/Temp places: check place_storage_ first (user functions)
    // If not found and frame_ptr_ is set, use frame (processes)
    //
    // Storage is keyed by root identity (kind + id), NOT PlaceId.
    PlaceRootKey root_key{.kind = resolved.root.kind, .id = resolved.root.id};
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
  // Resolve aliases FIRST - all decisions based on this single resolved place
  mir::Place resolved = ResolveAliases(place_id);

  // Compute pointer directly from the resolved place (no re-resolution)
  auto ptr_or_err = ComputePlacePointer(resolved, place_id);
  if (!ptr_or_err) return std::unexpected(ptr_or_err.error());

  // Determine canonical_signal_id from the SAME resolved root
  std::optional<uint32_t> signal_id;
  if (resolved.root.kind == mir::PlaceRoot::Kind::kDesign) {
    signal_id = static_cast<uint32_t>(resolved.root.id);
  }

  return WriteTarget{
      .ptr = *ptr_or_err,
      .canonical_signal_id = signal_id,
  };
}

auto Context::GetCanonicalRootSignalId(mir::PlaceId place_id)
    -> std::optional<uint32_t> {
  // Resolve aliases first
  mir::Place resolved = ResolveAliases(place_id);

  // Return the resolved root's slot ID, or nullopt if not design
  if (resolved.root.kind != mir::PlaceRoot::Kind::kDesign) {
    return std::nullopt;
  }
  return static_cast<uint32_t>(resolved.root.id);
}

auto Context::GetPlaceLlvmType(mir::PlaceId place_id) -> Result<llvm::Type*> {
  const auto& place = arena_[place_id];

  TypeId type_id = mir::TypeOfPlace(types_, place);
  const Type& type = types_[type_id];

  if (type.Kind() == TypeKind::kIntegral) {
    uint32_t bit_width = type.AsIntegral().bit_width;
    if (type.AsIntegral().is_four_state) {
      return GetFourStateStructType(*llvm_context_, bit_width);
    }
    return GetLlvmStorageType(*llvm_context_, bit_width);
  }
  if (type.Kind() == TypeKind::kReal) {
    return llvm::Type::getDoubleTy(*llvm_context_);
  }
  if (type.Kind() == TypeKind::kShortReal) {
    return llvm::Type::getFloatTy(*llvm_context_);
  }
  if (type.Kind() == TypeKind::kString ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    return llvm::PointerType::getUnqual(*llvm_context_);
  }
  if (IsPacked(type)) {
    auto width = PackedBitWidth(type, types_);
    if (IsPackedFourState(type, types_)) {
      return GetFourStateStructType(*llvm_context_, width);
    }
    return GetLlvmStorageType(*llvm_context_, width);
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
  return GetFourStateStructType(*llvm_context_, bit_width);
}

auto Context::HasBitRangeProjection(mir::PlaceId place_id) const -> bool {
  const auto& place = arena_[place_id];
  return !place.projections.empty() &&
         std::holds_alternative<mir::BitRangeProjection>(
             place.projections.back().info);
}

auto Context::GetBitRangeProjection(mir::PlaceId place_id) const
    -> const mir::BitRangeProjection& {
  const auto& place = arena_[place_id];
  return std::get<mir::BitRangeProjection>(place.projections.back().info);
}

auto Context::GetPlaceBaseType(mir::PlaceId place_id) -> Result<llvm::Type*> {
  const auto& place = arena_[place_id];

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

auto Context::LoadPlaceValue(mir::PlaceId place_id) -> Result<llvm::Value*> {
  auto ptr_result = GetPlacePointer(place_id);
  if (!ptr_result) return std::unexpected(ptr_result.error());

  auto type_result = GetPlaceLlvmType(place_id);
  if (!type_result) return std::unexpected(type_result.error());

  return builder_.CreateLoad(*type_result, *ptr_result, "load");
}

auto Context::LoadPlaceBaseValue(mir::PlaceId place_id)
    -> Result<llvm::Value*> {
  auto ptr_result = GetPlacePointer(place_id);
  if (!ptr_result) return std::unexpected(ptr_result.error());

  auto type_result = GetPlaceBaseType(place_id);
  if (!type_result) return std::unexpected(type_result.error());

  return builder_.CreateLoad(*type_result, *ptr_result, "base");
}

auto Context::ComposeBitRange(mir::PlaceId place_id)
    -> Result<ComposedBitRange> {
  const auto& place = arena_[place_id];

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

}  // namespace lyra::lowering::mir_to_llvm
