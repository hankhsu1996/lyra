#include "lyra/llvm_backend/layout/layout.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/constant.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/layout/layout_four_state.hpp"
#include "lyra/llvm_backend/layout/storage_types.hpp"
#include "lyra/llvm_backend/process_meta_utils.hpp"
#include "lyra/llvm_backend/value_repr.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_collect.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"
#include "lyra/runtime/owned_storage_handle.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::lowering::mir_to_llvm {

// Get the LLVM storage type for an integral type, rounding up to power-of-2.
namespace {

// GetLlvmTypeForTypeId is declared in the header (layout.hpp) and defined
// below (outside this anonymous namespace). Forward-declared here so
// BuildUnpackedStructType can call it recursively.
using ::lyra::lowering::mir_to_llvm::GetLlvmTypeForTypeId;

// Compute allocation size for a type in bytes (for union storage calculation)
auto ComputeAllocSize(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types) -> uint32_t;

// Compute alignment for a type in bytes (for union storage calculation)
auto ComputeAlignment(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> uint32_t {
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bits = type.AsIntegral().bit_width;
      // Round to power-of-2 storage, alignment = storage size (up to 8 bytes)
      if (bits <= 8) {
        return 1;
      }
      if (bits <= 16) {
        return 2;
      }
      if (bits <= 32) {
        return 4;
      }
      return 8;  // i64 and larger use 8-byte alignment
    }
    case TypeKind::kReal:
      return 8;  // double
    case TypeKind::kShortReal:
      return 4;  // float
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      uint32_t bits = PackedBitWidth(type, types);
      if (bits <= 8) {
        return 1;
      }
      if (bits <= 16) {
        return 2;
      }
      if (bits <= 32) {
        return 4;
      }
      return 8;
    }
    case TypeKind::kUnpackedStruct: {
      // Struct alignment = max alignment of fields
      const auto& info = type.AsUnpackedStruct();
      uint32_t max_align = 1;
      for (const auto& field : info.fields) {
        max_align =
            std::max(max_align, ComputeAlignment(ctx, field.type, types));
      }
      return max_align;
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      return ComputeAlignment(ctx, info.element_type, types);
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      uint32_t max_align = 1;
      for (const auto& member : info.members) {
        max_align =
            std::max(max_align, ComputeAlignment(ctx, member.type, types));
      }
      return max_align;
    }
    case TypeKind::kString:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      return 8;  // pointer alignment
    default:
      return 1;
  }
}

auto ComputeAllocSize(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> uint32_t {
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bits = type.AsIntegral().bit_width;
      // Round to power-of-2 storage
      if (bits <= 8) {
        return 1;
      }
      if (bits <= 16) {
        return 2;
      }
      if (bits <= 32) {
        return 4;
      }
      if (bits <= 64) {
        return 8;
      }
      // Larger integrals: round up to bytes
      return (bits + 7) / 8;
    }
    case TypeKind::kReal:
      return 8;  // double
    case TypeKind::kShortReal:
      return 4;  // float
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      uint32_t bits = PackedBitWidth(type, types);
      if (bits <= 8) {
        return 1;
      }
      if (bits <= 16) {
        return 2;
      }
      if (bits <= 32) {
        return 4;
      }
      if (bits <= 64) {
        return 8;
      }
      return (bits + 7) / 8;
    }
    case TypeKind::kUnpackedStruct: {
      // Sum of field sizes with alignment padding
      const auto& info = type.AsUnpackedStruct();
      uint32_t offset = 0;
      uint32_t max_align = 1;
      for (const auto& field : info.fields) {
        uint32_t field_align = ComputeAlignment(ctx, field.type, types);
        uint32_t field_size = ComputeAllocSize(ctx, field.type, types);
        max_align = std::max(max_align, field_align);
        // Align offset
        offset = (offset + field_align - 1) / field_align * field_align;
        offset += field_size;
      }
      // Pad to struct alignment
      return (offset + max_align - 1) / max_align * max_align;
    }
    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      uint32_t elem_size = ComputeAllocSize(ctx, info.element_type, types);
      return elem_size * info.range.Size();
    }
    case TypeKind::kUnpackedUnion: {
      const auto& info = type.AsUnpackedUnion();
      uint32_t max_size = 0;
      uint32_t max_align = 1;
      for (const auto& member : info.members) {
        max_size =
            std::max(max_size, ComputeAllocSize(ctx, member.type, types));
        max_align =
            std::max(max_align, ComputeAlignment(ctx, member.type, types));
      }
      // Round up to alignment
      return (max_size + max_align - 1) / max_align * max_align;
    }
    case TypeKind::kString:
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      return 8;  // pointer size
    default:
      throw common::InternalError(
          "ComputeAllocSize",
          std::format(
              "unsupported type kind: {}", static_cast<int>(type.Kind())));
  }
}

// Build LLVM byte array type for an unpacked union
auto BuildUnpackedUnionType(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type* {
  uint32_t size = ComputeAllocSize(ctx, type_id, types);
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  return llvm::ArrayType::get(i8_ty, size);
}

// Build LLVM struct type for an unpacked struct TypeId
auto BuildUnpackedStructType(
    llvm::LLVMContext& ctx, const UnpackedStructInfo& info,
    const TypeArena& types, bool force_two_state) -> llvm::Type* {
  std::vector<llvm::Type*> field_types;
  field_types.reserve(info.fields.size());
  for (const auto& field : info.fields) {
    field_types.push_back(
        GetLlvmTypeForTypeId(ctx, field.type, types, force_two_state));
  }
  return llvm::StructType::get(ctx, field_types);
}

}  // namespace

auto GetLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types,
    bool force_two_state) -> llvm::Type* {
  const Type& type = types[type_id];

  switch (type.Kind()) {
    case TypeKind::kIntegral: {
      uint32_t bit_width = type.AsIntegral().bit_width;
      if (IsLayoutPackedFourState(type, types, force_two_state)) {
        return GetBackingFourStateType(ctx, bit_width);
      }
      return GetBackingLlvmType(ctx, bit_width);
    }

    case TypeKind::kReal:
      return llvm::Type::getDoubleTy(ctx);

    case TypeKind::kString:
      return llvm::PointerType::getUnqual(ctx);

    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum: {
      auto width = PackedBitWidth(type, types);
      if (IsLayoutPackedFourState(type, types, force_two_state)) {
        return GetBackingFourStateType(ctx, width);
      }
      return GetBackingLlvmType(ctx, width);
    }

    case TypeKind::kUnpackedArray: {
      const auto& info = type.AsUnpackedArray();
      llvm::Type* elem =
          GetLlvmTypeForTypeId(ctx, info.element_type, types, force_two_state);
      return llvm::ArrayType::get(elem, info.range.Size());
    }

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
      return llvm::PointerType::getUnqual(ctx);

    case TypeKind::kShortReal:
      return llvm::Type::getFloatTy(ctx);

    case TypeKind::kUnpackedStruct:
      return BuildUnpackedStructType(
          ctx, type.AsUnpackedStruct(), types, force_two_state);

    case TypeKind::kUnpackedUnion:
      return BuildUnpackedUnionType(ctx, type_id, types);

    case TypeKind::kAssociativeArray:
      return llvm::PointerType::getUnqual(ctx);

    case TypeKind::kChandle:
      return llvm::PointerType::getUnqual(ctx);

    case TypeKind::kObjectHandle:
      return llvm::PointerType::getUnqual(ctx);

    case TypeKind::kVoid:
    case TypeKind::kEvent:
      throw common::InternalError(
          "GetLlvmTypeForTypeId",
          std::format(
              "{} type has no value-storage semantics", ToString(type.Kind())));
  }

  // Unreachable - all cases handled above
  throw common::InternalError("GetLlvmTypeForTypeId", "unreachable");
}

auto ResolveByteRange(
    const SlotStorageSpec& root_spec, const StorageSpecArena& arena,
    const mir::Place& place, const IndexResolver& resolve_index) -> ByteRange {
  if (place.projections.empty()) {
    return {.kind = RangeKind::kFullSlot};
  }

  const SlotStorageSpec* current_spec = &root_spec;
  uint64_t byte_offset = 0;
  uint8_t bit_index = 0;
  bool is_bit_range = false;

  for (const auto& proj : place.projections) {
    auto ok = std::visit(
        common::Overloaded{
            [&](const mir::FieldProjection& fp) -> bool {
              const auto* s =
                  std::get_if<StructStorageSpec>(&current_spec->data);
              if (s == nullptr) return false;
              auto field_idx = static_cast<size_t>(fp.field_index);
              if (field_idx >= s->fields.size()) return false;
              byte_offset += s->fields[field_idx].byte_offset;
              current_spec = &arena.Get(s->fields[field_idx].field_spec_id);
              return true;
            },
            [&](const mir::IndexProjection& ip) -> bool {
              const auto* s =
                  std::get_if<ArrayStorageSpec>(&current_spec->data);
              if (s == nullptr) return false;
              std::optional<uint64_t> index;
              if (ip.index.kind == mir::Operand::Kind::kConst) {
                const auto& constant = std::get<Constant>(ip.index.payload);
                const auto& integral =
                    std::get<IntegralConstant>(constant.value);
                index = static_cast<uint64_t>(integral.value[0]);
              } else if (resolve_index) {
                index = resolve_index(ip.index);
              }
              if (!index) return false;
              byte_offset += *index * static_cast<uint64_t>(s->element_stride);
              current_spec = &arena.Get(s->element_spec_id);
              return true;
            },
            [&](const mir::BitRangeProjection& br) -> bool {
              if (br.width != 1) return false;
              std::optional<int64_t> bit_off;
              if (br.bit_offset.kind == mir::Operand::Kind::kConst) {
                const auto& constant =
                    std::get<Constant>(br.bit_offset.payload);
                const auto& integral =
                    std::get<IntegralConstant>(constant.value);
                bit_off = static_cast<int64_t>(integral.value[0]);
              } else if (resolve_index) {
                if (auto resolved = resolve_index(br.bit_offset)) {
                  bit_off = static_cast<int64_t>(*resolved);
                }
              }
              if (!bit_off || *bit_off < 0) return false;
              const auto bit = static_cast<uint64_t>(*bit_off);
              byte_offset += bit / 8;
              bit_index = static_cast<uint8_t>(bit % 8);
              is_bit_range = true;
              return true;
            },
            [&](const auto&) -> bool { return false; },
        },
        proj.info);

    if (!ok) return {.kind = RangeKind::kFullSlot};
  }

  if (byte_offset > UINT32_MAX) return {.kind = RangeKind::kFullSlot};

  uint32_t leaf_byte_size = is_bit_range ? 1 : current_spec->TotalByteSize();
  return {
      .kind = RangeKind::kPrecise,
      .byte_offset = static_cast<uint32_t>(byte_offset),
      .byte_size = leaf_byte_size,
      .bit_index = bit_index,
  };
}

namespace {

// Get semantic width for a packed 4-state type.
// Precondition: type is a packed 4-state type (integral, packed array/struct,
// enum)
auto GetPackedSemanticWidth(TypeId type_id, const TypeArena& types)
    -> uint32_t {
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kIntegral:
      return type.AsIntegral().bit_width;
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum:
      return PackedBitWidth(type, types);
    default:
      throw common::InternalError(
          "GetPackedSemanticWidth", "called on non-packed type");
  }
}

// Add a patch entry to the appropriate bucket based on storage width.
// The mask has low semantic_width bits set.
void AddPatch(
    FourStatePatchTable& table, uint64_t offset, uint32_t semantic_width,
    uint32_t storage_width) {
  // Enforce invariant: semantic_width <= storage_width
  if (semantic_width > storage_width) {
    throw common::InternalError(
        "AddPatch", std::format(
                        "semantic_width ({}) > storage_width ({})",
                        semantic_width, storage_width));
  }

  // Compute mask: low semantic_width bits set
  uint64_t mask =
      (semantic_width >= 64) ? UINT64_MAX : (1ULL << semantic_width) - 1;

  switch (storage_width) {
    case 8:
      table.patches_8.emplace_back(offset, static_cast<uint8_t>(mask));
      break;
    case 16:
      table.patches_16.emplace_back(offset, static_cast<uint16_t>(mask));
      break;
    case 32:
      table.patches_32.emplace_back(offset, static_cast<uint32_t>(mask));
      break;
    case 64:
      table.patches_64.emplace_back(offset, mask);
      break;
    default:
      throw common::InternalError(
          "AddPatch",
          std::format("unexpected storage width: {}", storage_width));
  }
}

// Check if a type is scalar patchable by inspecting the LLVM field type.
// A type is patchable if it lowers to a struct {iW, iW} where W is 8/16/32/64.
auto IsFieldScalarPatchable(
    TypeId type_id, const TypeArena& types, llvm::Type* field_type,
    bool force_two_state) -> bool {
  const Type& type = types[type_id];

  // Must be packed and 4-state
  if (!IsPacked(type)) return false;
  if (!IsLayoutPackedFourState(type, types, force_two_state)) return false;

  // Must be a struct type with exactly 2 identical integer fields
  auto* struct_ty = llvm::dyn_cast<llvm::StructType>(field_type);
  if (struct_ty == nullptr || struct_ty->getNumElements() != 2) {
    return false;
  }

  auto* field0 = struct_ty->getElementType(0);
  auto* field1 = struct_ty->getElementType(1);
  if (field0 != field1 || !field0->isIntegerTy()) {
    return false;
  }

  // Storage width must be 8, 16, 32, or 64
  auto* int_ty = llvm::cast<llvm::IntegerType>(field0);
  uint32_t storage_width = int_ty->getBitWidth();
  return storage_width == 8 || storage_width == 16 || storage_width == 32 ||
         storage_width == 64;
}

// Collect patches for a struct type using DataLayout for accurate byte offsets.
// Only adds patches for scalar-patchable fields. Composite fields (arrays,
// structs with 4-state content) must be handled separately via recursive init.
template <typename TypeIdGetter>
void CollectPatches(
    FourStatePatchTable& table, llvm::StructType* struct_type,
    const llvm::DataLayout& dl, const TypeArena& types,
    TypeIdGetter get_type_id, size_t field_count, bool force_two_state) {
  const llvm::StructLayout* struct_layout = dl.getStructLayout(struct_type);

  for (size_t i = 0; i < field_count; ++i) {
    TypeId type_id = get_type_id(i);
    auto* field_type = struct_type->getElementType(static_cast<unsigned>(i));

    if (!IsFieldScalarPatchable(type_id, types, field_type, force_two_state)) {
      continue;
    }

    // Get field offset from parent struct
    uint64_t field_offset = struct_layout->getElementOffset(i);

    // Get the 4-state struct type (already validated as {iW, iW})
    auto* four_state_type = llvm::cast<llvm::StructType>(field_type);

    // Unknown plane is element 1 of the {iW, iW} struct
    const llvm::StructLayout* fs_layout = dl.getStructLayout(four_state_type);
    uint64_t unk_offset = fs_layout->getElementOffset(1);

    uint64_t total_offset = field_offset + unk_offset;

    // Get semantic width and storage width
    uint32_t semantic_width = GetPackedSemanticWidth(type_id, types);
    auto* elem_type = four_state_type->getElementType(0);
    auto storage_width = static_cast<uint32_t>(
        llvm::cast<llvm::IntegerType>(elem_type)->getBitWidth());

    AddPatch(table, total_offset, semantic_width, storage_width);
  }
}

// Collect a PlaceId from an Operand if it's a use
void CollectPlaceFromOperand(
    const mir::Operand& operand,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places) {
  if (operand.kind == mir::Operand::Kind::kUse) {
    places.insert(std::get<mir::PlaceId>(operand.payload));
  }
}

// Forward declarations for mutual recursion
void CollectPlacesFromRvalue(
    const mir::Rvalue& rvalue,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places);

// Collect PlaceIds from a RightHandSide
void CollectPlacesFromRhs(
    const mir::RightHandSide& rhs,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places) {
  std::visit(
      common::Overloaded{
          [&](const mir::Operand& op) { CollectPlaceFromOperand(op, places); },
          [&](const mir::Rvalue& rv) { CollectPlacesFromRvalue(rv, places); },
      },
      rhs);
}

// Collect PlaceIds from an Rvalue (operands + RvalueInfo).
// Uses exhaustive std::visit to ensure new RvalueInfo types with embedded
// PlaceIds cause compilation errors until handled.
void CollectPlacesFromRvalue(
    const mir::Rvalue& rvalue,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places) {
  for (const auto& operand : rvalue.operands) {
    CollectPlaceFromOperand(operand, places);
  }

  std::visit(
      [&](const auto& info) {
        using T = std::decay_t<decltype(info)>;

        if constexpr (std::is_same_v<T, mir::GuardedUseRvalueInfo>) {
          places.insert(info.place);
        } else if constexpr (std::is_same_v<T, mir::BuiltinCallRvalueInfo>) {
          if (info.receiver) {
            places.insert(*info.receiver);
          }
        } else if constexpr (std::is_same_v<T, mir::TestPlusargsRvalueInfo>) {
          // Test plusargs uses info.query, not Rvalue::operands
          CollectPlaceFromOperand(info.query.operand, places);
        } else if constexpr (std::is_same_v<T, mir::FopenRvalueInfo>) {
          // Fopen uses info.filename/mode, not Rvalue::operands
          CollectPlaceFromOperand(info.filename.operand, places);
          if (info.mode) {
            CollectPlaceFromOperand(info.mode->operand, places);
          }
        } else if constexpr (std::is_same_v<T, mir::SystemCmdRvalueInfo>) {
          // SystemCmd uses info.command, not Rvalue::operands
          if (info.command) {
            CollectPlaceFromOperand(info.command->operand, places);
          }
        } else if constexpr (std::is_same_v<T, mir::SFormatRvalueInfo>) {
          for (const auto& fop : info.ops) {
            if (fop.value) {
              CollectPlaceFromOperand(*fop.value, places);
            }
          }
        } else if constexpr (
            std::is_same_v<T, mir::UnaryRvalueInfo> ||
            std::is_same_v<T, mir::BinaryRvalueInfo> ||
            std::is_same_v<T, mir::CastRvalueInfo> ||
            std::is_same_v<T, mir::BitCastRvalueInfo> ||
            std::is_same_v<T, mir::AggregateRvalueInfo> ||
            std::is_same_v<T, mir::IsKnownRvalueInfo> ||
            std::is_same_v<T, mir::IndexInRangeRvalueInfo> ||
            std::is_same_v<T, mir::ConcatRvalueInfo> ||
            std::is_same_v<T, mir::ReplicateRvalueInfo> ||
            std::is_same_v<T, mir::RuntimeQueryRvalueInfo> ||
            std::is_same_v<T, mir::MathCallRvalueInfo> ||
            std::is_same_v<T, mir::SystemTfRvalueInfo> ||
            std::is_same_v<T, mir::ArrayQueryRvalueInfo> ||
            std::is_same_v<T, mir::SelectRvalueInfo> ||
            std::is_same_v<T, mir::ExternalReadRvalueInfo> ||
            std::is_same_v<T, mir::NewObjectRvalueInfo>) {
          // These RvalueInfo types have no embedded PlaceIds or Operands
          // beyond what's in Rvalue::operands (already collected above).
          // NewObjectRvalueInfo is not consumed by the LLVM backend in
          // this cut; listing it here keeps the exhaustive switch warning-
          // free while leaving backend semantics untouched.
        } else {
          static_assert(
              !sizeof(T),
              "Unhandled RvalueInfo type - if this type has embedded PlaceIds "
              "or Operands, add handling above");
        }
      },
      rvalue.info);
}

// Collect PlaceIds from an EffectOp
void CollectPlacesFromEffectOp(
    const mir::EffectOp& effect,
    std::unordered_set<mir::PlaceId, PlaceIdHash>& places) {
  std::visit(
      common::Overloaded{
          [&](const mir::DisplayEffect& d) {
            if (d.descriptor) {
              CollectPlaceFromOperand(*d.descriptor, places);
            }
            for (const auto& op : d.ops) {
              if (op.value) {
                CollectPlaceFromOperand(*op.value, places);
              }
            }
          },
          [&](const mir::ReportEffect& s) {
            for (const auto& op : s.ops) {
              if (op.value.has_value()) {
                CollectPlaceFromOperand(*op.value, places);
              }
            }
          },
          [&](const mir::MemIOEffect& m) {
            places.insert(m.target);
            CollectPlaceFromOperand(m.filename.operand, places);
            if (m.start_addr) {
              CollectPlaceFromOperand(*m.start_addr, places);
            }
            if (m.end_addr) {
              CollectPlaceFromOperand(*m.end_addr, places);
            }
          },
          [&](const mir::TimeFormatEffect&) {
            // TimeFormatEffect has no operands - all constants
          },
          [&](const mir::SystemTfEffect& s) {
            for (const auto& arg : s.args) {
              CollectPlaceFromOperand(arg, places);
            }
          },
          [&](const mir::StrobeEffect&) {
            // StrobeEffect only has FunctionId program, no operands
          },
          [&](const mir::MonitorEffect&) {
            // MonitorEffect only has FunctionIds and buffer size, no operands
          },
          [&](const mir::MonitorControlEffect&) {
            // MonitorControlEffect only has enable flag, no operands
          },
          [&](const mir::FillPackedEffect& f) {
            places.insert(f.target);
            CollectPlaceFromOperand(f.fill_value, places);
          },
          [&](const mir::RecordDecisionObservation&) {
            // Static payload only, no operands
          },
          [&](const mir::RecordDecisionObservationDynamic& r) {
            CollectPlaceFromOperand(r.match_class, places);
            CollectPlaceFromOperand(r.selected_kind, places);
            CollectPlaceFromOperand(r.selected_arm, places);
          },
          [&](const mir::CoverHitEffect&) {
            // CoverHitEffect carries only a site ID, no operands
          },
          [&](const mir::EnqueueDeferredAssertionEffect& e) {
            mir::ForEachReadOperand(e, [&](const mir::Operand& op) {
              CollectPlaceFromOperand(op, places);
            });
          },
          [&](const mir::ZeroInitStorageEffect& zi) {
            places.insert(zi.target);
          },
      },
      effect);
}

// Collect all PlaceIds referenced in a process.
// arena is needed for root-aware filtering of writeback destinations.
auto CollectProcessPlaces(const mir::Process& process, const mir::Arena& arena)
    -> std::unordered_set<mir::PlaceId, PlaceIdHash> {
  std::unordered_set<mir::PlaceId, PlaceIdHash> places;

  for (const auto& block : process.blocks) {
    for (const auto& instr : block.statements) {
      std::visit(
          common::Overloaded{
              [&](const auto& a)
                requires(mir::kIsDirectAssign<std::decay_t<decltype(a)>>)
              {
                if (const auto* p = std::get_if<mir::PlaceId>(&a.dest)) {
                  places.insert(*p);
                }
                CollectPlacesFromRhs(a.rhs, places);
              },
              [&](const mir::GuardedAssign& ga) {
                if (const auto* p = std::get_if<mir::PlaceId>(&ga.dest)) {
                  places.insert(*p);
                }
                CollectPlacesFromRhs(ga.rhs, places);
                CollectPlaceFromOperand(ga.guard, places);
              },
              [&](const mir::Effect& e) {
                CollectPlacesFromEffectOp(e.op, places);
              },
              [&](const mir::DeferredAssign& da) {
                if (const auto* p = std::get_if<mir::PlaceId>(&da.dest)) {
                  places.insert(*p);
                }
                CollectPlacesFromRhs(da.rhs, places);
              },
              [&](const mir::Call& call) {
                // Collect return tmp (not dest - dest may be design slot)
                if (call.ret) {
                  places.insert(call.ret->tmp);
                }
                // Collect writeback tmps (not dests)
                for (const auto& wb : call.writebacks) {
                  if (wb.tmp.has_value()) {
                    places.insert(*wb.tmp);
                  }
                }
                // Collect input args
                for (const auto& arg : call.in_args) {
                  CollectPlaceFromOperand(arg, places);
                }
              },
              [&](const mir::DpiCall& dc) {
                if (dc.ret) {
                  places.insert(dc.ret->tmp);
                }
                for (const auto& binding : dc.args) {
                  if (binding.input_value) {
                    CollectPlaceFromOperand(*binding.input_value, places);
                  }
                  if (binding.writeback_dest) {
                    mir::CollectIfProcessLocal(
                        *binding.writeback_dest, arena, places);
                  }
                }
              },
              [&](const mir::BuiltinCall& bcall) {
                if (bcall.dest) {
                  places.insert(*bcall.dest);
                }
                places.insert(bcall.receiver);
                for (const auto& arg : bcall.args) {
                  CollectPlaceFromOperand(arg, places);
                }
              },
              [&](const mir::DefineTemp& dt) {
                // DefineTemp doesn't create place storage; only visit RHS
                CollectPlacesFromRhs(dt.rhs, places);
              },
              [&](const mir::AssocOp& aop) {
                places.insert(aop.receiver);
                std::visit(
                    [&](const auto& op) {
                      if constexpr (requires { op.dest; }) {
                        places.insert(op.dest);
                      }
                      if constexpr (requires { op.key; }) {
                        CollectPlaceFromOperand(op.key, places);
                      }
                      if constexpr (requires { op.value; }) {
                        CollectPlaceFromOperand(op.value, places);
                      }
                      if constexpr (requires { op.out_key; }) {
                        places.insert(op.out_key);
                      }
                      if constexpr (requires { op.dest_found; }) {
                        places.insert(op.dest_found);
                      }
                      if constexpr (requires { op.key_place; }) {
                        places.insert(op.key_place);
                      }
                      if constexpr (requires { op.dest_keys; }) {
                        places.insert(op.dest_keys);
                      }
                    },
                    aop.data);
              },
              [](const mir::TriggerEvent&) {},
              [&](const mir::Initialize& init) {
                places.insert(init.dest);
                CollectPlaceFromOperand(init.value, places);
              },
              },
              instr.data);
    }

    std::visit(
        common::Overloaded{
            [&](const mir::Branch& b) {
              CollectPlaceFromOperand(b.condition, places);
            },
            [&](const mir::Switch& s) {
              CollectPlaceFromOperand(s.selector, places);
            },
            [](const auto&) {},
        },
        block.terminator.data);
  }

  return places;
}

struct RootInfo {
  PlaceRootKey key;
  TypeId type;
};

// Collect unique local/temp roots for a process, de-duplicating projected
// places. Storage class (frame vs alloca) is determined by the caller based
// on whether the process has suspension points.
auto CollectProcessRoots(
    const mir::Process& process, const mir::Arena& arena,
    const TypeArena& types) -> std::vector<RootInfo> {
  auto all_places = CollectProcessPlaces(process, arena);

  std::unordered_map<PlaceRootKey, TypeId, PlaceRootKeyHash> seen;
  for (mir::PlaceId place_id : all_places) {
    const auto& place = arena[place_id];
    if (place.root.kind == mir::PlaceRoot::Kind::kModuleSlot ||
        place.root.kind == mir::PlaceRoot::Kind::kDesignGlobal ||
        place.root.kind == mir::PlaceRoot::Kind::kObjectLocal) {
      continue;
    }
    // Void-typed places (e.g. targets of .delete()) never need storage
    if (types[place.root.type].Kind() == TypeKind::kVoid) {
      continue;
    }
    PlaceRootKey key{.kind = place.root.kind, .id = place.root.id};
    seen.emplace(key, place.root.type);
  }

  // Sort by (kind, id) for deterministic ordering
  std::vector<RootInfo> result;
  result.reserve(seen.size());
  for (const auto& [key, type] : seen) {
    result.push_back(RootInfo{.key = key, .type = type});
  }
  std::ranges::sort(result, [](const RootInfo& a, const RootInfo& b) {
    if (a.key.kind != b.key.kind) {
      return a.key.kind < b.key.kind;
    }
    return a.key.id < b.key.id;
  });
  return result;
}

// Conservative whole-process suspension classification (phase 1).
// True when the process contains any Delay or Wait terminator, meaning
// locals must persist across resumption and require frame storage.
// When false, all locals/temps can be plain allocas.
// Future: per-root classification for mixed processes.
auto ProcessHasSuspension(const mir::Process& process) -> bool {
  return std::ranges::any_of(process.blocks, [](const auto& block) {
    return std::holds_alternative<mir::Delay>(block.terminator.data) ||
           std::holds_alternative<mir::Wait>(block.terminator.data) ||
           std::holds_alternative<mir::WaitEvent>(block.terminator.data);
  });
}

// Check if a statement is "pure" for comb kernel batching.
// Pure means: no I/O, no NBA, no function calls, no container mutations.
auto IsStatementPure(const mir::Statement& stmt) -> bool {
  return std::visit(
      common::Overloaded{
          [](const mir::PlainAssign&) { return true; },
          [](const mir::CopyAssign&) { return true; },
          [](const mir::MoveAssign&) { return true; },
          [](const mir::GuardedAssign&) { return true; },
          [](const mir::DefineTemp& dt) {
            // DefineTemp is pure if RHS is pure
            if (const auto* rv = std::get_if<mir::Rvalue>(&dt.rhs)) {
              return !mir::RvalueHasSideEffects(rv->info);
            }
            return true;  // Operand RHS is always pure
          },
          [](const mir::Effect&) { return false; },
          [](const mir::DeferredAssign&) { return false; },
          [](const mir::Call&) { return false; },
          [](const mir::DpiCall&) { return false; },
          [](const mir::BuiltinCall&) { return false; },
          [](const mir::AssocOp&) { return false; },
          [](const mir::TriggerEvent&) { return false; },
          [](const mir::Initialize&) { return true; },
      },
      stmt.data);
}

// Check if a terminator is "pure control flow" (no delays, no events).
auto IsTerminatorPureCF(const mir::Terminator& term) -> bool {
  return std::visit(
      common::Overloaded{
          [](const mir::Jump&) { return true; },
          [](const mir::Branch&) { return true; },
          [](const mir::Switch&) { return true; },
          [](const mir::Wait&) { return true; },
          [](const mir::WaitEvent&) { return true; },
          [](const mir::Delay&) { return false; },
          [](const mir::Return&) { return false; },
          [](const mir::Finish&) { return false; },
          [](const mir::Repeat&) { return false; },
      },
      term.data);
}

// Build SuspendRecord as opaque blob - suspend helpers own the layout.
auto BuildSuspendRecordType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  static_assert(
      alignof(lyra::runtime::SuspendRecord) >= 8,
      "SuspendRecord must be at least 8-byte aligned for i64 blob");
  static_assert(
      sizeof(lyra::runtime::SuspendRecord) % 8 == 0,
      "SuspendRecord size must be multiple of 8 for i64 blob");
  auto* payload = llvm::ArrayType::get(
      llvm::Type::getInt64Ty(ctx), sizeof(lyra::runtime::SuspendRecord) / 8);
  return llvm::StructType::create(ctx, {payload}, "SuspendRecord");
}

// Build ProcessStateHeader as the minimal {suspend, outcome} prefix.
// Field order and count must match ProcessFrameHeader and
// ProcessFrameHeaderField in process_frame.hpp.
auto BuildHeaderType(llvm::LLVMContext& ctx, llvm::StructType* suspend_type)
    -> llvm::StructType* {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* outcome_ty =
      llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty, i32_ty});
  // { suspend, outcome }
  using F = lyra::runtime::ProcessFrameHeaderField;
  constexpr auto kFieldCount = static_cast<size_t>(F::kFieldCount);
  std::array<llvm::Type*, kFieldCount> fields = {suspend_type, outcome_ty};
  return llvm::StructType::create(ctx, fields, "ProcessStateHeader");
}

// Build RuntimeInstanceStorage LLVM struct type.
// Field order must match RuntimeInstanceStorage and
// RuntimeInstanceStorageField in runtime_instance.hpp.
auto BuildRuntimeInstanceStorageType(llvm::LLVMContext& ctx)
    -> llvm::StructType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  // { inline_base, inline_size, appendix_base, appendix_size,
  //   deferred_inline_base, deferred_appendix_base }
  using F = lyra::runtime::RuntimeInstanceStorageField;
  constexpr auto kFieldCount = static_cast<size_t>(F::kFieldCount);
  std::array<llvm::Type*, kFieldCount> fields = {ptr_ty, i64_ty, ptr_ty,
                                                 i64_ty, ptr_ty, ptr_ty};
  return llvm::StructType::create(ctx, fields, "RuntimeInstanceStorage");
}

// Build RuntimeInstance LLVM struct type.
// Field order must match RuntimeInstance and
// RuntimeInstanceField in runtime_instance.hpp.
auto BuildRuntimeInstanceType(
    llvm::LLVMContext& ctx, llvm::StructType* storage_type)
    -> llvm::StructType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  // { body, storage, ext_ref_bindings, ext_ref_binding_count }
  using F = lyra::runtime::RuntimeInstanceField;
  constexpr auto kFieldCount = static_cast<size_t>(F::kFieldCount);
  std::array<llvm::Type*, kFieldCount> fields = {
      ptr_ty, storage_type, ptr_ty, i32_ty};
  return llvm::StructType::create(ctx, fields, "RuntimeInstance");
}

}  // namespace

auto ResolveObservation(
    const mir::Arena& arena, const DesignLayout& design_layout,
    common::SlotId design_global_slot, mir::PlaceId place_id)
    -> std::optional<ResolvedObservation> {
  if (design_global_slot.value >= design_layout.slots.size()) {
    throw common::InternalError(
        "ResolveObservation",
        std::format(
            "design-global slot {} out of range ({} slots)",
            design_global_slot.value, design_layout.slots.size()));
  }
  return ResolveObservationFromSpec(
      arena, design_layout.slot_storage_specs[design_global_slot.value],
      design_layout.storage_spec_arena, place_id);
}

auto ResolveObservationFromSpec(
    const mir::Arena& arena, const SlotStorageSpec& spec,
    const StorageSpecArena& spec_arena, mir::PlaceId place_id)
    -> std::optional<ResolvedObservation> {
  const auto& place = arena[place_id];
  auto range = ResolveByteRange(spec, spec_arena, place, nullptr);
  if (range.kind != RangeKind::kPrecise) {
    return std::nullopt;
  }
  return ResolvedObservation{
      .byte_offset = range.byte_offset,
      .byte_size = range.byte_size,
      .bit_index = range.bit_index,
  };
}

auto AnalyzeCombKernel(const mir::Process& process, const mir::Arena& arena)
    -> std::optional<CombAnalysisResult> {
  if (process.kind != mir::ProcessKind::kLooping) return std::nullopt;

  const mir::Wait* wait_term = nullptr;

  // Track write slots with scope to avoid false self-edge matches
  // between module-local and design-global slots with the same numeric id.
  std::unordered_set<ScopedSignalKey, ScopedSignalKeyHash> write_slots;

  for (const auto& block : process.blocks) {
    for (const auto& stmt : block.statements) {
      if (!IsStatementPure(stmt)) return std::nullopt;

      if (auto ref = mir::TryGetDirectAssign(stmt.data); ref) {
        if (const auto* rv = std::get_if<mir::Rvalue>(ref.rhs)) {
          if (mir::RvalueHasSideEffects(rv->info)) return std::nullopt;
        }
        const auto* dest_pid = std::get_if<mir::PlaceId>(ref.dest);
        if (dest_pid == nullptr) return std::nullopt;
        const auto& root = arena[*dest_pid].root;
        if (root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
          write_slots.insert(
              {.scope = mir::SignalRef::Scope::kModuleLocal,
               .id = static_cast<uint32_t>(root.id)});
        } else if (root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
          write_slots.insert(
              {.scope = mir::SignalRef::Scope::kDesignGlobal,
               .id = static_cast<uint32_t>(root.id)});
        } else if (root.kind == mir::PlaceRoot::Kind::kObjectLocal) {
          write_slots.insert(
              {.scope = mir::SignalRef::Scope::kObjectLocal,
               .id = static_cast<uint32_t>(root.id),
               .object_index = root.object_index});
        }
      }
      if (const auto* ga = std::get_if<mir::GuardedAssign>(&stmt.data)) {
        if (const auto* rv = std::get_if<mir::Rvalue>(&ga->rhs)) {
          if (mir::RvalueHasSideEffects(rv->info)) return std::nullopt;
        }
        const auto* ga_dest_ptr = std::get_if<mir::PlaceId>(&ga->dest);
        if (ga_dest_ptr == nullptr) continue;
        auto ga_dest = *ga_dest_ptr;
        const auto& root = arena[ga_dest].root;
        if (root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
          write_slots.insert(
              {.scope = mir::SignalRef::Scope::kModuleLocal,
               .id = static_cast<uint32_t>(root.id)});
        } else if (root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
          write_slots.insert(
              {.scope = mir::SignalRef::Scope::kDesignGlobal,
               .id = static_cast<uint32_t>(root.id)});
        } else if (root.kind == mir::PlaceRoot::Kind::kObjectLocal) {
          write_slots.insert(
              {.scope = mir::SignalRef::Scope::kObjectLocal,
               .id = static_cast<uint32_t>(root.id),
               .object_index = root.object_index});
        }
      }
    }

    if (!IsTerminatorPureCF(block.terminator)) return std::nullopt;

    if (const auto* w = std::get_if<mir::Wait>(&block.terminator.data)) {
      if (wait_term != nullptr) return std::nullopt;
      wait_term = w;
    }
  }

  if (wait_term == nullptr) return std::nullopt;

  std::vector<CombAnalysisResult::TriggerFact> triggers;
  triggers.reserve(wait_term->triggers.size());
  for (const auto& trigger : wait_term->triggers) {
    if (trigger.edge != common::EdgeKind::kAnyChange) return std::nullopt;
    if (trigger.late_bound.has_value()) return std::nullopt;
    // Unresolved external refs cannot be analyzed at layout time (no
    // topology binding available). Fall back to Wait-based lowering
    // where FillTriggerArray will normalize them.
    if (trigger.unresolved_external_ref.has_value()) return std::nullopt;
    triggers.push_back({
        .signal = trigger.signal,
        .observed_place = trigger.observed_place,
    });
  }

  if (triggers.empty()) return std::nullopt;

  // Self-edge detection uses scoped identity to avoid false matches.
  bool has_self_edge = false;
  for (const auto& t : triggers) {
    if (write_slots.contains({.scope = t.signal.scope, .id = t.signal.id})) {
      has_self_edge = true;
      break;
    }
  }

  return CombAnalysisResult{
      .triggers = std::move(triggers),
      .has_self_edge = has_self_edge,
  };
}

auto BuildDesignLayout(
    std::span<const mir::SlotDesc> package_slots, const TypeArena& types,
    const llvm::DataLayout& dl, bool force_two_state,
    std::span<const InstanceSlotRange> instance_ranges) -> DesignLayout {
  DesignLayout layout;

  auto storage_mode =
      force_two_state ? StorageMode::kTwoState : StorageMode::kNormal;
  auto pointer_size = static_cast<uint32_t>(dl.getPointerSize());
  auto pointer_align =
      static_cast<uint32_t>(dl.getPointerABIAlignment(0).value());
  TargetStorageAbi target_abi{
      .pointer_byte_size = pointer_size,
      .pointer_alignment = pointer_align,
  };

  // Compute total flat slot count from package + per-instance sizes.
  auto total_slots = static_cast<uint32_t>(package_slots.size());
  for (const auto& range : instance_ranges) {
    total_slots += range.slot_count;
  }

  // Gather a flat storage_shape view for the invariant-check pass at the
  // end. This is the only place that needs random-access ordering across
  // both groups; layout stamping itself uses range-scoped iteration.
  std::vector<mir::StorageShape> slot_shapes;
  slot_shapes.reserve(total_slots);

  // Resolve storage spec for each slot in flat order (package then per
  // instance). Every slot owns its own storage.
  auto ingest_slot = [&](const mir::SlotDesc& desc, uint32_t global_idx) {
    if (global_idx != static_cast<uint32_t>(layout.slots.size())) {
      throw common::InternalError(
          "BuildDesignLayout",
          std::format(
              "slot identity invariant violated: global_idx {} != "
              "position {}",
              global_idx, layout.slots.size()));
    }
    layout.slots.push_back(common::SlotId{global_idx});
    layout.slot_storage_specs.push_back(ResolveStorageSpec(
        desc.type, types, storage_mode, target_abi, layout.storage_spec_arena));
    layout.slot_type_infos.push_back(
        ClassifySlotTypeInfo(desc.type, types, force_two_state));
    slot_shapes.push_back(desc.storage_shape);
  };

  for (size_t i = 0; i < package_slots.size(); ++i) {
    ingest_slot(package_slots[i], static_cast<uint32_t>(i));
  }
  for (const auto& range : instance_ranges) {
    if (range.body_slots.size() != range.slot_count) {
      throw common::InternalError(
          "BuildDesignLayout",
          std::format(
              "instance range slot_count {} != body_slots.size() {}",
              range.slot_count, range.body_slots.size()));
    }
    for (uint32_t i = 0; i < range.slot_count; ++i) {
      ingest_slot(range.body_slots[i], range.base_slot + i);
    }
  }

  layout.owned_data_offsets.resize(total_slots, std::nullopt);
  layout.slot_byte_offsets.resize(total_slots, 0);

  uint64_t max_align = 1;

  // Byte offset assignment: inline slots + owned-container appendix.
  //
  // When instance_ranges is provided (final layout), each instance's
  // appendix backing is placed contiguously after its inline slots so
  // that owned-container offsets are body-relative and repeatable.
  // Layout order: [pkg inline + pkg appendix][inst0 inline + inst0 appendix]...
  //
  // When instance_ranges is empty (preliminary layout), all package slots
  // are laid out sequentially with appendix at the end.
  //
  // Helper: assign inline byte offsets for a contiguous global slot range,
  // then assign appendix offsets for owned containers in that range.
  auto assign_group_offsets = [&](uint32_t begin, uint32_t end,
                                  uint64_t group_start) -> uint64_t {
    uint64_t offset = group_start;

    // Inline pass: all slots.
    for (uint32_t i = begin; i < end; ++i) {
      uint64_t slot_size = 0;
      uint64_t slot_align = 0;
      if (slot_shapes[i] == mir::StorageShape::kOwnedContainer) {
        slot_size = runtime::kOwnedStorageHandleByteSize;
        slot_align = runtime::kOwnedStorageHandleAlignment;
      } else {
        const auto& spec = layout.slot_storage_specs[i];
        slot_size = spec.TotalByteSize();
        slot_align = spec.Alignment();
      }
      max_align = std::max(max_align, slot_align);
      offset = AlignUp(offset, slot_align);
      layout.slot_byte_offsets[i] = offset;
      offset += slot_size;
    }

    // Appendix pass: owned-container backing data.
    offset = AlignUp(offset, max_align);
    for (uint32_t i = begin; i < end; ++i) {
      if (slot_shapes[i] != mir::StorageShape::kOwnedContainer) {
        continue;
      }
      const auto& spec = layout.slot_storage_specs[i];
      if (spec.Alignment() == 0 || spec.TotalByteSize() == 0) {
        throw common::InternalError(
            "BuildDesignLayout",
            std::format(
                "owned-container slot {} has invalid backing spec "
                "(alignment={}, size={})",
                i, spec.Alignment(), spec.TotalByteSize()));
      }
      uint64_t backing_align = spec.Alignment();
      max_align = std::max(max_align, backing_align);
      offset = AlignUp(offset, backing_align);
      layout.owned_data_offsets[i] = offset;
      offset += spec.TotalByteSize();
    }

    return AlignUp(offset, max_align);
  };

  if (instance_ranges.empty()) {
    // Preliminary layout: package slots in one group, appendix at end.
    uint64_t end_offset = assign_group_offsets(0, total_slots, 0);
    layout.inline_region_size = end_offset;
    layout.arena_size = std::max(uint64_t{1}, end_offset);
  } else {
    uint64_t running = 0;
    auto num_package_slots = static_cast<uint32_t>(package_slots.size());

    // Package group.
    if (num_package_slots > 0) {
      running = assign_group_offsets(0, num_package_slots, running);
    }

    // Per-instance groups.
    for (const auto& range : instance_ranges) {
      if (range.slot_count == 0) continue;
      running = assign_group_offsets(
          range.base_slot, range.base_slot + range.slot_count, running);
    }

    layout.inline_region_size = running;
    layout.arena_size = std::max(uint64_t{1}, running);
  }

  // Verify invariants: kInlineValue slots must have nullopt owned offset,
  // kOwnedContainer slots must have engaged owned offset.
  for (size_t i = 0; i < total_slots; ++i) {
    bool is_container = slot_shapes[i] == mir::StorageShape::kOwnedContainer;
    if (is_container && !layout.owned_data_offsets[i].has_value()) {
      throw common::InternalError(
          "BuildDesignLayout",
          std::format("owned-container slot {} missing appendix offset", i));
    }
    if (!is_container && layout.owned_data_offsets[i].has_value()) {
      throw common::InternalError(
          "BuildDesignLayout",
          std::format("inline-value slot {} has spurious appendix offset", i));
    }
  }

  // Build storage binding table. Every slot owns storage.
  layout.slot_storage_bindings.reserve(total_slots);
  for (size_t i = 0; i < total_slots; ++i) {
    layout.slot_storage_bindings.push_back(
        OwnedLocalStorage{ArenaByteOffset{layout.slot_byte_offsets[i]}});
  }

  return layout;
}

auto BuildBodyStorageLayout(
    std::span<const mir::SlotDesc> slots, const TypeArena& types,
    const llvm::DataLayout& dl, bool force_two_state) -> BodyStorageLayout {
  auto storage_mode =
      force_two_state ? StorageMode::kTwoState : StorageMode::kNormal;
  auto pointer_size = static_cast<uint32_t>(dl.getPointerSize());
  auto pointer_align =
      static_cast<uint32_t>(dl.getPointerABIAlignment(0).value());
  TargetStorageAbi target_abi{
      .pointer_byte_size = pointer_size,
      .pointer_alignment = pointer_align,
  };

  BodyStorageLayout result;
  result.slot_specs.reserve(slots.size());
  for (const auto& slot : slots) {
    result.slot_specs.push_back(ResolveStorageSpec(
        slot.type, types, storage_mode, target_abi, result.spec_arena));
  }
  return result;
}

auto ComputeBodyStateSize(
    std::span<const mir::SlotDesc> slot_descs, const BodyStorageLayout& storage)
    -> BodyStateSizeInfo {
  uint64_t max_align = 1;
  uint64_t offset = 0;

  // Inline pass: all body-local slots are storage owners (body-local
  // storage ownership is identity -- no body-local alias concept in MIR).
  for (uint32_t i = 0; i < slot_descs.size(); ++i) {
    uint64_t slot_size = 0;
    uint64_t slot_align = 0;
    if (slot_descs[i].storage_shape == mir::StorageShape::kOwnedContainer) {
      slot_size = runtime::kOwnedStorageHandleByteSize;
      slot_align = runtime::kOwnedStorageHandleAlignment;
    } else {
      slot_size = storage.slot_specs[i].TotalByteSize();
      slot_align = storage.slot_specs[i].Alignment();
    }
    max_align = std::max(max_align, slot_align);
    offset = AlignUp(offset, slot_align);
    offset += slot_size;
  }

  uint64_t inline_end = AlignUp(offset, max_align);

  // Appendix pass: owned-container backing data.
  offset = inline_end;
  for (uint32_t i = 0; i < slot_descs.size(); ++i) {
    if (slot_descs[i].storage_shape != mir::StorageShape::kOwnedContainer) {
      continue;
    }
    uint64_t backing_align = storage.slot_specs[i].Alignment();
    max_align = std::max(max_align, backing_align);
    offset = AlignUp(offset, backing_align);
    offset += storage.slot_specs[i].TotalByteSize();
  }

  uint64_t total_end = AlignUp(offset, max_align);
  return BodyStateSizeInfo{
      .inline_bytes = inline_end,
      .appendix_bytes = total_end - inline_end,
      .total_bytes = total_end,
  };
}

auto BuildBodyLayout(
    std::span<const mir::SlotDesc> slot_descs, const BodyStorageLayout& storage)
    -> BodyLayout {
  BodyLayout layout;
  layout.inline_offsets.reserve(slot_descs.size());
  layout.appendix_offsets.resize(slot_descs.size(), std::nullopt);

  uint64_t max_align = 1;
  uint64_t offset = 0;

  // Inline pass: assign body-relative byte offsets to each slot.
  for (uint32_t i = 0; i < slot_descs.size(); ++i) {
    uint64_t slot_size = 0;
    uint64_t slot_align = 0;
    if (slot_descs[i].storage_shape == mir::StorageShape::kOwnedContainer) {
      slot_size = runtime::kOwnedStorageHandleByteSize;
      slot_align = runtime::kOwnedStorageHandleAlignment;
    } else {
      slot_size = storage.slot_specs[i].TotalByteSize();
      slot_align = storage.slot_specs[i].Alignment();
    }
    max_align = std::max(max_align, slot_align);
    offset = AlignUp(offset, slot_align);
    layout.inline_offsets.push_back(BodyByteOffset{offset});
    offset += slot_size;
  }

  layout.inline_region_size = AlignUp(offset, max_align);

  // Appendix pass: owned-container backing data.
  offset = layout.inline_region_size;
  for (uint32_t i = 0; i < slot_descs.size(); ++i) {
    if (slot_descs[i].storage_shape != mir::StorageShape::kOwnedContainer) {
      continue;
    }
    uint64_t backing_align = storage.slot_specs[i].Alignment();
    max_align = std::max(max_align, backing_align);
    offset = AlignUp(offset, backing_align);
    layout.appendix_offsets[i] = BodyByteOffset{offset};
    offset += storage.slot_specs[i].TotalByteSize();
  }

  layout.appendix_region_size =
      AlignUp(offset, max_align) - layout.inline_region_size;

  return layout;
}

// DesignLayout method implementations.

auto DesignLayout::ContainsSlot(common::SlotId slot_id) const -> bool {
  return slot_id.value < slots.size();
}

auto DesignLayout::GetStorageByteOffset(common::SlotId slot_id) const
    -> uint64_t {
  if (slot_id.value >= slots.size()) {
    throw common::InternalError(
        "DesignLayout::GetStorageByteOffset",
        std::format(
            "slot {} not in layout ({} slots)", slot_id.value, slots.size()));
  }
  return slot_byte_offsets[slot_id.value];
}

auto DesignLayout::GetStorageSpec(common::SlotId slot_id) const
    -> const SlotStorageSpec& {
  if (slot_id.value >= slots.size()) {
    throw common::InternalError(
        "DesignLayout::GetStorageSpec",
        std::format(
            "slot {} not in layout ({} slots)", slot_id.value, slots.size()));
  }
  return slot_storage_specs[slot_id.value];
}

auto DesignLayout::GetSlotStorageBinding(uint32_t slot_row) const
    -> const OwnedLocalStorage& {
  if (slot_row >= slot_storage_bindings.size()) {
    throw common::InternalError(
        "DesignLayout::GetSlotStorageBinding",
        std::format(
            "slot_row {} out of range (size={})", slot_row,
            slot_storage_bindings.size()));
  }
  return slot_storage_bindings[slot_row];
}

auto DesignLayout::GetInstanceOffset(
    uint32_t slot_row, const InstanceStorageBase& instance_base,
    const InstanceSlotRange& range) const -> common::InstanceByteOffset {
  if (slot_row < range.base_slot ||
      slot_row >= range.base_slot + range.slot_count) {
    throw common::InternalError(
        "DesignLayout::GetInstanceOffset",
        std::format(
            "slot_row {} outside instance range [{}, {})", slot_row,
            range.base_slot, range.base_slot + range.slot_count));
  }
  const auto& binding = GetSlotStorageBinding(slot_row);
  if (!instance_base.abs_byte_offset.has_value()) {
    throw common::InternalError(
        "DesignLayout::GetInstanceOffset",
        std::format(
            "slot_row {} requires instance storage base, "
            "but the instance base is missing",
            slot_row));
  }
  return ToInstanceOffset(
      binding.abs_byte_offset, *instance_base.abs_byte_offset);
}

auto DesignLayout::GetBodyOffset(
    uint32_t slot_row, ArenaByteOffset body_base) const -> BodyByteOffset {
  const auto& binding = GetSlotStorageBinding(slot_row);
  return ToBodyOffset(binding.abs_byte_offset, body_base);
}

auto DesignLayout::GetStorageBaseForRange(
    uint32_t base_slot, uint32_t slot_count) const
    -> std::optional<ArenaByteOffset> {
  if (slot_count == 0) return std::nullopt;
  return GetSlotStorageBinding(base_slot).abs_byte_offset;
}

namespace {

// Descriptor for an activation-local shadow field to append to the frame.
struct ShadowFieldDesc {
  uint32_t slot_id;
  TypeId root_type;
};

// Build FrameLayout from de-duplicated roots and optional shadow fields.
// Shadow fields are appended after regular roots in the LLVM struct.
// frame_name is the semantic type name (e.g., "Body0Frame0", "Conn3Frame").
// If extra_llvm_type is non-null, it is appended as the last field and
// *out_extra_field_index is set to its struct field index.
auto BuildFrameLayout(
    const std::vector<RootInfo>& roots,
    std::span<const ShadowFieldDesc> shadows, const TypeArena& types,
    llvm::LLVMContext& ctx, std::string_view frame_name,
    const llvm::DataLayout& dl, bool force_two_state,
    llvm::Type* extra_llvm_type = nullptr,
    uint32_t* out_extra_field_index = nullptr) -> FrameLayout {
  FrameLayout layout;
  layout.num_semantic_roots = static_cast<uint32_t>(roots.size());
  std::vector<llvm::Type*> llvm_field_types;

  // Region 1: semantic roots from MIR places.
  for (size_t i = 0; i < roots.size(); ++i) {
    const auto& root = roots[i];
    layout.field_types.push_back(root.type);
    layout.root_to_field[root.key] = static_cast<uint32_t>(i);
    llvm_field_types.push_back(
        GetLlvmTypeForTypeId(ctx, root.type, types, force_two_state));
  }

  // Region 2: activation-local shadow fields.
  for (const auto& shadow : shadows) {
    auto field_index = static_cast<uint32_t>(llvm_field_types.size());
    layout.shadow_fields.push_back(
        ShadowFieldEntry{
            .slot_id = shadow.slot_id,
            .field_index = field_index,
            .root_type = shadow.root_type,
        });
    layout.field_types.push_back(shadow.root_type);
    llvm_field_types.push_back(
        GetLlvmTypeForTypeId(ctx, shadow.root_type, types, force_two_state));
  }

  // Region 3: optional trailing extra field (expression connection binding).
  if (extra_llvm_type != nullptr) {
    if (out_extra_field_index != nullptr) {
      *out_extra_field_index = static_cast<uint32_t>(llvm_field_types.size());
    }
    llvm_field_types.push_back(extra_llvm_type);
  }

  if (llvm_field_types.empty()) {
    layout.llvm_type = llvm::StructType::create(
        ctx, {llvm::Type::getInt8Ty(ctx)}, std::string(frame_name));
  } else {
    layout.llvm_type = llvm::StructType::create(
        ctx, llvm_field_types, std::string(frame_name));
  }

  // Collect 4-state patches over both regions (exclude extra field).
  size_t total_fields = layout.field_types.size();
  if (total_fields > 0) {
    CollectPatches(
        layout.four_state_patches, layout.llvm_type, dl, types,
        [&](size_t i) { return layout.field_types[i]; }, total_fields,
        force_two_state);
  }

  return layout;
}

// Build process state type: {ProcessStateHeader, Frame}
// state_name is the semantic type name (e.g., "Body0State0", "Conn3State").
auto BuildProcessStateType(
    llvm::LLVMContext& ctx, llvm::StructType* header_type,
    llvm::StructType* frame_type, std::string_view state_name)
    -> llvm::StructType* {
  return llvm::StructType::create(
      ctx, {header_type, frame_type}, std::string(state_name));
}

}  // namespace

auto FrameLayout::GetShadowField(uint32_t slot_id) const
    -> const ShadowFieldEntry& {
  for (const auto& entry : shadow_fields) {
    if (entry.slot_id == slot_id) return entry;
  }
  throw common::InternalError(
      "FrameLayout::GetShadowField",
      std::format("no shadow field for managed slot {}", slot_id));
}

auto IsScalarPatchable(
    TypeId type_id, const TypeArena& types, bool force_two_state) -> bool {
  const Type& type = types[type_id];

  // Must be packed and 4-state
  if (!IsPacked(type)) return false;
  if (!IsLayoutPackedFourState(type, types, force_two_state)) return false;

  uint32_t width = PackedBitWidth(type, types);
  // Storage width must fit in 8/16/32/64 bits
  return width <= 64;
}

auto ClassifySlotTypeInfo(
    TypeId type_id, const TypeArena& types, bool force_two_state)
    -> SlotTypeInfo {
  const Type& type = types[type_id];
  if (type.Kind() == TypeKind::kReal) {
    return {
        .kind = VarTypeKind::kReal,
        .width = 64,
        .is_signed = true,
        .is_four_state = false,
    };
  }
  if (type.Kind() == TypeKind::kString) {
    return {
        .kind = VarTypeKind::kString,
        .width = 0,
        .is_signed = false,
        .is_four_state = false,
    };
  }
  if (IsPacked(type)) {
    uint32_t width = PackedBitWidth(type, types);
    return {
        .kind = VarTypeKind::kIntegral,
        .width = width > 0 ? width : 32,
        .is_signed = IsPackedSigned(type, types),
        .is_four_state = IsLayoutPackedFourState(type, types, force_two_state),
    };
  }
  // Unsupported type: placeholder metadata. The actual TypeId is preserved
  // upstream for LLVM type derivation.
  return {
      .kind = VarTypeKind::kIntegral,
      .width = 32,
      .is_signed = false,
      .is_four_state = false,
  };
}

auto Layout::GetInstanceStorageBase(ModuleIndex idx) const
    -> const InstanceStorageBase& {
  if (idx.value >= instance_storage_bases.size()) {
    throw common::InternalError(
        "GetInstanceStorageBase",
        std::format(
            "module_index {} out of range (size={})", idx.value,
            instance_storage_bases.size()));
  }
  return instance_storage_bases[idx.value];
}

auto Layout::GetInstanceSlotCount(ModuleIndex idx) const -> uint32_t {
  if (idx.value >= instance_slot_counts.size()) {
    throw common::InternalError(
        "GetInstanceSlotCount", std::format(
                                    "module_index {} out of range (size={})",
                                    idx.value, instance_slot_counts.size()));
  }
  return instance_slot_counts[idx.value];
}

auto ResolveInstanceOwnedFlatSlot(
    uint32_t num_package_slots, std::span<const uint32_t> instance_slot_counts,
    uint32_t flat_slot_id) -> SlotOwnerInfo {
  uint32_t running_base = num_package_slots;
  for (uint32_t mi = 0; mi < instance_slot_counts.size(); ++mi) {
    uint32_t count = instance_slot_counts[mi];
    if (flat_slot_id < running_base + count) {
      return {
          .instance_id = runtime::InstanceId{mi},
          .local_signal_id =
              runtime::LocalSignalId{flat_slot_id - running_base},
      };
    }
    running_base += count;
  }
  throw common::InternalError(
      "ResolveInstanceOwnedFlatSlot",
      std::format(
          "flat slot {} not found in any instance slot range "
          "(num_package_slots={}, num_instances={})",
          flat_slot_id, num_package_slots, instance_slot_counts.size()));
}

auto BuildLayout(
    std::span<const mir::ProcessId> init_processes,
    std::vector<ConnectionKernelEntry> precollected_connection_kernels,
    std::span<const LayoutModulePlan> module_plans,
    std::span<const std::span<const mir::ProcessId>> module_body_processes,
    const mir::Design& /*design*/, const mir::Arena& design_arena,
    const TypeArena& types, DesignLayout design_layout,
    const std::unordered_map<const mir::ModuleBody*, BodyStorageLayout>&
        body_storage_layouts,
    llvm::LLVMContext& ctx, const llvm::DataLayout& dl, bool force_two_state)
    -> Layout {
  Layout layout;

  // Build runtime types
  layout.suspend_record_type = BuildSuspendRecordType(ctx);
  layout.header_type = BuildHeaderType(ctx, layout.suspend_record_type);
  layout.runtime_instance_storage_type = BuildRuntimeInstanceStorageType(ctx);
  layout.runtime_instance_type =
      BuildRuntimeInstanceType(ctx, layout.runtime_instance_storage_type);

  // Use prebuilt design layout
  layout.design = std::move(design_layout);

  // Phase 1: Collect init processes (package variable initialization)
  for (mir::ProcessId proc_id : init_processes) {
    layout.scheduled_processes.push_back({proc_id, ModuleIndex{}});
  }
  layout.num_init_processes = layout.scheduled_processes.size();

  // Connection kernels are pipeline-owned pre-layout data.
  // BuildLayout consumes them; it does not collect or canonicalize them.
  layout.connection_kernel_entries = std::move(precollected_connection_kernels);

  // Build connection dirty-propagation contract from trigger slots.
  // Covers both memcpy-style connection kernels and installable-computation
  // deps: both are reactive consumers whose source signals must always
  // mark dirty on write so the shared reactive trigger model sees them.
  layout.slot_has_connection_trigger.assign(layout.design.slots.size(), false);
  for (const auto& entry : layout.connection_kernel_entries) {
    auto trigger = entry.trigger_slot.value;
    if (trigger >= layout.slot_has_connection_trigger.size()) {
      throw common::InternalError(
          "BuildLayout",
          std::format(
              "connection trigger slot {} out of range (design has "
              "{} slots)",
              trigger, layout.slot_has_connection_trigger.size()));
    }
    layout.slot_has_connection_trigger[trigger] = true;
  }
  // Installable-computation deps: per-instance body-local dep slots
  // become design-global trigger slots via the instance's slot base.
  // Parent-side writes to these slots must always mark dirty so the IC
  // fires in the shared reactive dispatch.
  for (const auto& plan : module_plans) {
    if (plan.body == nullptr) continue;
    for (const auto& ic : plan.body->installable_computations) {
      for (const auto& dep : ic.deps) {
        uint32_t global_slot = plan.design_state_base_slot + dep.value;
        if (global_slot >= layout.slot_has_connection_trigger.size()) {
          throw common::InternalError(
              "BuildLayout",
              std::format(
                  "installable computation dep global slot {} out of range "
                  "(design has {} slots)",
                  global_slot, layout.slot_has_connection_trigger.size()));
        }
        layout.slot_has_connection_trigger[global_slot] = true;
      }
    }
  }

  // Phase 3: Collect module processes (run through scheduler)
  layout.num_module_process_base = layout.scheduled_processes.size();
  for (uint32_t mi = 0; mi < module_plans.size(); ++mi) {
    const auto& plan = module_plans[mi];
    const auto& body_arena = plan.body->arena;
    for (mir::ProcessId proc_id : module_body_processes[mi]) {
      const auto& process = body_arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;
      layout.scheduled_processes.push_back({proc_id, ModuleIndex{mi}});
    }
  }

  // Build process layouts.
  // Resolve the correct arena for each scheduled process:
  // init/connection processes use design_arena, body processes use body arena.
  auto resolve_arena = [&](const ScheduledProcess& sp) -> const mir::Arena& {
    if (sp.module_index.value < module_plans.size()) {
      return module_plans[sp.module_index.value].body->arena;
    }
    return design_arena;
  };

  layout.processes.reserve(layout.scheduled_processes.size());

  // Build canonical non-final ordinal maps per body. These are the single
  // source of truth for mapping ProcessId <-> dense non-final ordinal.
  // Keyed by body pointer: instances of the same body share a map entry.
  absl::flat_hash_map<const mir::ModuleBody*, BodyProcessOrdinalMap>
      body_ordinal_maps;
  for (const auto& plan : module_plans) {
    if (body_ordinal_maps.contains(plan.body)) continue;
    body_ordinal_maps.emplace(
        plan.body, BuildBodyProcessOrdinalMap(*plan.body));
  }

  // Counter for semantic naming.
  uint32_t init_counter = 0;

  for (size_t i = 0; i < layout.scheduled_processes.size(); ++i) {
    const auto& sp = layout.scheduled_processes[i];
    const auto& sched_arena = resolve_arena(sp);
    const auto& process = sched_arena[sp.process_id];

    // Determine semantic name based on process category.
    std::string frame_name;
    std::string state_name;
    if (i < layout.num_init_processes) {
      frame_name = std::format("Init{}Frame", init_counter);
      state_name = std::format("Init{}State", init_counter);
      ++init_counter;
    } else {
      uint32_t mi = sp.module_index.value;
      const auto* body_ptr = module_plans[mi].body;
      const auto& ordinal_map = body_ordinal_maps.at(body_ptr);
      uint32_t nonfinal_proc_ordinal =
          GetNonFinalOrdinal(ordinal_map, sp.process_id);
      // Use module_index as local naming ordinal (deterministic, no
      // transported body identity). Multiple instances of the same body
      // share the same layout, so any representative works.
      frame_name = std::format("M{}Frame{}", mi, nonfinal_proc_ordinal);
      state_name = std::format("M{}State{}", mi, nonfinal_proc_ordinal);
    }

    ProcessLayout proc_layout;
    proc_layout.process_index = i;
    proc_layout.has_suspension = ProcessHasSuspension(process);

    auto roots = CollectProcessRoots(process, sched_arena, types);
    llvm::Type* extra_type = nullptr;
    uint32_t extra_field_idx = UINT32_MAX;

    if (proc_layout.has_suspension) {
      // Compute activation plan at layout time so shadow fields can be
      // added to the persistent process frame (not stack allocas).
      proc_layout.activation_plan.emplace(
          BuildProcessActivationPlan(process, sched_arena, types));

      // Collect deduplicated shadow field descriptors from the plan.
      std::vector<ShadowFieldDesc> shadows;
      if (proc_layout.activation_plan->HasActivationLocalSlots()) {
        std::unordered_set<uint32_t> seen;
        for (const auto& seg : proc_layout.activation_plan->segments) {
          for (const auto& ms : seg.managed_slots.slots) {
            if (seen.insert(ms.slot.id).second) {
              shadows.push_back(
                  ShadowFieldDesc{
                      .slot_id = ms.slot.id, .root_type = ms.root_type});
            }
          }
        }
        std::ranges::sort(shadows, {}, &ShadowFieldDesc::slot_id);
      }

      proc_layout.frame = BuildFrameLayout(
          roots, shadows, types, ctx, frame_name, dl, force_two_state,
          extra_type, &extra_field_idx);

      // Structural invariant: every managed slot in the activation plan
      // must resolve to a frame shadow field. This guards against drift
      // between plan semantics and frame population, making the
      // stale-stack-alloca bug class structurally impossible.
      for (const auto& seg : proc_layout.activation_plan->segments) {
        for (const auto& ms : seg.managed_slots.slots) {
          static_cast<void>(proc_layout.frame.GetShadowField(ms.slot.id));
        }
      }
    } else {
      // Suspension-free: empty frame, roots become allocas at codegen time.
      proc_layout.frame = BuildFrameLayout(
          {}, {}, types, ctx, frame_name, dl, force_two_state, extra_type,
          &extra_field_idx);
      proc_layout.alloca_roots.reserve(roots.size());
      for (const auto& root : roots) {
        proc_layout.alloca_roots.push_back(
            AllocaRootInfo{.key = root.key, .type = root.type});
      }
    }

    proc_layout.state_type = BuildProcessStateType(
        ctx, layout.header_type, proc_layout.frame.llvm_type, state_name);

    layout.processes.push_back(std::move(proc_layout));
  }

  // Build constructor metadata: state schemas and constructor records.
  // Only covers simulation processes (post-init: connection + module).
  {
    // Compute constructor-relevant schema properties for a process layout.
    // All properties that grouped schemas must agree on are computed here.
    // When adding new schema fields, add them to this helper so the
    // grouped-schema consistency check covers them automatically.
    struct SchemaProps {
      uint64_t state_size;
      uint64_t state_align;
      bool needs_4state_init;

      auto operator==(const SchemaProps&) const -> bool = default;
    };
    auto compute_schema_props =
        [&](const ProcessLayout& proc_layout) -> SchemaProps {
      uint64_t sz = dl.getTypeAllocSize(proc_layout.state_type);
      uint64_t al = dl.getABITypeAlign(proc_layout.state_type).value();
      bool needs =
          !force_two_state && !proc_layout.frame.four_state_patches.IsEmpty();
      if (!force_two_state && !needs) {
        for (TypeId type_id : proc_layout.frame.field_types) {
          if (IsLayoutFourState(type_id, types, force_two_state) &&
              !IsScalarPatchable(type_id, types, force_two_state)) {
            needs = true;
            break;
          }
        }
      }
      return {.state_size = sz, .state_align = al, .needs_4state_init = needs};
    };

    // Map from (body pointer, dense non-final ordinal) to schema index for
    // module process deduplication.
    struct SchemaKey {
      const mir::ModuleBody* body;
      uint32_t proc_within_body;  // Dense non-final body-local ordinal.
      auto operator==(const SchemaKey&) const -> bool = default;
    };
    struct SchemaKeyHash {
      auto operator()(const SchemaKey& k) const -> size_t {
        return std::hash<const void*>{}(k.body) ^
               std::hash<uint32_t>{}(k.proc_within_body);
      }
    };
    std::unordered_map<SchemaKey, uint32_t, SchemaKeyHash> module_schema_map;

    for (size_t i = layout.num_init_processes;
         i < layout.scheduled_processes.size(); ++i) {
      const auto& sp = layout.scheduled_processes[i];
      auto props = compute_schema_props(layout.processes[i]);

      {
        // Module process: deduplicate by (body, nonfinal_proc_ordinal).
        uint32_t mi = sp.module_index.value;
        const auto* body_ptr = module_plans[mi].body;
        const auto& ordinal_map = body_ordinal_maps.at(body_ptr);
        uint32_t nonfinal_proc_ordinal =
            GetNonFinalOrdinal(ordinal_map, sp.process_id);

        SchemaKey key{
            .body = body_ptr, .proc_within_body = nonfinal_proc_ordinal};
        auto it = module_schema_map.find(key);
        if (it != module_schema_map.end()) {
          // Existing schema: assert all constructor-relevant properties
          // and naming/debug anchors match.
          const auto& existing = layout.state_schemas[it->second];
          auto existing_props = SchemaProps{
              .state_size = existing.state_size,
              .state_align = existing.state_align,
              .needs_4state_init = existing.needs_4state_init,
          };
          if (existing_props != props) {
            throw common::InternalError(
                "BuildLayout",
                "grouped module processes disagree on schema properties");
          }
          if (existing.proc_within_body != nonfinal_proc_ordinal ||
              existing.conn_index != std::nullopt) {
            throw common::InternalError(
                "BuildLayout",
                "grouped module processes disagree on schema identity");
          }
          // Verify representative_process_index is a valid anchor for
          // the same schema identity.
          const auto& rep_sp =
              layout.scheduled_processes[existing.representative_process_index];
          uint32_t rep_mi = rep_sp.module_index.value;
          const auto* rep_body_ptr = module_plans[rep_mi].body;
          const auto& rep_ordinal_map = body_ordinal_maps.at(rep_body_ptr);
          uint32_t rep_nonfinal_proc_ordinal =
              GetNonFinalOrdinal(rep_ordinal_map, rep_sp.process_id);
          if (rep_body_ptr != body_ptr ||
              rep_nonfinal_proc_ordinal != nonfinal_proc_ordinal) {
            throw common::InternalError(
                "BuildLayout",
                "representative process does not match schema identity");
          }
        } else {
          auto schema_idx = static_cast<uint32_t>(layout.state_schemas.size());
          layout.state_schemas.push_back({
              .state_size = props.state_size,
              .state_align = props.state_align,
              .needs_4state_init = props.needs_4state_init,
              .representative_process_index = i,
              .proc_within_body = nonfinal_proc_ordinal,
              .conn_index = std::nullopt,
          });
          module_schema_map[key] = schema_idx;
        }
      }
    }

    // Build per-body realization descriptors from the schema dedup map.
    // This is body-shaped data: one entry per unique body, not per instance.
    // Ordering: by first-seen body during schema dedup (deterministic
    // from BFS-sorted elaboration order). Cut 3 will consume this vector
    // in order when generating emitted constructor loops.
    {
      // Build body_realization_infos from module_plans (covers all bodies,
      // including those with zero non-final processes).
      // Keyed by body pointer (canonical identity).
      std::unordered_map<const mir::ModuleBody*, size_t> body_to_info_index;
      for (const auto& plan : module_plans) {
        auto [it, inserted] = body_to_info_index.try_emplace(
            plan.body, layout.body_realization_infos.size());
        if (inserted) {
          BodyStateSizeInfo size;
          BodyLayout body_layout;
          std::vector<SlotStorageSpec> body_slot_specs;
          if (plan.slot_count > 0) {
            auto bsl_it = body_storage_layouts.find(plan.body);
            if (bsl_it == body_storage_layouts.end()) {
              throw common::InternalError(
                  "BuildLayout", "no precomputed body storage layout");
            }
            size = ComputeBodyStateSize(plan.body_slots, bsl_it->second);
            body_layout = BuildBodyLayout(plan.body_slots, bsl_it->second);
            body_slot_specs = bsl_it->second.slot_specs;
          }
          layout.body_realization_infos.push_back(
              Layout::BodyRealizationInfo{
                  .body = plan.body,
                  .slot_count = plan.slot_count,
                  .body_layout = std::move(body_layout),
                  .slot_specs = std::move(body_slot_specs),
                  .slot_has_behavioral_trigger = {},
                  .slot_has_cross_body_behavioral_trigger = {},
                  .slot_has_connection_notification = {},
                  .inline_state_size_bytes = size.inline_bytes,
                  .appendix_state_size_bytes = size.appendix_bytes,
                  .total_state_size_bytes = size.total_bytes,
                  .time_unit_power = plan.time_unit_power,
                  .time_precision_power = plan.time_precision_power,
                  .num_installable_computations =
                      mir::GetInstallableComputationCount(*plan.body),
                  .port_entries =
                      [&] {
                        std::vector<runtime::RuntimePortEntry> rpe;
                        rpe.reserve(plan.body->port_entries.size());
                        for (const auto& pe : plan.body->port_entries) {
                          rpe.push_back(
                              runtime::RuntimePortEntry{
                                  .sym_value = pe.sym.value,
                                  .local_slot = pe.slot.value,
                                  .dir = static_cast<uint8_t>(pe.dir)});
                        }
                        return rpe;
                      }(),
                  .event_count =
                      static_cast<uint32_t>(plan.body->events.size()),
              });
          layout.body_runtime_descriptors.push_back({});
        } else {
          // Validate all plans for the same body agree on slot_count.
          auto& existing = layout.body_realization_infos[it->second];
          if (existing.slot_count != plan.slot_count) {
            throw common::InternalError(
                "BuildLayout",
                std::format(
                    "body group {} slot_count disagreement: {} vs {}",
                    it->second, existing.slot_count, plan.slot_count));
          }
        }
      }

      // Fill process schema indices ordered by dense non-final ordinal.
      // Invariant: every ordinal in [0, nonfinal_count) is present
      // exactly once and matches the canonical BodyProcessOrdinalMap.
      for (size_t bi = 0; bi < layout.body_realization_infos.size(); ++bi) {
        const auto& info = layout.body_realization_infos[bi];
        auto& rt = layout.body_runtime_descriptors[bi];
        const auto& ordinal_map = body_ordinal_maps.at(info.body);
        auto nonfinal_count =
            static_cast<uint32_t>(ordinal_map.nonfinal_processes.size());

        // process_schema_indices is indexed by dense non-final body-local
        // ordinal. Drive population from the canonical ordinal domain
        // directly rather than reconstructing density from emitted schema
        // keys.
        rt.process_schema_indices.resize(nonfinal_count);
        for (uint32_t nonfinal_proc_ordinal = 0;
             nonfinal_proc_ordinal < nonfinal_count; ++nonfinal_proc_ordinal) {
          SchemaKey key{
              .body = info.body,
              .proc_within_body = nonfinal_proc_ordinal,
          };
          auto it = module_schema_map.find(key);
          if (it == module_schema_map.end()) {
            throw common::InternalError(
                "BuildLayout",
                std::format(
                    "body group {} missing nonfinal_proc_ordinal {}", bi,
                    nonfinal_proc_ordinal));
          }
          rt.process_schema_indices[nonfinal_proc_ordinal] = it->second;
        }
      }
    }
  }

  // Compute num_package_slots: the first module instance's
  // design_state_base_slot.
  if (!module_plans.empty()) {
    layout.num_package_slots = module_plans[0].design_state_base_slot;
  } else {
    layout.num_package_slots =
        static_cast<uint32_t>(layout.design.slot_byte_offsets.size());
  }

  // Build per-instance body-group index.
  {
    std::unordered_map<const mir::ModuleBody*, uint32_t> body_to_group;
    for (uint32_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
      body_to_group[layout.body_realization_infos[gi].body] = gi;
    }
    layout.instance_body_groups.reserve(module_plans.size());
    for (const auto& plan : module_plans) {
      layout.instance_body_groups.push_back(body_to_group.at(plan.body));
    }
  }

  // Build per-body connection notification bitmaps directly from the
  // static contract sources: memcpy-style connection kernels and
  // installable-computation deps. Both are reactive consumers whose
  // writes must unconditionally mark dirty; both feed the same shared
  // reactive trigger model at runtime.
  for (auto& info : layout.body_realization_infos) {
    info.slot_has_connection_notification.assign(info.slot_count, false);
  }
  // Kernelized connections: use structured trigger identity directly.
  for (const auto& entry : layout.connection_kernel_entries) {
    auto gi = layout.instance_body_groups[entry.trigger_object_index.value];
    layout.body_realization_infos[gi]
        .slot_has_connection_notification[entry.trigger_local_slot.value] =
        true;
  }
  // Installable-computation deps: every parent-local slot that an IC
  // reads must be marked so codegen emits unconditional MarkDirty on
  // writes to it. Without this, writes from scheduled processes are
  // suppressed and the shared reactive model never sees the change.
  for (auto& info : layout.body_realization_infos) {
    if (info.body == nullptr) continue;
    for (const auto& ic : info.body->installable_computations) {
      for (const auto& dep : ic.deps) {
        if (dep.value >= info.slot_has_connection_notification.size()) {
          throw common::InternalError(
              "BuildLayout",
              std::format(
                  "installable computation dep slot {} out of range "
                  "(slot_count={})",
                  dep.value, info.slot_has_connection_notification.size()));
        }
        info.slot_has_connection_notification[dep.value] = true;
      }
    }
  }
  // Compute per-instance storage bases and slot counts.
  // The storage base is the first owned-local slot's arena-absolute byte
  // offset. Forwarded aliases are skipped. Instances with no owned-local
  // slots have nullopt (no local storage region).
  {
    size_t num_instances = module_plans.size();
    layout.instance_storage_bases.resize(num_instances);
    layout.instance_slot_counts.resize(num_instances);
    layout.instance_storage_sizes.resize(num_instances);

    for (size_t mi = 0; mi < num_instances; ++mi) {
      const auto& plan = module_plans[mi];
      layout.instance_slot_counts[mi] = plan.slot_count;

      InstanceStorageBase base;
      if (plan.slot_count > 0) {
        uint32_t row = plan.design_state_base_slot;
        base.abs_byte_offset =
            layout.design.slot_storage_bindings[row].abs_byte_offset;
      }
      layout.instance_storage_bases[mi] = base;

      // Per-instance storage sizes from body-local layout (canonical source).
      // All instances of the same body share the same layout, so we read
      // from the body realization info computed by ComputeBodyStateSize.
      auto gi = layout.instance_body_groups[mi];
      const auto& body_info = layout.body_realization_infos[gi];
      layout.instance_storage_sizes[mi] = {
          .inline_bytes = body_info.inline_state_size_bytes,
          .appendix_bytes = body_info.appendix_state_size_bytes};
    }
  }

  return layout;
}

}  // namespace lyra::lowering::mir_to_llvm
