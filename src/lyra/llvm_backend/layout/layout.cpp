#include "lyra/llvm_backend/layout/layout.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
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

    case TypeKind::kVoid:
      throw common::InternalError(
          "GetLlvmTypeForTypeId",
          std::format(
              "unsupported type kind: {}", static_cast<int>(type.Kind())));
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
            std::is_same_v<T, mir::SelectRvalueInfo>) {
          // These RvalueInfo types have no embedded PlaceIds or Operands
          // beyond what's in Rvalue::operands (already collected above)
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
              [&](const mir::Assign& a) {
                places.insert(a.dest);
                CollectPlacesFromRhs(a.rhs, places);
              },
              [&](const mir::GuardedAssign& ga) {
                places.insert(ga.dest);
                CollectPlacesFromRhs(ga.rhs, places);
                CollectPlaceFromOperand(ga.guard, places);
              },
              [&](const mir::Effect& e) {
                CollectPlacesFromEffectOp(e.op, places);
              },
              [&](const mir::DeferredAssign& da) {
                places.insert(da.dest);
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
        place.root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
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
           std::holds_alternative<mir::Wait>(block.terminator.data);
  });
}

// Check if a statement is "pure" for comb kernel batching.
// Pure means: no I/O, no NBA, no function calls, no container mutations.
auto IsStatementPure(const mir::Statement& stmt) -> bool {
  return std::visit(
      common::Overloaded{
          [](const mir::Assign&) { return true; },
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

// Build ProcessStateHeader with realized instance binding.
// Field order and count must match ProcessFrameHeader and
// ProcessFrameHeaderField in process_frame.hpp.
auto BuildHeaderType(llvm::LLVMContext& ctx, llvm::StructType* suspend_type)
    -> llvm::StructType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* outcome_ty =
      llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty, i32_ty});
  // { suspend, body, engine_ptr, design_ptr, instance, outcome, process_id }
  using F = lyra::runtime::ProcessFrameHeaderField;
  constexpr auto kFieldCount = static_cast<size_t>(F::kFieldCount);
  std::array<llvm::Type*, kFieldCount> fields = {
      suspend_type, ptr_ty, ptr_ty, ptr_ty, ptr_ty, outcome_ty, i32_ty};
  return llvm::StructType::create(ctx, fields, "ProcessStateHeader");
}

// Build RuntimeInstanceStorage LLVM struct type.
// Field order must match RuntimeInstanceStorage and
// RuntimeInstanceStorageField in runtime_instance.hpp.
auto BuildRuntimeInstanceStorageType(llvm::LLVMContext& ctx)
    -> llvm::StructType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  // { inline_base, inline_size, appendix_base, appendix_size }
  using F = lyra::runtime::RuntimeInstanceStorageField;
  constexpr auto kFieldCount = static_cast<size_t>(F::kFieldCount);
  std::array<llvm::Type*, kFieldCount> fields = {
      ptr_ty, i64_ty, ptr_ty, i64_ty};
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
  // { instance_id, body, storage, path_c_str, owner_ordinal,
  //   module_proc_base, num_module_processes }
  using F = lyra::runtime::RuntimeInstanceField;
  constexpr auto kFieldCount = static_cast<size_t>(F::kFieldCount);
  std::array<llvm::Type*, kFieldCount> fields = {
      i32_ty, ptr_ty, storage_type, ptr_ty, i32_ty, i32_ty, i32_ty};
  return llvm::StructType::create(ctx, fields, "RuntimeInstance");
}

}  // namespace

auto ResolveObservation(
    const mir::Arena& arena, const DesignLayout& design_layout,
    common::SlotId design_global_slot, mir::PlaceId place_id)
    -> std::optional<ResolvedObservation> {
  auto slot_it = design_layout.slot_to_index.find(design_global_slot);
  if (slot_it == design_layout.slot_to_index.end()) {
    throw common::InternalError(
        "ResolveObservation", std::format(
                                  "design-global slot {} not found in layout",
                                  design_global_slot.value));
  }
  const auto& spec = design_layout.slot_storage_specs[slot_it->second];
  const auto& place = arena[place_id];
  auto range =
      ResolveByteRange(spec, design_layout.storage_spec_arena, place, nullptr);
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

      if (const auto* assign = std::get_if<mir::Assign>(&stmt.data)) {
        if (const auto* rv = std::get_if<mir::Rvalue>(&assign->rhs)) {
          if (mir::RvalueHasSideEffects(rv->info)) return std::nullopt;
        }
        const auto& root = arena[assign->dest].root;
        if (root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
          write_slots.insert(
              {.scope = mir::SignalRef::Scope::kModuleLocal,
               .id = static_cast<uint32_t>(root.id)});
        } else if (root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
          write_slots.insert(
              {.scope = mir::SignalRef::Scope::kDesignGlobal,
               .id = static_cast<uint32_t>(root.id)});
        }
      }
      if (const auto* ga = std::get_if<mir::GuardedAssign>(&stmt.data)) {
        if (const auto* rv = std::get_if<mir::Rvalue>(&ga->rhs)) {
          if (mir::RvalueHasSideEffects(rv->info)) return std::nullopt;
        }
        const auto& root = arena[ga->dest].root;
        if (root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
          write_slots.insert(
              {.scope = mir::SignalRef::Scope::kModuleLocal,
               .id = static_cast<uint32_t>(root.id)});
        } else if (root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
          write_slots.insert(
              {.scope = mir::SignalRef::Scope::kDesignGlobal,
               .id = static_cast<uint32_t>(root.id)});
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
    const std::vector<SlotInfo>& slots, const TypeArena& types,
    const llvm::DataLayout& dl, bool force_two_state,
    uint32_t num_package_slots,
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

  // Resolve storage spec for each slot. Every slot owns its own storage.
  for (size_t i = 0; i < slots.size(); ++i) {
    const auto& slot = slots[i];
    layout.slots.push_back(slot.slot_id);
    layout.slot_to_index[slot.slot_id] = static_cast<uint32_t>(i);
    layout.slot_storage_specs.push_back(ResolveStorageSpec(
        slot.type_id, types, storage_mode, target_abi,
        layout.storage_spec_arena));
  }

  // Initialize owned data offset vector (nullopt for all slots initially).
  layout.owned_data_offsets.resize(slots.size(), std::nullopt);

  // Byte offset assignment: inline slots + owned-container appendix.
  //
  // When instance_ranges is provided (final layout), each instance's
  // appendix backing is placed contiguously after its inline slots so
  // that owned-container offsets are body-relative and repeatable.
  // Layout order: [pkg inline + pkg appendix][inst0 inline + inst0 appendix]...
  //
  // When instance_ranges is empty (preliminary layout), all slots
  // are laid out sequentially with appendix at the end.
  layout.slot_byte_offsets.resize(slots.size(), 0);

  uint64_t max_align = 1;

  // Helper: assign inline byte offsets for a contiguous slot range,
  // then assign appendix offsets for owned containers in that range.
  // Every slot in the range gets storage (no forwarding skip).
  auto assign_group_offsets = [&](uint32_t begin, uint32_t end,
                                  uint64_t group_start) -> uint64_t {
    uint64_t offset = group_start;

    // Inline pass: all slots.
    for (uint32_t i = begin; i < end; ++i) {
      uint64_t slot_size = 0;
      uint64_t slot_align = 0;
      if (slots[i].storage_shape == mir::StorageShape::kOwnedContainer) {
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
      if (slots[i].storage_shape != mir::StorageShape::kOwnedContainer) {
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
    // Preliminary layout: all slots in one group, appendix at end.
    auto total = static_cast<uint32_t>(slots.size());
    uint64_t end_offset = assign_group_offsets(0, total, 0);
    layout.inline_region_size = end_offset;
    layout.arena_size = std::max(uint64_t{1}, end_offset);
  } else {
    uint64_t running = 0;

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
  for (size_t i = 0; i < slots.size(); ++i) {
    bool is_container =
        slots[i].storage_shape == mir::StorageShape::kOwnedContainer;
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
  layout.slot_storage_bindings.reserve(slots.size());
  for (size_t i = 0; i < slots.size(); ++i) {
    layout.slot_storage_bindings.push_back(
        OwnedLocalStorage{ArenaByteOffset{layout.slot_byte_offsets[i]}});
  }

  return layout;
}

auto BuildBodyStorageLayout(
    const mir::ModuleBody& body, const TypeArena& types,
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
  result.slot_specs.reserve(body.slots.size());
  for (const auto& slot : body.slots) {
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

// DesignLayout method implementations.

auto DesignLayout::ContainsSlot(common::SlotId slot_id) const -> bool {
  return slot_to_index.contains(slot_id);
}

auto DesignLayout::GetStorageByteOffset(common::SlotId slot_id) const
    -> uint64_t {
  auto it = slot_to_index.find(slot_id);
  if (it == slot_to_index.end()) {
    throw common::InternalError(
        "DesignLayout::GetStorageByteOffset",
        std::format("slot {} not in layout", slot_id.value));
  }
  return slot_byte_offsets[it->second];
}

auto DesignLayout::GetStorageSpec(common::SlotId slot_id) const
    -> const SlotStorageSpec& {
  auto it = slot_to_index.find(slot_id);
  if (it == slot_to_index.end()) {
    throw common::InternalError(
        "DesignLayout::GetStorageSpec",
        std::format("slot {} not in layout", slot_id.value));
  }
  return slot_storage_specs[it->second];
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

auto DesignLayout::GetSlotRow(common::SlotId slot_id) const -> uint32_t {
  auto it = slot_to_index.find(slot_id);
  if (it == slot_to_index.end()) {
    throw common::InternalError(
        "DesignLayout::GetSlotRow",
        std::format("slot {} not in layout", slot_id.value));
  }
  return it->second;
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

// Build FrameLayout from de-duplicated roots.
// frame_name is the semantic type name (e.g., "Body0Frame0", "Conn3Frame").
auto BuildFrameLayout(
    const std::vector<RootInfo>& roots, const TypeArena& types,
    llvm::LLVMContext& ctx, std::string_view frame_name,
    const llvm::DataLayout& dl, bool force_two_state) -> FrameLayout {
  FrameLayout layout;
  std::vector<llvm::Type*> field_types;

  for (size_t i = 0; i < roots.size(); ++i) {
    const auto& root = roots[i];
    layout.root_types.push_back(root.type);
    layout.root_to_field[root.key] = static_cast<uint32_t>(i);
    field_types.push_back(
        GetLlvmTypeForTypeId(ctx, root.type, types, force_two_state));
  }

  if (field_types.empty()) {
    layout.llvm_type = llvm::StructType::create(
        ctx, {llvm::Type::getInt8Ty(ctx)}, std::string(frame_name));
  } else {
    layout.llvm_type =
        llvm::StructType::create(ctx, field_types, std::string(frame_name));
  }

  // Collect 4-state patches for scalar patchable fields
  if (!roots.empty()) {
    CollectPatches(
        layout.four_state_patches, layout.llvm_type, dl, types,
        [&](size_t i) { return roots[i].type; }, roots.size(), force_two_state);
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

auto BuildSlotInfo(
    std::span<const mir::SlotDesc> slots_in, const TypeArena& types,
    bool force_two_state) -> std::vector<SlotInfo> {
  std::vector<SlotInfo> slots;
  slots.reserve(slots_in.size());

  for (size_t i = 0; i < slots_in.size(); ++i) {
    TypeId type_id = slots_in[i].type;
    const Type& type = types[type_id];

    SlotTypeInfo type_info{};
    if (type.Kind() == TypeKind::kReal) {
      type_info = {
          .kind = VarTypeKind::kReal,
          .width = 64,
          .is_signed = true,
          .is_four_state = false,
      };
    } else if (type.Kind() == TypeKind::kString) {
      type_info = {
          .kind = VarTypeKind::kString,
          .width = 0,
          .is_signed = false,
          .is_four_state = false,
      };
    } else if (IsPacked(type)) {
      uint32_t width = PackedBitWidth(type, types);
      bool is_signed = IsPackedSigned(type, types);
      bool is_four_state =
          IsLayoutPackedFourState(type, types, force_two_state);
      type_info = {
          .kind = VarTypeKind::kIntegral,
          .width = width > 0 ? width : 32,
          .is_signed = is_signed,
          .is_four_state = is_four_state,
      };
    } else {
      // Unsupported type - use placeholder for SlotTypeInfo
      // The actual TypeId is preserved for LLVM type derivation
      type_info = {
          .kind = VarTypeKind::kIntegral,
          .width = 32,
          .is_signed = false,
          .is_four_state = false,
      };
    }

    slots.push_back(
        SlotInfo{
            .slot_id = common::SlotId{static_cast<uint32_t>(i)},
            .type_id = type_id,
            .type_info = type_info,
            .storage_shape = slots_in[i].storage_shape,
        });
  }

  return slots;
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

auto ResolveInstanceOwnedFlatSlot(const Layout& layout, uint32_t flat_slot_id)
    -> SlotOwnerInfo {
  uint32_t running_base = layout.num_package_slots;
  for (uint32_t mi = 0; mi < layout.instance_slot_counts.size(); ++mi) {
    uint32_t count = layout.instance_slot_counts[mi];
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
          flat_slot_id, layout.num_package_slots,
          layout.instance_slot_counts.size()));
}

auto BuildLayout(
    std::span<const mir::ProcessId> init_processes,
    std::vector<ConnectionKernelEntry> precollected_connection_kernels,
    std::vector<mir::ProcessId> non_kernelized_connection_processes,
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena, const TypeArena& types,
    DesignLayout design_layout,
    const std::unordered_map<uint32_t, BodyStorageLayout>& body_storage_layouts,
    const std::vector<common::BodyTimeScale>* body_timescales,
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

  // Non-kernelized connection processes: extract trigger slots from their
  // Wait terminators and add to the connection trigger bitmap. These
  // processes failed TryKernelizeConnection (e.g., port expression
  // connections) and are not in connection_kernel_entries.
  for (mir::ProcessId proc_id : non_kernelized_connection_processes) {
    layout.scheduled_processes.push_back({proc_id, ModuleIndex{}});
    const auto& process = design_arena[proc_id];
    for (const auto& block : process.blocks) {
      const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
      if (wait == nullptr) continue;
      for (const auto& trigger : wait->triggers) {
        if (trigger.signal.scope != mir::SignalRef::Scope::kDesignGlobal) {
          throw common::InternalError(
              "BuildLayout", std::format(
                                 "non-kernelized connection process {} has "
                                 "trigger with non-design-global scope (id={})",
                                 proc_id.value, trigger.signal.id));
        }
        auto slot = static_cast<uint32_t>(trigger.signal.id);
        if (slot >= layout.slot_has_connection_trigger.size()) {
          throw common::InternalError(
              "BuildLayout",
              std::format(
                  "non-kernelized connection trigger slot {} out of "
                  "range (design has {} slots)",
                  slot, layout.slot_has_connection_trigger.size()));
        }
        layout.slot_has_connection_trigger[slot] = true;
      }
    }
  }

  // Phase 3: Collect module processes (run through scheduler)
  layout.num_module_process_base = layout.scheduled_processes.size();
  for (uint32_t mi = 0; mi < module_plans.size(); ++mi) {
    const auto& plan = module_plans[mi];
    const auto& body_arena = design.module_bodies.at(plan.body_id.value).arena;
    for (mir::ProcessId proc_id : plan.body_processes) {
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
      return design.module_bodies
          .at(module_plans[sp.module_index.value].body_id.value)
          .arena;
    }
    return design_arena;
  };

  layout.processes.reserve(layout.scheduled_processes.size());

  // Build canonical non-final ordinal maps per body. These are the single
  // source of truth for mapping ProcessId <-> dense non-final ordinal.
  absl::flat_hash_map<mir::ModuleBodyId, BodyProcessOrdinalMap>
      body_ordinal_maps;
  for (const auto& plan : module_plans) {
    if (body_ordinal_maps.contains(plan.body_id)) continue;
    const auto& body = design.module_bodies.at(plan.body_id.value);
    body_ordinal_maps.emplace(plan.body_id, BuildBodyProcessOrdinalMap(body));
  }

  // Counters for semantic naming per category.
  uint32_t init_counter = 0;
  uint32_t conn_counter = 0;

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
    } else if (i < layout.num_module_process_base) {
      frame_name = std::format("Conn{}Frame", conn_counter);
      state_name = std::format("Conn{}State", conn_counter);
      ++conn_counter;
    } else {
      uint32_t mi = sp.module_index.value;
      auto body_id = module_plans[mi].body_id;
      const auto& ordinal_map = body_ordinal_maps.at(body_id);
      uint32_t nonfinal_proc_ordinal =
          GetNonFinalOrdinal(ordinal_map, sp.process_id);
      frame_name =
          std::format("Body{}Frame{}", body_id.value, nonfinal_proc_ordinal);
      state_name =
          std::format("Body{}State{}", body_id.value, nonfinal_proc_ordinal);
    }

    ProcessLayout proc_layout;
    proc_layout.process_index = i;
    proc_layout.has_suspension = ProcessHasSuspension(process);

    auto roots = CollectProcessRoots(process, sched_arena, types);

    if (proc_layout.has_suspension) {
      proc_layout.frame =
          BuildFrameLayout(roots, types, ctx, frame_name, dl, force_two_state);
    } else {
      // Suspension-free: empty frame, roots become allocas at codegen time.
      proc_layout.frame =
          BuildFrameLayout({}, types, ctx, frame_name, dl, force_two_state);
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
        for (TypeId type_id : proc_layout.frame.root_types) {
          if (IsLayoutFourState(type_id, types, force_two_state) &&
              !IsScalarPatchable(type_id, types, force_two_state)) {
            needs = true;
            break;
          }
        }
      }
      return {.state_size = sz, .state_align = al, .needs_4state_init = needs};
    };

    // Map from (body_id, dense non-final ordinal) to schema index for
    // module process deduplication.
    struct SchemaKey {
      uint32_t body_id;
      uint32_t proc_within_body;  // Dense non-final body-local ordinal.
      auto operator==(const SchemaKey&) const -> bool = default;
    };
    struct SchemaKeyHash {
      auto operator()(const SchemaKey& k) const -> size_t {
        return std::hash<uint64_t>{}(
            (static_cast<uint64_t>(k.body_id) << 32) | k.proc_within_body);
      }
    };
    std::unordered_map<SchemaKey, uint32_t, SchemaKeyHash> module_schema_map;

    conn_counter = 0;

    for (size_t i = layout.num_init_processes;
         i < layout.scheduled_processes.size(); ++i) {
      const auto& sp = layout.scheduled_processes[i];
      auto props = compute_schema_props(layout.processes[i]);

      if (i < layout.num_module_process_base) {
        // Connection process: unique schema per process.
        layout.state_schemas.push_back({
            .state_size = props.state_size,
            .state_align = props.state_align,
            .needs_4state_init = props.needs_4state_init,
            .representative_process_index = i,
            .body_id = std::nullopt,
            .proc_within_body = std::nullopt,
            .conn_index = conn_counter,
        });

        ++conn_counter;
      } else {
        // Module process: deduplicate by (body_id, nonfinal_proc_ordinal).
        uint32_t mi = sp.module_index.value;
        auto body_id = module_plans[mi].body_id;
        const auto& ordinal_map = body_ordinal_maps.at(body_id);
        uint32_t nonfinal_proc_ordinal =
            GetNonFinalOrdinal(ordinal_map, sp.process_id);

        SchemaKey key{
            .body_id = body_id.value,
            .proc_within_body = nonfinal_proc_ordinal};
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
          if (existing.body_id != body_id ||
              existing.proc_within_body != nonfinal_proc_ordinal ||
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
          auto rep_body_id = module_plans[rep_mi].body_id;
          const auto& rep_ordinal_map = body_ordinal_maps.at(rep_body_id);
          uint32_t rep_nonfinal_proc_ordinal =
              GetNonFinalOrdinal(rep_ordinal_map, rep_sp.process_id);
          if (rep_body_id != body_id ||
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
              .body_id = body_id,
              .proc_within_body = nonfinal_proc_ordinal,
              .conn_index = std::nullopt,
          });
          module_schema_map[key] = schema_idx;
        }
      }
    }

    // Build per-body realization descriptors from the schema dedup map.
    // This is body-shaped data: one entry per unique body, not per instance.
    // Ordering: by first-seen body_id during schema dedup (deterministic
    // from BFS-sorted elaboration order). Cut 3 will consume this vector
    // in order when generating emitted constructor loops.
    {
      // Build body_realization_infos from module_plans (covers all bodies,
      // including those with zero non-final processes).
      std::unordered_map<uint32_t, size_t> body_id_to_info_index;
      for (const auto& plan : module_plans) {
        auto body_id_val = plan.body_id.value;
        auto [it, inserted] = body_id_to_info_index.try_emplace(
            body_id_val, layout.body_realization_infos.size());
        if (inserted) {
          BodyStateSizeInfo size;
          if (plan.slot_count > 0) {
            const auto& body = design.module_bodies.at(body_id_val);
            auto bsl_it = body_storage_layouts.find(body_id_val);
            if (bsl_it == body_storage_layouts.end()) {
              throw common::InternalError(
                  "BuildLayout",
                  std::format(
                      "no precomputed body storage layout for body {}",
                      body_id_val));
            }
            size = ComputeBodyStateSize(std::span(body.slots), bsl_it->second);
          }
          if (body_id_val >= body_timescales->size()) {
            throw common::InternalError(
                "BuildLayout",
                std::format("body {} missing timescale entry", body_id_val));
          }
          const auto& ts = body_timescales->at(body_id_val);
          if (ts.body_id != body_id_val) {
            throw common::InternalError(
                "BuildLayout",
                std::format(
                    "body {} timescale entry has mismatched body_id {}",
                    body_id_val, ts.body_id));
          }
          layout.body_realization_infos.push_back(
              Layout::BodyRealizationInfo{
                  .body_id = plan.body_id,
                  .slot_count = plan.slot_count,
                  .process_schema_indices = {},
                  .meta = {},
                  .triggers = {},
                  .comb = {},
                  .observable_descriptors = {},
                  .init = {},
                  .slot_has_behavioral_trigger = {},
                  .inline_state_size_bytes = size.inline_bytes,
                  .appendix_state_size_bytes = size.appendix_bytes,
                  .total_state_size_bytes = size.total_bytes,
                  .decision_metas = {},
                  .decision_meta_files = {},
                  .time_unit_power = ts.unit_power,
                  .time_precision_power = ts.precision_power,
              });
        } else {
          // Validate all plans for the same body agree on slot_count.
          auto& existing = layout.body_realization_infos[it->second];
          if (existing.slot_count != plan.slot_count) {
            throw common::InternalError(
                "BuildLayout",
                std::format(
                    "body {} slot_count disagreement: {} vs {}", body_id_val,
                    existing.slot_count, plan.slot_count));
          }
        }
      }

      // Fill process schema indices ordered by dense non-final ordinal.
      // Invariant: every ordinal in [0, nonfinal_count) is present
      // exactly once and matches the canonical BodyProcessOrdinalMap.
      for (auto& info : layout.body_realization_infos) {
        const auto& ordinal_map = body_ordinal_maps.at(info.body_id);
        auto nonfinal_count =
            static_cast<uint32_t>(ordinal_map.nonfinal_processes.size());

        // process_schema_indices is indexed by dense non-final body-local
        // ordinal. Drive population from the canonical ordinal domain
        // directly rather than reconstructing density from emitted schema
        // keys.
        info.process_schema_indices.resize(nonfinal_count);
        for (uint32_t nonfinal_proc_ordinal = 0;
             nonfinal_proc_ordinal < nonfinal_count; ++nonfinal_proc_ordinal) {
          SchemaKey key{
              .body_id = info.body_id.value,
              .proc_within_body = nonfinal_proc_ordinal,
          };
          auto it = module_schema_map.find(key);
          if (it == module_schema_map.end()) {
            throw common::InternalError(
                "BuildLayout", std::format(
                                   "body {} missing nonfinal_proc_ordinal {}",
                                   info.body_id.value, nonfinal_proc_ordinal));
          }
          info.process_schema_indices[nonfinal_proc_ordinal] = it->second;
        }
      }
    }

    // Build per-connection realization descriptors from connection schemas.
    // Connection schemas are the first conn_counter entries in state_schemas
    // (each connection gets a unique schema, added in order).
    for (uint32_t c = 0; c < conn_counter; ++c) {
      // Connection schemas are at the start of state_schemas, one per
      // connection process in scheduling order. Verify identity.
      const auto& schema = layout.state_schemas[c];
      if (schema.conn_index != c) {
        throw common::InternalError(
            "BuildLayout",
            "connection schema ordering does not match expected conn_index");
      }
      auto conn_sched_idx = layout.num_init_processes + c;
      layout.connection_realization_infos.push_back(
          Layout::ConnectionRealizationInfo{
              .schema_index = c,
              .process_id =
                  layout.scheduled_processes[conn_sched_idx].process_id,
              .trigger = std::nullopt,
          });
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

      // Compute per-instance realized inline and appendix sizes from the
      // design layout. These are the correct per-instance sizes accounting
      // for concrete parameterized types.
      if (base.abs_byte_offset.has_value()) {
        uint64_t inst_base = base.abs_byte_offset->value;
        uint64_t inline_end = inst_base;
        uint64_t total_end = inst_base;

        // Find the end of inline region: last owned inline slot + its size.
        // Find the end of appendix region: last owned backing + its size.
        for (uint32_t i = 0; i < plan.slot_count; ++i) {
          uint32_t row = plan.design_state_base_slot + i;
          uint64_t slot_off = layout.design.slot_byte_offsets[row];
          bool has_backing = layout.design.owned_data_offsets[row].has_value();
          uint64_t slot_size =
              has_backing
                  ? runtime::kOwnedStorageHandleByteSize
                  : layout.design.slot_storage_specs[row].TotalByteSize();
          uint64_t slot_end = slot_off + slot_size;
          inline_end = std::max(inline_end, slot_end);

          // Check for owned backing data (container appendix).
          if (layout.design.owned_data_offsets[row].has_value()) {
            uint64_t backing_off = *layout.design.owned_data_offsets[row];
            uint64_t backing_size =
                layout.design.slot_storage_specs[row].TotalByteSize();
            uint64_t backing_end = backing_off + backing_size;
            total_end = std::max(total_end, backing_end);
          }
        }

        uint64_t inline_bytes = inline_end - inst_base;
        uint64_t appendix_bytes =
            (total_end > inline_end) ? (total_end - inline_end) : 0;
        layout.instance_storage_sizes[mi] = {
            .inline_bytes = inline_bytes, .appendix_bytes = appendix_bytes};
      }
    }
  }

  return layout;
}

}  // namespace lyra::lowering::mir_to_llvm
