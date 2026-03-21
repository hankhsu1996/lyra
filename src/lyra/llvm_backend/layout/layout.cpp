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

#include "lyra/common/constant.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/layout/layout_four_state.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"
#include "lyra/runtime/owned_storage_handle.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::lowering::mir_to_llvm {

// Get the LLVM storage type for an integral type, rounding up to power-of-2.
auto GetLlvmStorageType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type* {
  if (bit_width <= 8) {
    return llvm::Type::getInt8Ty(ctx);
  }
  if (bit_width <= 16) {
    return llvm::Type::getInt16Ty(ctx);
  }
  if (bit_width <= 32) {
    return llvm::Type::getInt32Ty(ctx);
  }
  if (bit_width <= 64) {
    return llvm::Type::getInt64Ty(ctx);
  }
  return llvm::Type::getIntNTy(ctx, bit_width);
}

// Get the LLVM struct type for a 4-state value: {iN_storage, iN_storage}
auto GetFourStateStructType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::StructType* {
  auto* elem = GetLlvmStorageType(ctx, bit_width);
  return llvm::StructType::get(ctx, {elem, elem});
}

auto GetLlvmAbiTypeForValue(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types,
    bool force_two_state) -> llvm::Type* {
  const auto& type = types[type_id];

  // Handle types (passed as ptr handle value)
  if (type.Kind() == TypeKind::kString ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue ||
      type.Kind() == TypeKind::kAssociativeArray) {
    return llvm::PointerType::getUnqual(ctx);
  }

  // Floating point
  if (type.Kind() == TypeKind::kReal) {
    return llvm::Type::getDoubleTy(ctx);
  }
  if (type.Kind() == TypeKind::kShortReal) {
    return llvm::Type::getFloatTy(ctx);
  }

  // Packed integrals (2-state or 4-state)
  if (IsPacked(type)) {
    uint32_t width = PackedBitWidth(type, types);
    if (IsLayoutPackedFourState(type, types, force_two_state)) {
      return GetFourStateStructType(ctx, width);
    }
    return GetLlvmStorageType(ctx, width);
  }

  // Aggregates (unpacked array/struct/union) - not passable by value
  // Return nullptr; caller must handle via out-param or error
  if (type.Kind() == TypeKind::kUnpackedArray ||
      type.Kind() == TypeKind::kUnpackedStruct ||
      type.Kind() == TypeKind::kUnpackedUnion) {
    return nullptr;
  }

  // Void - cannot be passed by value (throw, don't return nullptr)
  if (type.Kind() == TypeKind::kVoid) {
    throw common::InternalError(
        "GetLlvmAbiTypeForValue",
        std::format("void type {} cannot be passed by value", type_id.value));
  }

  // Unsupported type kind (throw, don't return nullptr)
  throw common::InternalError(
      "GetLlvmAbiTypeForValue",
      std::format(
          "unsupported type kind {} for type {}", static_cast<int>(type.Kind()),
          type_id.value));
}

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
        return GetFourStateStructType(ctx, bit_width);
      }
      return GetLlvmStorageType(ctx, bit_width);
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
        return GetFourStateStructType(ctx, width);
      }
      return GetLlvmStorageType(ctx, width);
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
            std::is_same_v<T, mir::ArrayQueryRvalueInfo>) {
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
          [&](const mir::SeverityEffect& s) {
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
      },
      effect);
}

// Collect all PlaceIds referenced in a process
auto CollectProcessPlaces(const mir::Process& process)
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
            [&](const mir::QualifiedDispatch& qd) {
              for (const auto& cond : qd.conditions) {
                CollectPlaceFromOperand(cond, places);
              }
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
  auto all_places = CollectProcessPlaces(process);

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

// Check if a connection process can be kernelized (single Assign + Wait with
// 1 trigger, source is a design slot read, dest is a design slot write).
// File-local intermediates for kernelization.
// These carry raw PlaceIds from the owning arena. BuildLayout converts them
// into final boundary structs with ResolvedObservation before they escape.

struct PendingConnectionKernelEntry {
  mir::ProcessId process_id;
  mir::SlotId src_slot;
  mir::SlotId dst_slot;
  mir::SlotId trigger_slot;
  common::EdgeKind trigger_edge = common::EdgeKind::kAnyChange;
  std::optional<mir::PlaceId> trigger_observed_place;
};

struct PendingCombTrigger {
  mir::SlotId slot;
  std::optional<mir::PlaceId> observed_place;
};

struct PendingCombKernelEntry {
  std::vector<PendingCombTrigger> triggers;
  bool has_self_edge = false;
};

auto ResolveObservation(
    const mir::Arena& arena, const DesignLayout& design_layout,
    mir::SlotId design_global_slot, mir::PlaceId place_id)
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

auto TryKernelizeConnection(
    const mir::Process& process, const mir::Arena& arena)
    -> std::optional<PendingConnectionKernelEntry> {
  // Must have exactly 1 basic block
  if (process.blocks.size() != 1) {
    return std::nullopt;
  }
  const auto& block = process.blocks[0];

  // Must have exactly 1 statement (the Assign)
  if (block.statements.size() != 1) {
    return std::nullopt;
  }

  // Statement must be an Assign
  const auto* assign = std::get_if<mir::Assign>(&block.statements[0].data);
  if (assign == nullptr) {
    return std::nullopt;
  }

  // Dest must be a design slot (PlaceRoot::kDesign, no projections)
  const auto& dest_place = arena[assign->dest];
  if (dest_place.root.kind != mir::PlaceRoot::Kind::kDesignGlobal) {
    return std::nullopt;
  }
  if (!dest_place.projections.empty()) {
    return std::nullopt;
  }

  // RHS must be an Operand (not Rvalue)
  const auto* rhs_operand = std::get_if<mir::Operand>(&assign->rhs);
  if (rhs_operand == nullptr) {
    return std::nullopt;
  }

  // RHS operand must be a Use (place read)
  if (rhs_operand->kind != mir::Operand::Kind::kUse) {
    return std::nullopt;
  }

  // Source must be a design slot (no projections)
  auto src_place_id = std::get<mir::PlaceId>(rhs_operand->payload);
  const auto& src_place = arena[src_place_id];
  if (src_place.root.kind != mir::PlaceRoot::Kind::kDesignGlobal) {
    return std::nullopt;
  }
  if (!src_place.projections.empty()) {
    return std::nullopt;
  }

  // Terminator must be a Wait with exactly 1 trigger
  const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
  if (wait == nullptr) {
    return std::nullopt;
  }
  if (wait->triggers.size() != 1) {
    return std::nullopt;
  }

  const auto& trigger = wait->triggers[0];

  // No late-bound triggers
  if (trigger.late_bound.has_value()) {
    return std::nullopt;
  }

  // Connection processes operate on design-global storage only.
  if (trigger.signal.scope != mir::SignalRef::Scope::kDesignGlobal) {
    throw common::InternalError(
        "TryKernelizeConnection", "connection trigger must be design-global");
  }

  return PendingConnectionKernelEntry{
      .process_id = {},  // Set by caller
      .src_slot = mir::SlotId{static_cast<uint32_t>(src_place.root.id)},
      .dst_slot = mir::SlotId{static_cast<uint32_t>(dest_place.root.id)},
      .trigger_slot = mir::SlotId{trigger.signal.id},
      .trigger_edge = trigger.edge,
      .trigger_observed_place = trigger.observed_place,
  };
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
          [](const mir::QualifiedDispatch&) { return true; },
          [](const mir::Wait&) { return true; },
          [](const mir::Delay&) { return false; },
          [](const mir::Return&) { return false; },
          [](const mir::Finish&) { return false; },
          [](const mir::Repeat&) { return false; },
      },
      term.data);
}

// Centralized helper: resolve a SignalRef to design-global slot ID.
auto ResolveSignalToGlobalSlot(const mir::SignalRef& signal, uint32_t slot_base)
    -> uint32_t {
  return (signal.scope == mir::SignalRef::Scope::kModuleLocal)
             ? slot_base + signal.id
             : signal.id;
}

// Check if a process can be batched as a comb kernel.
// Criteria: kLooping, all statements pure, all terminators are pure CF or Wait,
// exactly one Wait terminator with kAnyChange triggers and no late-bound.
auto TryKernelizeComb(
    const mir::Process& process, const mir::Arena& arena, uint32_t slot_base)
    -> std::optional<PendingCombKernelEntry> {
  if (process.kind != mir::ProcessKind::kLooping) return std::nullopt;

  const mir::Wait* wait_term = nullptr;

  // Collect write destination slot IDs for self-edge detection.
  std::unordered_set<uint32_t> write_slots;

  for (const auto& block : process.blocks) {
    // Check all statements are pure
    for (const auto& stmt : block.statements) {
      if (!IsStatementPure(stmt)) return std::nullopt;

      // Check Assign RHS for side-effecting Rvalues, and collect write slots.
      if (const auto* assign = std::get_if<mir::Assign>(&stmt.data)) {
        if (const auto* rv = std::get_if<mir::Rvalue>(&assign->rhs)) {
          if (mir::RvalueHasSideEffects(rv->info)) return std::nullopt;
        }
        const auto& root = arena[assign->dest].root;
        if (root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
          write_slots.insert(slot_base + static_cast<uint32_t>(root.id));
        } else if (root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
          write_slots.insert(static_cast<uint32_t>(root.id));
        }
      }
      if (const auto* ga = std::get_if<mir::GuardedAssign>(&stmt.data)) {
        if (const auto* rv = std::get_if<mir::Rvalue>(&ga->rhs)) {
          if (mir::RvalueHasSideEffects(rv->info)) return std::nullopt;
        }
        const auto& root = arena[ga->dest].root;
        if (root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
          write_slots.insert(slot_base + static_cast<uint32_t>(root.id));
        } else if (root.kind == mir::PlaceRoot::Kind::kDesignGlobal) {
          write_slots.insert(static_cast<uint32_t>(root.id));
        }
      }
    }

    // Check terminator
    if (!IsTerminatorPureCF(block.terminator)) return std::nullopt;

    // Track the Wait terminator
    if (const auto* w = std::get_if<mir::Wait>(&block.terminator.data)) {
      if (wait_term != nullptr) return std::nullopt;  // Multiple Waits
      wait_term = w;
    }
  }

  // Must have exactly one Wait terminator
  if (wait_term == nullptr) return std::nullopt;

  // All triggers must be kAnyChange with no late-bound
  std::vector<PendingCombTrigger> triggers;
  triggers.reserve(wait_term->triggers.size());
  for (const auto& trigger : wait_term->triggers) {
    if (trigger.edge != common::EdgeKind::kAnyChange) return std::nullopt;
    if (trigger.late_bound.has_value()) return std::nullopt;
    triggers.push_back({
        .slot =
            mir::SlotId{ResolveSignalToGlobalSlot(trigger.signal, slot_base)},
        .observed_place = trigger.observed_place,
    });
  }

  if (triggers.empty()) return std::nullopt;

  // Self-edge detection: slot-granular overlap between write set and trigger
  // set. Conservative -- sub-slot disjointness (e.g. a[3] write vs a[5]
  // trigger) is not considered, matching the runtime's slot-granular snapshot
  // mechanism.
  bool has_self_edge = false;
  for (const auto& trigger : triggers) {
    if (write_slots.contains(trigger.slot.value)) {
      has_self_edge = true;
      break;
    }
  }

  return PendingCombKernelEntry{
      .triggers = std::move(triggers),
      .has_self_edge = has_self_edge,
  };
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
  // { suspend, body, engine_ptr, design_ptr, this_ptr,
  //   instance_id, signal_id_offset, outcome }
  using F = lyra::runtime::ProcessFrameHeaderField;
  constexpr auto kFieldCount = static_cast<size_t>(F::kFieldCount);
  std::array<llvm::Type*, kFieldCount> fields = {
      suspend_type, ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, outcome_ty};
  return llvm::StructType::create(ctx, fields, "ProcessStateHeader");
}

}  // namespace

auto BuildDesignLayout(
    const std::vector<SlotInfo>& slots, const TypeArena& types,
    const llvm::DataLayout& dl, bool force_two_state) -> DesignLayout {
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

  // Resolve canonical storage spec for each slot.
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

  // Compute inline-region byte offsets.
  // For kInlineValue: slot_byte_offsets[i] is the offset of the slot's
  //   value bytes, and slot_storage_specs[i] describes those bytes.
  // For kOwnedContainer: slot_byte_offsets[i] is the offset of the
  //   OwnedStorageHandle (fixed size), and slot_storage_specs[i]
  //   describes the backing data in the appendix (not the handle).
  uint64_t offset = 0;
  uint64_t max_align = 1;
  for (size_t i = 0; i < slots.size(); ++i) {
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
    layout.slot_byte_offsets.push_back(offset);
    offset += slot_size;
  }
  layout.inline_region_size = std::max(uint64_t{1}, AlignUp(offset, max_align));

  // Compute appendix offsets for owned container backing data.
  uint64_t appendix_offset = layout.inline_region_size;
  for (size_t i = 0; i < slots.size(); ++i) {
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
    appendix_offset = AlignUp(appendix_offset, backing_align);
    layout.owned_data_offsets[i] = appendix_offset;
    appendix_offset += spec.TotalByteSize();
  }

  // Verify invariants: kInlineValue slots must have nullopt owned offset,
  // kOwnedContainer slots must have engaged owned offset.
  for (size_t i = 0; i < slots.size(); ++i) {
    bool is_owned =
        slots[i].storage_shape == mir::StorageShape::kOwnedContainer;
    if (is_owned && !layout.owned_data_offsets[i].has_value()) {
      throw common::InternalError(
          "BuildDesignLayout",
          std::format("owned-container slot {} missing appendix offset", i));
    }
    if (!is_owned && layout.owned_data_offsets[i].has_value()) {
      throw common::InternalError(
          "BuildDesignLayout",
          std::format("inline-value slot {} has spurious appendix offset", i));
    }
  }

  // Total arena size covers both inline and appendix regions.
  layout.arena_size =
      std::max(uint64_t{1}, AlignUp(appendix_offset, max_align));

  // Collect 4-state patches using canonical byte offsets.
  // IsPatchTableEligible encodes the full eligibility rule: packed type,
  // 4-state, and lane size fits patch table (1/2/4/8 bytes).
  for (size_t i = 0; i < slots.size(); ++i) {
    if (!IsPatchTableEligible(layout.slot_storage_specs[i])) {
      continue;
    }
    const auto& spec = layout.slot_storage_specs[i];
    const auto& packed = std::get<PackedStorageSpec>(spec.data);

    uint64_t slot_offset = layout.slot_byte_offsets[i];
    uint32_t lane_byte_size = packed.LaneByteSize();
    uint32_t semantic_width = packed.bit_width;

    auto patch_offset = NarrowToU32(
        slot_offset + packed.UnknownLaneOffset(), "BuildDesignLayout");

    uint32_t mask_width = lane_byte_size * 8;
    uint64_t mask = semantic_width >= mask_width
                        ? ~uint64_t{0}
                        : (uint64_t{1} << semantic_width) - 1;

    switch (lane_byte_size) {
      case 1:
        layout.four_state_patches.patches_8.push_back(
            {patch_offset, static_cast<uint8_t>(mask)});
        break;
      case 2:
        layout.four_state_patches.patches_16.push_back(
            {patch_offset, static_cast<uint16_t>(mask)});
        break;
      case 4:
        layout.four_state_patches.patches_32.push_back(
            {patch_offset, static_cast<uint32_t>(mask)});
        break;
      case 8:
        layout.four_state_patches.patches_64.push_back({patch_offset, mask});
        break;
      default:
        throw common::InternalError(
            "BuildDesignLayout",
            "IsPatchTableEligible passed but lane size is not 1/2/4/8");
    }
  }

  return layout;
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
            .slot_id = mir::SlotId{static_cast<uint32_t>(i)},
            .type_id = type_id,
            .type_info = type_info,
            .storage_shape = slots_in[i].storage_shape,
        });
  }

  return slots;
}

auto Layout::GetInstanceBaseByteOffset(ModuleIndex idx) const -> uint64_t {
  return instance_base_byte_offsets[idx.value];
}

auto Layout::GetInstanceRelByteOffsets(ModuleIndex idx) const
    -> const std::vector<uint64_t>& {
  if (idx.value >= instance_rel_byte_offsets.size()) {
    throw common::InternalError(
        "GetInstanceRelByteOffsets",
        std::format(
            "module_index {} out of range (size={})", idx.value,
            instance_rel_byte_offsets.size()));
  }
  return instance_rel_byte_offsets[idx.value];
}

auto BuildLayout(
    std::span<const mir::ProcessId> init_processes,
    std::span<const mir::ProcessId> connection_processes,
    std::span<const LayoutModulePlan> module_plans, const mir::Design& design,
    const mir::Arena& design_arena, const TypeArena& types,
    DesignLayout design_layout, llvm::LLVMContext& ctx,
    const llvm::DataLayout& dl, bool force_two_state) -> Layout {
  Layout layout;

  // Build runtime types
  layout.suspend_record_type = BuildSuspendRecordType(ctx);
  layout.header_type = BuildHeaderType(ctx, layout.suspend_record_type);

  // Use prebuilt design layout
  layout.design = std::move(design_layout);

  // Phase 1: Collect init processes (package variable initialization)
  for (mir::ProcessId proc_id : init_processes) {
    layout.scheduled_processes.push_back({proc_id, ModuleIndex{}});
  }
  layout.num_init_processes = layout.scheduled_processes.size();

  // Phase 2: Collect connection processes (scheduled, always_comb semantics)
  // Kernelizable ones are separated out.
  for (mir::ProcessId proc_id : connection_processes) {
    const auto& process = design_arena[proc_id];
    auto pending = TryKernelizeConnection(process, design_arena);
    if (pending) {
      std::optional<ResolvedObservation> obs;
      if (pending->trigger_observed_place) {
        obs = ResolveObservation(
            design_arena, layout.design, pending->trigger_slot,
            *pending->trigger_observed_place);
      }
      layout.connection_kernel_entries.push_back({
          .process_id = proc_id,
          .src_slot = pending->src_slot,
          .dst_slot = pending->dst_slot,
          .trigger_slot = pending->trigger_slot,
          .trigger_edge = pending->trigger_edge,
          .trigger_observation = obs,
      });
    } else {
      layout.scheduled_processes.push_back({proc_id, ModuleIndex{}});
    }
  }

  // Phase 3: Collect module processes (run through scheduler)
  // Also detect pure combinational processes for comb kernel batching.
  layout.num_module_process_base = layout.scheduled_processes.size();
  // module_process_counter is 0-based within the module-process slice
  // of scheduled_processes. Comb kernels receive this as their canonical
  // scheduled_process_index.
  uint32_t module_process_counter = 0;
  for (uint32_t mi = 0; mi < module_plans.size(); ++mi) {
    const auto& plan = module_plans[mi];
    const auto& body_arena = design.module_bodies.at(plan.body_id.value).arena;
    for (mir::ProcessId proc_id : plan.body_processes) {
      const auto& process = body_arena[proc_id];
      if (process.kind == mir::ProcessKind::kFinal) continue;

      auto pending =
          TryKernelizeComb(process, body_arena, plan.design_state_base_slot);
      if (pending) {
        std::vector<CombTrigger> resolved_triggers;
        resolved_triggers.reserve(pending->triggers.size());
        for (const auto& pt : pending->triggers) {
          std::optional<ResolvedObservation> obs;
          if (pt.observed_place) {
            obs = ResolveObservation(
                body_arena, layout.design, pt.slot, *pt.observed_place);
          }
          resolved_triggers.push_back({.slot = pt.slot, .observation = obs});
        }
        layout.comb_kernel_entries.push_back({
            .scheduled_process_index = module_process_counter,
            .triggers = std::move(resolved_triggers),
            .has_self_edge = pending->has_self_edge,
        });
      }
      layout.scheduled_processes.push_back({proc_id, ModuleIndex{mi}});
      ++module_process_counter;
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

  // Find the ordinal index of a process within a body's process list.
  // Throws InternalError if the process is not found.
  auto find_proc_within_body = [](const LayoutModulePlan& plan,
                                  mir::ProcessId proc_id) -> uint32_t {
    for (uint32_t pi = 0; pi < plan.body_processes.size(); ++pi) {
      if (plan.body_processes[pi] == proc_id) {
        return pi;
      }
    }
    throw common::InternalError(
        "BuildLayout", "process not found in body process list");
  };

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
      uint32_t pwb = find_proc_within_body(module_plans[mi], sp.process_id);
      frame_name = std::format("Body{}Frame{}", body_id.value, pwb);
      state_name = std::format("Body{}State{}", body_id.value, pwb);
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
    size_t num_simulation =
        layout.scheduled_processes.size() - layout.num_init_processes;

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

    // Map from (body_id, proc_within_body) to schema index for module
    // process deduplication.
    struct SchemaKey {
      uint32_t body_id;
      uint32_t proc_within_body;
      auto operator==(const SchemaKey&) const -> bool = default;
    };
    struct SchemaKeyHash {
      auto operator()(const SchemaKey& k) const -> size_t {
        return std::hash<uint64_t>{}(
            (static_cast<uint64_t>(k.body_id) << 32) | k.proc_within_body);
      }
    };
    std::unordered_map<SchemaKey, uint32_t, SchemaKeyHash> module_schema_map;

    layout.constructor_records.reserve(num_simulation);
    conn_counter = 0;

    for (size_t i = layout.num_init_processes;
         i < layout.scheduled_processes.size(); ++i) {
      const auto& sp = layout.scheduled_processes[i];
      auto props = compute_schema_props(layout.processes[i]);

      if (i < layout.num_module_process_base) {
        // Connection process: unique schema per process.
        auto schema_idx = static_cast<uint32_t>(layout.state_schemas.size());
        layout.state_schemas.push_back({
            .state_size = props.state_size,
            .state_align = props.state_align,
            .needs_4state_init = props.needs_4state_init,
            .representative_process_index = i,
            .body_id = std::nullopt,
            .proc_within_body = std::nullopt,
            .conn_index = conn_counter,
        });
        layout.constructor_records.push_back({.schema_index = schema_idx});
        ++conn_counter;
      } else {
        // Module process: deduplicate by (body_id, proc_within_body).
        uint32_t mi = sp.module_index.value;
        auto body_id = module_plans[mi].body_id;
        uint32_t pwb = find_proc_within_body(module_plans[mi], sp.process_id);

        SchemaKey key{.body_id = body_id.value, .proc_within_body = pwb};
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
          if (existing.body_id != body_id || existing.proc_within_body != pwb ||
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
          uint32_t rep_pwb =
              find_proc_within_body(module_plans[rep_mi], rep_sp.process_id);
          if (rep_body_id != body_id || rep_pwb != pwb) {
            throw common::InternalError(
                "BuildLayout",
                "representative process does not match schema identity");
          }
          layout.constructor_records.push_back({.schema_index = it->second});
        } else {
          auto schema_idx = static_cast<uint32_t>(layout.state_schemas.size());
          layout.state_schemas.push_back({
              .state_size = props.state_size,
              .state_align = props.state_align,
              .needs_4state_init = props.needs_4state_init,
              .representative_process_index = i,
              .body_id = body_id,
              .proc_within_body = pwb,
              .conn_index = std::nullopt,
          });
          module_schema_map[key] = schema_idx;
          layout.constructor_records.push_back({.schema_index = schema_idx});
        }
      }
    }
  }

  // Compute instance base byte offsets and body-owned relative byte offsets
  // from canonical slot_byte_offsets.
  if (!module_plans.empty()) {
    const auto& offsets = layout.design.slot_byte_offsets;
    size_t num_instances = module_plans.size();
    layout.instance_base_byte_offsets.resize(num_instances);

    for (size_t mi = 0; mi < num_instances; ++mi) {
      const auto& plan = module_plans[mi];
      if (plan.slot_count > 0) {
        if (plan.design_state_base_slot + plan.slot_count > offsets.size()) {
          throw common::InternalError(
              "BuildLayout",
              std::format(
                  "slot range [{}, {}) exceeds design slot count {}",
                  plan.design_state_base_slot,
                  plan.design_state_base_slot + plan.slot_count,
                  offsets.size()));
        }
        layout.instance_base_byte_offsets[mi] =
            offsets[plan.design_state_base_slot];
      }
    }

    // Compute per-instance raw relative byte offsets.
    // Consumed by spec compilation to classify slots as stable/unstable.
    layout.instance_rel_byte_offsets.resize(num_instances);

    for (size_t mi = 0; mi < num_instances; ++mi) {
      const auto& plan = module_plans[mi];
      std::vector<uint64_t> rel_offsets(plan.slot_count);
      if (plan.slot_count > 0) {
        uint64_t base = offsets[plan.design_state_base_slot];
        for (uint32_t i = 0; i < plan.slot_count; ++i) {
          rel_offsets[i] = offsets[plan.design_state_base_slot + i] - base;
        }
      }
      layout.instance_rel_byte_offsets[mi] = std::move(rel_offsets);
    }
  }

  return layout;
}

}  // namespace lyra::lowering::mir_to_llvm
