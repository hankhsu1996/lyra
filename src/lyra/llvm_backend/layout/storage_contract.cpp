#include "lyra/llvm_backend/layout/storage_contract.hpp"

#include <algorithm>
#include <cstdint>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/packed_storage_abi.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/type_query.hpp"

namespace lyra::lowering::mir_to_llvm {

auto IsPatchTableEligible(const SlotStorageSpec& spec) -> bool {
  const auto* packed = std::get_if<PackedStorageSpec>(&spec.data);
  if (packed == nullptr || !packed->is_four_state) {
    return false;
  }
  uint32_t lane_bytes = packed->LaneByteSize();
  return lane_bytes == 1 || lane_bytes == 2 || lane_bytes == 4 ||
         lane_bytes == 8;
}

// Delegates to the shared ABI contract in lyra::PackedStorageByteSize.
auto GetStorageByteSize(uint32_t bit_width) -> uint32_t {
  return lyra::PackedStorageByteSize(bit_width);
}

auto GetStorageAlignment(uint32_t bit_width) -> uint32_t {
  return std::min(GetStorageByteSize(bit_width), uint32_t{8});
}

// Delegates to the shared ABI contract in lyra::FourStateUnknownByteOffset.
auto FourStateUnknownLaneOffset(uint32_t bit_width) -> uint32_t {
  return lyra::FourStateUnknownByteOffset(bit_width);
}

auto FourStateTotalByteSize(uint32_t bit_width) -> uint32_t {
  return 2 * GetStorageByteSize(bit_width);
}

auto SlotStorageSpec::TotalByteSize() const -> uint32_t {
  return std::visit(
      [](const auto& s) -> uint32_t { return s.layout.total_byte_size; }, data);
}

auto SlotStorageSpec::Alignment() const -> uint32_t {
  return std::visit(
      [](const auto& s) -> uint32_t { return s.layout.alignment; }, data);
}

auto HasFourStateContent(
    const SlotStorageSpec& spec, const StorageSpecArena& arena) -> bool {
  return std::visit(
      common::Overloaded{
          [](const PackedStorageSpec& s) -> bool { return s.is_four_state; },
          [](const FloatStorageSpec&) -> bool { return false; },
          [&](const ArrayStorageSpec& s) -> bool {
            return HasFourStateContent(arena.Get(s.element_spec_id), arena);
          },
          [&](const StructStorageSpec& s) -> bool {
            return std::ranges::any_of(s.fields, [&](const auto& field) {
              return HasFourStateContent(arena.Get(field.field_spec_id), arena);
            });
          },
          [](const UnionStorageSpec& s) -> bool {
            return s.has_four_state_content;
          },
          [](const HandleStorageSpec&) -> bool { return false; },
      },
      spec.data);
}

namespace {

auto ResolveImpl(
    TypeId type_id, const TypeArena& types, StorageMode mode,
    const TargetStorageAbi& target, StorageSpecArena& arena) -> SlotStorageSpec;

auto ResolvePackedSpec(
    const Type& type, const TypeArena& types, StorageMode mode)
    -> PackedStorageSpec {
  uint32_t width = PackedBitWidth(type, types);
  bool is_four_state =
      IsPackedFourState(type, types, mode == StorageMode::kTwoState);
  uint32_t lane_size = GetStorageByteSize(width);
  uint32_t align = GetStorageAlignment(width);
  // Dense storage: known lane immediately followed by unknown lane.
  // No inter-lane padding. This is the Lyra canonical storage rule.
  uint32_t size = is_four_state ? FourStateTotalByteSize(width) : lane_size;
  return {
      .layout = {.total_byte_size = size, .alignment = align},
      .bit_width = width,
      .is_four_state = is_four_state,
  };
}

auto ResolveArraySpec(
    const UnpackedArrayInfo& info, const TypeArena& types, StorageMode mode,
    const TargetStorageAbi& target, StorageSpecArena& arena)
    -> ArrayStorageSpec {
  auto elem_spec = ResolveImpl(info.element_type, types, mode, target, arena);
  uint32_t elem_size = elem_spec.TotalByteSize();
  uint32_t elem_align = elem_spec.Alignment();
  auto elem_id = arena.Alloc(std::move(elem_spec));

  auto stride = NarrowToU32(AlignUp(elem_size, elem_align), "ArrayStorageSpec");
  uint32_t count = info.range.Size();
  // Compute in uint64_t to avoid overflow, then narrow with check.
  uint64_t total_u64 =
      count > 0 ? ((static_cast<uint64_t>(stride) * (count - 1)) + elem_size)
                : 0;
  auto total = NarrowToU32(total_u64, "ArrayStorageSpec total");

  return {
      .layout = {.total_byte_size = total, .alignment = elem_align},
      .element_count = count,
      .element_stride = stride,
      .element_spec_id = elem_id,
  };
}

auto ResolveStructSpec(
    const UnpackedStructInfo& info, const TypeArena& types, StorageMode mode,
    const TargetStorageAbi& target, StorageSpecArena& arena)
    -> StructStorageSpec {
  std::vector<StructFieldSpec> fields;
  uint64_t offset = 0;
  uint32_t max_align = 1;

  for (const auto& field : info.fields) {
    auto field_spec = ResolveImpl(field.type, types, mode, target, arena);
    uint32_t field_align = field_spec.Alignment();
    uint32_t field_size = field_spec.TotalByteSize();
    auto field_id = arena.Alloc(std::move(field_spec));

    max_align = std::max(max_align, field_align);
    offset = AlignUp(offset, field_align);
    auto field_offset = NarrowToU32(offset, "StructStorageSpec field offset");

    fields.push_back({.byte_offset = field_offset, .field_spec_id = field_id});
    offset += field_size;
  }

  auto total = NarrowToU32(AlignUp(offset, max_align), "StructStorageSpec");

  return {
      .layout = {.total_byte_size = total, .alignment = max_align},
      .fields = std::move(fields),
  };
}

auto ResolveUnionSpec(
    TypeId type_id, const TypeArena& types, StorageMode mode,
    const TargetStorageAbi& target, StorageSpecArena& arena)
    -> UnionStorageSpec {
  const auto& info = types[type_id].AsUnpackedUnion();
  uint32_t max_size = 0;
  uint32_t max_align = 1;
  bool has_four_state = false;

  for (const auto& member : info.members) {
    auto member_spec = ResolveImpl(member.type, types, mode, target, arena);
    max_size = std::max(max_size, member_spec.TotalByteSize());
    max_align = std::max(max_align, member_spec.Alignment());
    if (HasFourStateContent(member_spec, arena)) {
      has_four_state = true;
    }
    // Member specs are not retained individually. The union only needs
    // max size, max alignment, and the 4-state summary.
  }

  auto total = NarrowToU32(AlignUp(max_size, max_align), "UnionStorageSpec");
  return {
      .layout = {.total_byte_size = total, .alignment = max_align},
      .has_four_state_content = has_four_state,
  };
}

auto ResolveImpl(
    TypeId type_id, const TypeArena& types, StorageMode mode,
    const TargetStorageAbi& target, StorageSpecArena& arena)
    -> SlotStorageSpec {
  const Type& type = types[type_id];

  switch (type.Kind()) {
    case TypeKind::kIntegral:
    case TypeKind::kPackedArray:
    case TypeKind::kPackedStruct:
    case TypeKind::kEnum:
      return {.data = ResolvePackedSpec(type, types, mode)};

    case TypeKind::kReal:
      return {
          .data = FloatStorageSpec{
              .layout = {.total_byte_size = 8, .alignment = 8}}};

    case TypeKind::kShortReal:
      return {
          .data = FloatStorageSpec{
              .layout = {.total_byte_size = 4, .alignment = 4}}};

    case TypeKind::kUnpackedArray:
      return {
          .data = ResolveArraySpec(
              type.AsUnpackedArray(), types, mode, target, arena)};

    case TypeKind::kUnpackedStruct:
      return {
          .data = ResolveStructSpec(
              type.AsUnpackedStruct(), types, mode, target, arena)};

    case TypeKind::kUnpackedUnion:
      return {.data = ResolveUnionSpec(type_id, types, mode, target, arena)};

    case TypeKind::kString:
      return {
          .data = HandleStorageSpec{
              .layout =
                  {.total_byte_size = target.pointer_byte_size,
                   .alignment = target.pointer_alignment},
              .kind = HandleKind::kString}};

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      return {
          .data = HandleStorageSpec{
              .layout =
                  {.total_byte_size = target.pointer_byte_size,
                   .alignment = target.pointer_alignment},
              .kind = HandleKind::kContainer}};

    case TypeKind::kVoid:
      throw common::InternalError(
          "ResolveStorageSpec", "void type cannot have storage");
  }

  throw common::InternalError("ResolveStorageSpec", "unreachable");
}

}  // namespace

auto ResolveStorageSpec(
    TypeId type_id, const TypeArena& types, StorageMode mode,
    const TargetStorageAbi& target, StorageSpecArena& arena)
    -> SlotStorageSpec {
  return ResolveImpl(type_id, types, mode, target, arena);
}

}  // namespace lyra::lowering::mir_to_llvm
