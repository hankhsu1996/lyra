#include "lyra/llvm_backend/layout/storage_contract.hpp"

#include <algorithm>
#include <cstdint>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/type_query.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto AlignUp(uint32_t value, uint32_t align) -> uint32_t {
  return (value + align - 1) / align * align;
}

}  // namespace

auto GetStorageByteSize(uint32_t bit_width) -> uint32_t {
  if (bit_width <= 8) return 1;
  if (bit_width <= 16) return 2;
  if (bit_width <= 32) return 4;
  if (bit_width <= 64) return 8;
  return (bit_width + 7) / 8;
}

auto GetStorageAlignment(uint32_t bit_width) -> uint32_t {
  return std::min(GetStorageByteSize(bit_width), uint32_t{8});
}

auto FourStateUnknownLaneOffset(uint32_t bit_width) -> uint32_t {
  return GetStorageByteSize(bit_width);
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

  uint32_t stride = AlignUp(elem_size, elem_align);
  uint32_t count = info.range.Size();
  uint32_t total = count > 0 ? ((stride * (count - 1)) + elem_size) : 0;

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
  uint32_t offset = 0;
  uint32_t max_align = 1;

  for (const auto& field : info.fields) {
    auto field_spec = ResolveImpl(field.type, types, mode, target, arena);
    uint32_t field_align = field_spec.Alignment();
    uint32_t field_size = field_spec.TotalByteSize();
    auto field_id = arena.Alloc(std::move(field_spec));

    max_align = std::max(max_align, field_align);
    offset = AlignUp(offset, field_align);

    fields.push_back({.byte_offset = offset, .field_spec_id = field_id});
    offset += field_size;
  }

  uint32_t total = AlignUp(offset, max_align);

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

  for (const auto& member : info.members) {
    auto member_spec = ResolveImpl(member.type, types, mode, target, arena);
    max_size = std::max(max_size, member_spec.TotalByteSize());
    max_align = std::max(max_align, member_spec.Alignment());
    // Member specs are not retained. Union layout only needs max size and
    // max alignment. Do not allocate into the arena.
  }

  uint32_t total = AlignUp(max_size, max_align);
  return {.layout = {.total_byte_size = total, .alignment = max_align}};
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
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      return {
          .data = HandleStorageSpec{
              .layout = {
                  .total_byte_size = target.pointer_byte_size,
                  .alignment = target.pointer_alignment}}};

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
