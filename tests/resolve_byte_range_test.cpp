#include <cstdint>
#include <gtest/gtest.h>

#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {
namespace {

// Helper: build a constant integer operand for index projections.
auto MakeConstIndex(uint64_t value) -> mir::Operand {
  IntegralConstant ic;
  ic.value.push_back(value);
  return mir::Operand::Const({.type = TypeId{0}, .value = ic});
}

// Helper: build a Place with given projections.
auto MakePlace(std::vector<mir::Projection> projections) -> mir::Place {
  mir::Place place;
  place.root = {
      .kind = mir::PlaceRoot::Kind::kModuleSlot,
      .id = 0,
      .type = TypeId{0},
  };
  place.projections = std::move(projections);
  return place;
}

TEST(ResolveByteRangeTest, EmptyProjections) {
  SlotStorageSpec spec{
      .data = PackedStorageSpec{
          .layout = {.total_byte_size = 4, .alignment = 4},
          .bit_width = 32,
          .is_four_state = false}};
  StorageSpecArena arena;
  auto place = MakePlace({});

  auto range = ResolveByteRange(spec, arena, place, nullptr);
  EXPECT_EQ(range.kind, RangeKind::kFullSlot);
}

TEST(ResolveByteRangeTest, StructFieldProjection) {
  StorageSpecArena arena;

  // Struct with two fields: u8 at offset 0, u32 at offset 4
  auto field0_id = arena.Alloc(
      SlotStorageSpec{
          .data = PackedStorageSpec{
              .layout = {.total_byte_size = 1, .alignment = 1},
              .bit_width = 8,
              .is_four_state = false}});
  auto field1_id = arena.Alloc(
      SlotStorageSpec{
          .data = PackedStorageSpec{
              .layout = {.total_byte_size = 4, .alignment = 4},
              .bit_width = 32,
              .is_four_state = false}});

  SlotStorageSpec struct_spec{
      .data = StructStorageSpec{
          .layout = {.total_byte_size = 8, .alignment = 4},
          .fields = {
              {.byte_offset = 0, .field_spec_id = field0_id},
              {.byte_offset = 4, .field_spec_id = field1_id}}}};

  // Project to field 1 (u32 at offset 4)
  mir::Projection proj;
  proj.info = mir::FieldProjection{.field_index = 1};
  auto place = MakePlace({proj});

  auto range = ResolveByteRange(struct_spec, arena, place, nullptr);
  EXPECT_EQ(range.kind, RangeKind::kPrecise);
  EXPECT_EQ(range.byte_offset, 4U);
  EXPECT_EQ(range.byte_size, 4U);
}

TEST(ResolveByteRangeTest, ArrayIndexProjection) {
  StorageSpecArena arena;

  auto elem_id = arena.Alloc(
      SlotStorageSpec{
          .data = PackedStorageSpec{
              .layout = {.total_byte_size = 4, .alignment = 4},
              .bit_width = 32,
              .is_four_state = false}});

  SlotStorageSpec array_spec{
      .data = ArrayStorageSpec{
          .layout = {.total_byte_size = 40, .alignment = 4},
          .element_count = 10,
          .element_stride = 4,
          .element_spec_id = elem_id}};

  // Index element 3
  mir::Projection proj;
  proj.info = mir::IndexProjection{.index = MakeConstIndex(3)};
  auto place = MakePlace({proj});

  auto range = ResolveByteRange(array_spec, arena, place, nullptr);
  EXPECT_EQ(range.kind, RangeKind::kPrecise);
  EXPECT_EQ(range.byte_offset, 12U);
  EXPECT_EQ(range.byte_size, 4U);
}

TEST(ResolveByteRangeTest, NestedStructInArray) {
  StorageSpecArena arena;

  // Inner struct: {u8 @ 0, u16 @ 2}
  auto inner_f0 = arena.Alloc(
      SlotStorageSpec{
          .data = PackedStorageSpec{
              .layout = {.total_byte_size = 1, .alignment = 1},
              .bit_width = 8,
              .is_four_state = false}});
  auto inner_f1 = arena.Alloc(
      SlotStorageSpec{
          .data = PackedStorageSpec{
              .layout = {.total_byte_size = 2, .alignment = 2},
              .bit_width = 16,
              .is_four_state = false}});
  auto struct_id = arena.Alloc(
      SlotStorageSpec{
          .data = StructStorageSpec{
              .layout = {.total_byte_size = 4, .alignment = 2},
              .fields = {
                  {.byte_offset = 0, .field_spec_id = inner_f0},
                  {.byte_offset = 2, .field_spec_id = inner_f1}}}});

  // Array of 5 structs, stride 4
  SlotStorageSpec array_spec{
      .data = ArrayStorageSpec{
          .layout = {.total_byte_size = 20, .alignment = 2},
          .element_count = 5,
          .element_stride = 4,
          .element_spec_id = struct_id}};

  // arr[2].field1 -> offset = 2*4 + 2 = 10, size = 2
  mir::Projection idx_proj;
  idx_proj.info = mir::IndexProjection{.index = MakeConstIndex(2)};
  mir::Projection field_proj;
  field_proj.info = mir::FieldProjection{.field_index = 1};
  auto place = MakePlace({idx_proj, field_proj});

  auto range = ResolveByteRange(array_spec, arena, place, nullptr);
  EXPECT_EQ(range.kind, RangeKind::kPrecise);
  EXPECT_EQ(range.byte_offset, 10U);
  EXPECT_EQ(range.byte_size, 2U);
}

TEST(ResolveByteRangeTest, ArrayInsideStruct) {
  StorageSpecArena arena;

  // Element: u32
  auto elem_id = arena.Alloc(
      SlotStorageSpec{
          .data = PackedStorageSpec{
              .layout = {.total_byte_size = 4, .alignment = 4},
              .bit_width = 32,
              .is_four_state = false}});
  // Array of 8 u32, stride 4
  auto array_id = arena.Alloc(
      SlotStorageSpec{
          .data = ArrayStorageSpec{
              .layout = {.total_byte_size = 32, .alignment = 4},
              .element_count = 8,
              .element_stride = 4,
              .element_spec_id = elem_id}});
  // Struct: {u8 @ 0, array @ 4}
  auto f0_id = arena.Alloc(
      SlotStorageSpec{
          .data = PackedStorageSpec{
              .layout = {.total_byte_size = 1, .alignment = 1},
              .bit_width = 8,
              .is_four_state = false}});

  SlotStorageSpec struct_spec{
      .data = StructStorageSpec{
          .layout = {.total_byte_size = 36, .alignment = 4},
          .fields = {
              {.byte_offset = 0, .field_spec_id = f0_id},
              {.byte_offset = 4, .field_spec_id = array_id}}}};

  // struct.field1[5] -> offset = 4 + 5*4 = 24, size = 4
  mir::Projection field_proj;
  field_proj.info = mir::FieldProjection{.field_index = 1};
  mir::Projection idx_proj;
  idx_proj.info = mir::IndexProjection{.index = MakeConstIndex(5)};
  auto place = MakePlace({field_proj, idx_proj});

  auto range = ResolveByteRange(struct_spec, arena, place, nullptr);
  EXPECT_EQ(range.kind, RangeKind::kPrecise);
  EXPECT_EQ(range.byte_offset, 24U);
  EXPECT_EQ(range.byte_size, 4U);
}

TEST(ResolveByteRangeTest, FourStateLeafSize) {
  StorageSpecArena arena;

  auto elem_id = arena.Alloc(
      SlotStorageSpec{
          .data = PackedStorageSpec{
              .layout = {.total_byte_size = 8, .alignment = 4},
              .bit_width = 32,
              .is_four_state = true}});

  SlotStorageSpec array_spec{
      .data = ArrayStorageSpec{
          .layout = {.total_byte_size = 80, .alignment = 4},
          .element_count = 10,
          .element_stride = 8,
          .element_spec_id = elem_id}};

  mir::Projection proj;
  proj.info = mir::IndexProjection{.index = MakeConstIndex(1)};
  auto place = MakePlace({proj});

  auto range = ResolveByteRange(array_spec, arena, place, nullptr);
  EXPECT_EQ(range.kind, RangeKind::kPrecise);
  EXPECT_EQ(range.byte_offset, 8U);
  // 4-state: total_byte_size = 8 (val lane + unk lane)
  EXPECT_EQ(range.byte_size, 8U);
}

TEST(ResolveByteRangeTest, UnionProjectionDegradesToFullSlot) {
  StorageSpecArena arena;

  SlotStorageSpec union_spec{
      .data = UnionStorageSpec{
          .layout = {.total_byte_size = 8, .alignment = 4},
          .has_four_state_content = false}};

  mir::Projection proj;
  proj.info = mir::UnionMemberProjection{.member_index = 0};
  auto place = MakePlace({proj});

  auto range = ResolveByteRange(union_spec, arena, place, nullptr);
  EXPECT_EQ(range.kind, RangeKind::kFullSlot);
}

TEST(ResolveByteRangeTest, DynamicIndexDegradesToFullSlot) {
  StorageSpecArena arena;

  auto elem_id = arena.Alloc(
      SlotStorageSpec{
          .data = PackedStorageSpec{
              .layout = {.total_byte_size = 4, .alignment = 4},
              .bit_width = 32,
              .is_four_state = false}});

  SlotStorageSpec array_spec{
      .data = ArrayStorageSpec{
          .layout = {.total_byte_size = 40, .alignment = 4},
          .element_count = 10,
          .element_stride = 4,
          .element_spec_id = elem_id}};

  // Dynamic index (kUseTemp with no resolver)
  mir::Operand dyn_index{
      .kind = mir::Operand::Kind::kUseTemp, .payload = mir::TempId{0}};
  mir::Projection proj;
  proj.info = mir::IndexProjection{.index = dyn_index};
  auto place = MakePlace({proj});

  auto range = ResolveByteRange(array_spec, arena, place, nullptr);
  EXPECT_EQ(range.kind, RangeKind::kFullSlot);
}

TEST(ResolveByteRangeTest, BitRangeProjection) {
  StorageSpecArena arena;

  SlotStorageSpec packed_spec{
      .data = PackedStorageSpec{
          .layout = {.total_byte_size = 4, .alignment = 4},
          .bit_width = 32,
          .is_four_state = false}};

  mir::Projection proj;
  proj.info = mir::BitRangeProjection{
      .bit_offset = MakeConstIndex(11), .width = 1, .element_type = TypeId{0}};
  auto place = MakePlace({proj});

  auto range = ResolveByteRange(packed_spec, arena, place, nullptr);
  EXPECT_EQ(range.kind, RangeKind::kPrecise);
  // bit 11 -> byte 1, bit_index 3
  EXPECT_EQ(range.byte_offset, 1U);
  EXPECT_EQ(range.byte_size, 1U);
  EXPECT_EQ(range.bit_index, 3U);
}

}  // namespace
}  // namespace lyra::lowering::mir_to_llvm
