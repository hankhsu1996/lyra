#include <gtest/gtest.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/llvm_backend/observable_descriptor_utils.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::mir_to_llvm {
namespace {

auto MakeLogicType(TypeArena& arena) -> TypeId {
  return arena.Intern(
      TypeKind::kIntegral,
      IntegralInfo{.bit_width = 1, .is_signed = false, .is_four_state = true});
}

auto MakeIntType(TypeArena& arena) -> TypeId {
  return arena.Intern(
      TypeKind::kIntegral,
      IntegralInfo{.bit_width = 32, .is_signed = true, .is_four_state = false});
}

auto MakeLogicSpec() -> SlotStorageSpec {
  return SlotStorageSpec{
      .data = PackedStorageSpec{
          .layout = {.total_byte_size = 2, .alignment = 1},
          .bit_width = 1,
          .is_four_state = true}};
}

auto MakeIntSpec() -> SlotStorageSpec {
  return SlotStorageSpec{
      .data = PackedStorageSpec{
          .layout = {.total_byte_size = 4, .alignment = 4},
          .bit_width = 32,
          .is_four_state = false}};
}

TEST(CanonicalObservableShapeTest, NonForwardedSlot) {
  TypeArena arena;
  TypeId logic_id = MakeLogicType(arena);

  DesignLayout layout;
  layout.slot_storage_specs = {MakeLogicSpec()};

  CanonicalObservableShape shape = ComputeCanonicalObservableShape(
      ObservableOwnerSlotId::Create(0), layout, logic_id,
      mir::SlotKind::kVariable, arena);

  EXPECT_EQ(shape.trace.bit_width, 1U);
  EXPECT_EQ(shape.trace.trace_kind, runtime::TraceSignalKind::kVariable);
  EXPECT_EQ(shape.storage.storage_kind, runtime::SlotStorageKind::kPacked4);
  EXPECT_EQ(shape.storage.total_bytes, 2U);
  ASSERT_TRUE(shape.storage.packed4_lanes.has_value());
  EXPECT_EQ(shape.storage.packed4_lanes->value_lane_byte_size, 1U);
}

// This API takes only the canonical owner. Alias-local metadata is not an
// input. The test verifies only owner-based shape.
TEST(CanonicalObservableShapeTest, VariableOwnerWinsOverAliasLocalNetMetadata) {
  TypeArena arena;
  TypeId logic_id = MakeLogicType(arena);

  DesignLayout layout;
  layout.slot_storage_specs = {MakeLogicSpec(), MakeLogicSpec()};

  CanonicalObservableShape shape = ComputeCanonicalObservableShape(
      ObservableOwnerSlotId::Create(0), layout, logic_id,
      mir::SlotKind::kVariable, arena);

  EXPECT_EQ(shape.trace.trace_kind, runtime::TraceSignalKind::kVariable);
  EXPECT_EQ(shape.trace.bit_width, 1U);
}

TEST(CanonicalObservableShapeTest, SameTypeForwarding) {
  TypeArena arena;
  TypeId int_id = MakeIntType(arena);

  DesignLayout layout;
  layout.slot_storage_specs = {MakeIntSpec(), MakeIntSpec()};

  CanonicalObservableShape shape = ComputeCanonicalObservableShape(
      ObservableOwnerSlotId::Create(0), layout, int_id,
      mir::SlotKind::kVariable, arena);

  EXPECT_EQ(shape.trace.bit_width, 32U);
  EXPECT_EQ(shape.trace.trace_kind, runtime::TraceSignalKind::kVariable);
  EXPECT_FALSE(shape.storage.packed4_lanes.has_value());
}

// Guardrail: even if another slot row differs, canonical observable shape
// must read storage only from the owner row provided to the API.
TEST(
    CanonicalObservableShapeTest,
    CanonicalStorageShapeUsesOwnerRowEvenIfAliasRowDiffers) {
  TypeArena arena;
  TypeId int_id = MakeIntType(arena);

  // Owner row (0): 32-bit 2-state. Other row (1): 1-bit 4-state.
  // Helper should read row 0.
  DesignLayout layout;
  layout.slot_storage_specs = {MakeIntSpec(), MakeLogicSpec()};

  CanonicalObservableShape shape = ComputeCanonicalObservableShape(
      ObservableOwnerSlotId::Create(0), layout, int_id,
      mir::SlotKind::kVariable, arena);

  EXPECT_EQ(shape.storage.total_bytes, 4U);
  EXPECT_EQ(shape.storage.storage_kind, runtime::SlotStorageKind::kPacked2);
  EXPECT_FALSE(shape.storage.packed4_lanes.has_value());
}

TEST(CanonicalObservableShapeTest, OwnerOutOfRangeThrows) {
  TypeArena arena;
  TypeId logic_id = MakeLogicType(arena);

  DesignLayout layout;
  layout.slot_storage_specs = {MakeLogicSpec()};

  EXPECT_THROW(
      ComputeCanonicalObservableShape(
          ObservableOwnerSlotId::Create(99), layout, logic_id,
          mir::SlotKind::kVariable, arena),
      common::InternalError);
}

TEST(CanonicalObservableShapeTest, Packed4ShapeCarriesLaneFields) {
  TypeArena arena;
  TypeId logic_id = MakeLogicType(arena);

  DesignLayout layout;
  layout.slot_storage_specs = {MakeLogicSpec()};

  CanonicalObservableShape shape = ComputeCanonicalObservableShape(
      ObservableOwnerSlotId::Create(0), layout, logic_id,
      mir::SlotKind::kVariable, arena);

  EXPECT_EQ(shape.storage.storage_kind, runtime::SlotStorageKind::kPacked4);
  ASSERT_TRUE(shape.storage.packed4_lanes.has_value());
  EXPECT_EQ(shape.storage.packed4_lanes->value_lane_byte_offset, 0U);
  EXPECT_EQ(shape.storage.packed4_lanes->value_lane_byte_size, 1U);
  EXPECT_GT(shape.storage.packed4_lanes->unk_lane_byte_offset, 0U);
  EXPECT_EQ(shape.storage.packed4_lanes->unk_lane_byte_size, 1U);
}

TEST(CanonicalObservableShapeTest, NonPacked4ShapeHasNoLanes) {
  TypeArena arena;
  TypeId int_id = MakeIntType(arena);

  DesignLayout layout;
  layout.slot_storage_specs = {MakeIntSpec()};

  CanonicalObservableShape shape = ComputeCanonicalObservableShape(
      ObservableOwnerSlotId::Create(0), layout, int_id,
      mir::SlotKind::kVariable, arena);

  EXPECT_EQ(shape.storage.storage_kind, runtime::SlotStorageKind::kPacked2);
  EXPECT_FALSE(shape.storage.packed4_lanes.has_value());
}

TEST(CanonicalObservableShapeTest, NetOwnerProducesNetTraceKind) {
  TypeArena arena;
  TypeId logic_id = MakeLogicType(arena);

  DesignLayout layout;
  layout.slot_storage_specs = {MakeLogicSpec()};

  CanonicalObservableShape shape = ComputeCanonicalObservableShape(
      ObservableOwnerSlotId::Create(0), layout, logic_id, mir::SlotKind::kNet,
      arena);

  EXPECT_EQ(shape.trace.trace_kind, runtime::TraceSignalKind::kNet);
}

}  // namespace
}  // namespace lyra::lowering::mir_to_llvm
