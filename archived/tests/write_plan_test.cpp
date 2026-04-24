#include "lyra/llvm_backend/write_plan.hpp"

#include <gtest/gtest.h>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

namespace lyra::lowering::mir_to_llvm {
namespace {

struct WritePlanTest : public testing::Test {
  TypeArena types;

  auto MakeInt32() -> TypeId {
    return types.Intern(
        TypeKind::kIntegral,
        IntegralInfo{
            .bit_width = 32, .is_signed = true, .is_four_state = false});
  }

  auto MakeLogic32() -> TypeId {
    return types.Intern(
        TypeKind::kIntegral,
        IntegralInfo{
            .bit_width = 32, .is_signed = false, .is_four_state = true});
  }

  auto MakeReal() -> TypeId {
    return types.Intern(TypeKind::kReal, std::monostate{});
  }

  auto MakeString() -> TypeId {
    return types.Intern(TypeKind::kString, std::monostate{});
  }

  auto MakeDynArray(TypeId elem) -> TypeId {
    return types.Intern(
        TypeKind::kDynamicArray, DynamicArrayInfo{.element_type = elem});
  }

  auto MakeQueue(TypeId elem) -> TypeId {
    return types.Intern(
        TypeKind::kQueue, QueueInfo{.element_type = elem, .max_bound = 0});
  }

  auto MakeAssocArray(TypeId elem, TypeId key) -> TypeId {
    return types.Intern(
        TypeKind::kAssociativeArray,
        AssociativeArrayInfo{.element_type = elem, .key_type = key});
  }

  auto MakeUnpackedStruct(std::string name, std::vector<StructField> fields)
      -> TypeId {
    return types.Intern(
        TypeKind::kUnpackedStruct,
        UnpackedStructInfo{
            .name = std::move(name), .fields = std::move(fields)});
  }

  auto MakeUnpackedArray(TypeId elem, uint32_t size) -> TypeId {
    return types.Intern(
        TypeKind::kUnpackedArray,
        UnpackedArrayInfo{
            .element_type = elem,
            .range = ConstantRange{
                .left = static_cast<int32_t>(size) - 1, .right = 0}});
  }

  auto MakeUnpackedUnion() -> TypeId {
    return types.Intern(
        TypeKind::kUnpackedUnion, UnpackedUnionInfo{
                                      .members = {},
                                      .storage_bit_width = 32,
                                      .contains_float = false,
                                      .storage_is_four_state = false});
  }
};

TEST_F(WritePlanTest, Int32IsPackedScalar) {
  auto id = MakeInt32();
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kPackedOrFloatScalar);
  auto plan = BuildWritePlan(id, types);
  EXPECT_EQ(plan.op, WriteOp::kCommitPackedOrFloatScalar);
}

TEST_F(WritePlanTest, Logic32IsPackedScalar) {
  auto id = MakeLogic32();
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kPackedOrFloatScalar);
}

TEST_F(WritePlanTest, RealIsPackedScalar) {
  auto id = MakeReal();
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kPackedOrFloatScalar);
  auto plan = BuildWritePlan(id, types);
  EXPECT_EQ(plan.op, WriteOp::kCommitPackedOrFloatScalar);
}

TEST_F(WritePlanTest, StringIsManagedScalar) {
  auto id = MakeString();
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kManagedScalar);
  auto plan = BuildWritePlan(id, types);
  EXPECT_EQ(plan.op, WriteOp::kCommitManagedScalar);
}

TEST_F(WritePlanTest, DynArrayIsManagedScalar) {
  auto int_id = MakeInt32();
  auto id = MakeDynArray(int_id);
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kManagedScalar);
}

TEST_F(WritePlanTest, QueueIsManagedScalar) {
  auto int_id = MakeInt32();
  auto id = MakeQueue(int_id);
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kManagedScalar);
}

TEST_F(WritePlanTest, AssocArrayIsManagedScalar) {
  auto int_id = MakeInt32();
  auto id = MakeAssocArray(int_id, int_id);
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kManagedScalar);
}

TEST_F(WritePlanTest, PlainUnpackedStructIsPlainAggregate) {
  auto int_id = MakeInt32();
  auto id = MakeUnpackedStruct(
      "point_t",
      {{.name = "x", .type = int_id}, {.name = "y", .type = int_id}});
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kPlainUnpackedAggregate);
  auto plan = BuildWritePlan(id, types);
  EXPECT_EQ(plan.op, WriteOp::kStorePlainAggregate);
}

TEST_F(WritePlanTest, PlainUnpackedArrayIsPlainAggregate) {
  auto int_id = MakeInt32();
  auto id = MakeUnpackedArray(int_id, 4);
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kPlainUnpackedAggregate);
  auto plan = BuildWritePlan(id, types);
  EXPECT_EQ(plan.op, WriteOp::kStorePlainAggregate);
}

TEST_F(WritePlanTest, StructWithStringFieldIsFieldByField) {
  auto int_id = MakeInt32();
  auto str_id = MakeString();
  auto id = MakeUnpackedStruct(
      "record_t",
      {{.name = "id", .type = int_id}, {.name = "name", .type = str_id}});
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kFieldByFieldAggregate);
  auto plan = BuildWritePlan(id, types);
  EXPECT_EQ(plan.op, WriteOp::kCommitFieldByFieldStruct);
}

TEST_F(WritePlanTest, ArrayWithStringElementIsFieldByField) {
  auto str_id = MakeString();
  auto id = MakeUnpackedArray(str_id, 2);
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kFieldByFieldAggregate);
  auto plan = BuildWritePlan(id, types);
  EXPECT_EQ(plan.op, WriteOp::kCommitFieldByFieldArray);
}

TEST_F(WritePlanTest, UnpackedUnionIsUnionMemcpy) {
  auto id = MakeUnpackedUnion();
  EXPECT_EQ(ClassifyWriteShape(id, types), WriteShape::kUnionMemcpy);
  auto plan = BuildWritePlan(id, types);
  EXPECT_EQ(plan.op, WriteOp::kCommitUnionMemcpy);
}

TEST_F(WritePlanTest, StructWithContainerFieldIsUnsupported) {
  auto int_id = MakeInt32();
  auto dyn_id = MakeDynArray(int_id);
  auto id = MakeUnpackedStruct(
      "bad_t",
      {{.name = "data", .type = dyn_id}, {.name = "count", .type = int_id}});
  EXPECT_EQ(
      ClassifyWriteShape(id, types), WriteShape::kUnsupportedManagedAggregate);
  auto plan = BuildWritePlan(id, types);
  EXPECT_EQ(plan.op, WriteOp::kRejectUnsupported);
}

}  // namespace
}  // namespace lyra::lowering::mir_to_llvm
