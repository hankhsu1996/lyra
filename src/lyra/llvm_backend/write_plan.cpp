#include "lyra/llvm_backend/write_plan.hpp"

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"

namespace lyra::lowering::mir_to_llvm {

auto ClassifyWriteShape(TypeId type_id, const TypeArena& types) -> WriteShape {
  const Type& type = types[type_id];

  // 1. Top-level managed handle types
  if (GetManagedKind(type.Kind()) != ManagedKind::kNone) {
    return WriteShape::kManagedScalar;
  }

  // 2. Pointer-like scalar (chandle)
  if (type.Kind() == TypeKind::kChandle) {
    return WriteShape::kPointerScalar;
  }

  // 2. Unpacked union
  if (type.Kind() == TypeKind::kUnpackedUnion) {
    return WriteShape::kUnionMemcpy;
  }

  // 3. Unpacked struct / unpacked array
  if (type.Kind() == TypeKind::kUnpackedStruct ||
      type.Kind() == TypeKind::kUnpackedArray) {
    if (NeedsFieldByField(type_id, types)) {
      return WriteShape::kFieldByFieldAggregate;
    }
    if (TypeContainsManaged(type_id, types)) {
      return WriteShape::kUnsupportedManagedAggregate;
    }
    return WriteShape::kPlainUnpackedAggregate;
  }

  // 4. Everything else: packed integral, enum, real, shortreal
  return WriteShape::kPackedOrFloatScalar;
}

auto BuildWritePlan(TypeId type_id, const TypeArena& types) -> WritePlan {
  auto shape = ClassifyWriteShape(type_id, types);
  WriteOp op{};

  switch (shape) {
    case WriteShape::kManagedScalar:
      op = WriteOp::kCommitManagedScalar;
      break;
    case WriteShape::kPointerScalar:
      op = WriteOp::kCommitPointerScalar;
      break;
    case WriteShape::kPlainUnpackedAggregate:
      op = WriteOp::kStorePlainAggregate;
      break;
    case WriteShape::kFieldByFieldAggregate: {
      const Type& type = types[type_id];
      if (type.Kind() == TypeKind::kUnpackedStruct) {
        op = WriteOp::kCommitFieldByFieldStruct;
      } else {
        op = WriteOp::kCommitFieldByFieldArray;
      }
      break;
    }
    case WriteShape::kUnsupportedManagedAggregate:
      op = WriteOp::kRejectUnsupported;
      break;
    case WriteShape::kPackedOrFloatScalar:
      op = WriteOp::kCommitPackedOrFloatScalar;
      break;
    case WriteShape::kUnionMemcpy:
      op = WriteOp::kCommitUnionMemcpy;
      break;
  }

  return WritePlan{.op = op, .shape = shape, .type_id = type_id};
}

}  // namespace lyra::lowering::mir_to_llvm
