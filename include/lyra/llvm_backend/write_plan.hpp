#pragma once

#include <variant>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/packed_storage_view.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Canonical semantic classification of write strategy.
// Single backend-wide authority. No callsite may re-derive write shape
// from ad hoc TypeKind / IsPacked / NeedsFieldByField branching.
enum class WriteShape {
  kManagedScalar,                // string / dyn array / queue / assoc array
  kPlainUnpackedAggregate,       // unpacked struct/array, bytewise-storable
  kFieldByFieldAggregate,        // unpacked struct/array with managed fields
  kUnsupportedManagedAggregate,  // aggregate with container members (deferred)
  kPackedOrFloatScalar,          // packed integral / enum / real / shortreal
  kUnionMemcpy,                  // unpacked union, memcpy-based write path
};

// Directly-actionable write operation. Resolves shape + type-kind ambiguity
// (e.g. struct vs array within kFieldByFieldAggregate) at planning time so
// the executor never branches on TypeKind.
enum class WriteOp {
  kCommitManagedScalar,
  kStorePlainAggregate,
  kCommitPackedOrFloatScalar,
  kCommitFieldByFieldStruct,
  kCommitFieldByFieldArray,
  kCommitUnionMemcpy,
  kRejectUnsupported,
};

// Executable write plan. Built once from type semantics, consumed by the
// source-aware executor without re-classification.
struct WritePlan {
  WriteOp op = {};
  WriteShape shape = {};
  TypeId type_id;
};

// Build a write plan from type semantics. This is the single place that
// converts type properties into an executable write route.
auto ClassifyWriteShape(TypeId type_id, const TypeArena& types) -> WriteShape;
auto BuildWritePlan(TypeId type_id, const TypeArena& types) -> WritePlan;

// Source form descriptors. Callers provide what they have; the executor
// adapts to the required form for the planned write operation.
struct RawValueSource {
  llvm::Value* value;
};

struct OperandSource {
  const mir::Operand* operand;
};

// Non-lossy packed RHS carrier for packed-store paths.
// Bypasses the raw llvm::Value* transport to preserve 2-state/4-state
// semantics (unk == nullptr means provably 2-state).
struct PackedRValueSource {
  PackedRValue rvalue;
  TypeId type_id;
};

using WriteSource =
    std::variant<RawValueSource, OperandSource, PackedRValueSource>;

// Source-aware write dispatcher. Builds a plan and executes it.
// This is the canonical dispatch boundary. All write paths route here.
auto DispatchWrite(
    Context& ctx, mir::PlaceId target, const WriteSource& source,
    TypeId type_id, OwnershipPolicy policy) -> Result<void>;

// Execute a pre-built write plan against a source.
// Separated from DispatchWrite so callers (e.g. LowerRvalueAssign) can
// inspect the plan before execution for special-case routing.
auto ExecuteWritePlan(
    Context& ctx, mir::PlaceId target, const WriteSource& source,
    const WritePlan& plan, OwnershipPolicy policy) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
