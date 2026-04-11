#include <expected>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/lifecycle/detail.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/write_plan.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

// Raw-value commit adapter. Routes through DispatchWrite with RawValueSource.
// Does not own semantic write routing -- that lives in
// write_plan/write_dispatch. Retained for backward compatibility with callers
// (assoc_op, call, etc.) that have an already-loaded llvm::Value*.
auto CommitValue(
    Context& ctx, const CuFacts& facts, mir::PlaceId target,
    llvm::Value* raw_value, TypeId type_id, OwnershipPolicy policy)
    -> Result<void> {
  return DispatchWrite(
      ctx, facts, mir::WriteTarget{target}, RawValueSource{raw_value}, type_id,
      policy);
}

void CommitMoveCleanupIfTemp(
    Context& ctx, mir::PlaceId source, OwnershipPolicy policy, TypeId type_id) {
  // Gate 1: Only kMove requires null-out
  if (policy != OwnershipPolicy::kMove) {
    return;
  }

  const auto& arena = ctx.GetMirArena();
  const auto& src_place = arena[source];

  // Gate 2 + Enforcement: Source place root must be kTemp
  // kMove from non-temp is a bug (DetermineOwnership only returns kMove for
  // temps)
  if (src_place.root.kind != mir::PlaceRoot::Kind::kTemp) {
    throw common::InternalError(
        "CommitMoveCleanupIfTemp", "kMove from non-temp source");
  }

  // Get source pointer
  auto src_ptr_result = ctx.GetPlacePointer(source);
  if (!src_ptr_result) {
    throw common::InternalError(
        "CommitMoveCleanupIfTemp", "failed to get source place pointer");
  }

  // Null out managed fields in source (delegate to lifecycle module)
  MoveCleanup(ctx, *src_ptr_result, type_id);
}

}  // namespace lyra::lowering::mir_to_llvm
