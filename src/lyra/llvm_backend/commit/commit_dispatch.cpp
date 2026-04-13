#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/lifecycle/detail.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

void CommitMoveCleanupIfTemp(
    Context& ctx, const CuFacts& facts, mir::PlaceId source,
    OwnershipPolicy policy, TypeId type_id) {
  // Gate 1: Only kMove requires null-out
  if (policy != OwnershipPolicy::kMove) {
    return;
  }

  const auto& arena = ctx.GetMirArena();
  const auto& src_place = arena[source];

  // Gate 2 + Enforcement: Source place root must be kTemp.
  // kMove from non-temp is a bug (MIR only emits MoveAssign for temp sources).
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
  MoveCleanup(ctx, facts, *src_ptr_result, type_id);
}

}  // namespace lyra::lowering::mir_to_llvm
