#include <expected>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

// Forward declarations for family-specific implementations (internal linkage in
// their respective files)
auto CommitStringValue(
    Context& ctx, const WriteTarget& wt, llvm::Value* handle,
    OwnershipPolicy policy, TypeId type_id) -> Result<void>;
auto CommitContainerValue(
    Context& ctx, const WriteTarget& wt, llvm::Value* handle,
    OwnershipPolicy policy, TypeId type_id) -> Result<void>;
auto CommitPackedValue(
    Context& ctx, const WriteTarget& wt, llvm::Value* raw, TypeId type_id)
    -> Result<void>;

auto CommitValue(
    Context& ctx, mir::PlaceId target, llvm::Value* raw_value, TypeId type_id,
    OwnershipPolicy policy) -> Result<void> {
  // Resolve WriteTarget internally - callers pass PlaceId, not WriteTarget
  auto wt_or_err = commit::Access::GetWriteTarget(ctx, target);
  if (!wt_or_err) return std::unexpected(wt_or_err.error());
  const WriteTarget& wt = *wt_or_err;

  const auto& types = ctx.GetTypeArena();
  const Type& type = types[type_id];

  // Managed types: ownership matters
  switch (GetManagedKind(type.Kind())) {
    case ManagedKind::kString:
      return CommitStringValue(ctx, wt, raw_value, policy, type_id);
    case ManagedKind::kContainer:
      return CommitContainerValue(ctx, wt, raw_value, policy, type_id);
    case ManagedKind::kNone:
      break;
  }

  // Non-managed types: ownership is no-op (kClone == kMove == plain store)
  return CommitPackedValue(ctx, wt, raw_value, type_id);
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
