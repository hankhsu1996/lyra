#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/packed_storage_view.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace detail {

void CommitPlainField(Context& ctx, llvm::Value* ptr, llvm::Value* value) {
  ctx.GetBuilder().CreateStore(value, ptr);
}

}  // namespace detail

void CommitPackedValue(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    const PackedRValue& rvalue, TypeId type_id) {
  auto wt_or_err = commit::Access::GetWriteTarget(ctx, target);
  if (!wt_or_err) {
    throw common::InternalError(
        "CommitPackedValue", "failed to resolve WriteTarget for target");
  }
  const auto& wt = *wt_or_err;

  // Non-design target (process-local alloca): direct LLVM store.
  // This path is outside the packed-store policy architecture. Process locals
  // have no canonical storage, no dirty tracking, no unknown-plane semantics.
  if (!wt.canonical_signal_id.has_value()) {
    // Non-design path only reachable for PlaceId targets (process-local).
    auto* place_id = std::get_if<mir::PlaceId>(&target);
    if (place_id == nullptr) {
      throw common::InternalError(
          "CommitPackedValue",
          "non-design target from ExternalRefId (should be unreachable)");
    }
    auto* raw = ConvertPackedRValueToLegacyLlvmValue(
        ctx, rvalue, ctx.GetPlaceLlvmType(*place_id).value());
    ctx.GetBuilder().CreateStore(raw, wt.ptr);
    return;
  }

  // Design target: route through PSV with store plan.
  auto view = BuildWholeValueStorageView(ctx, facts, wt.ptr, type_id, true);
  auto policy = BuildStorePolicyFromContext(
      ctx, wt.canonical_signal_id,
      wt.mutation_signal ? &*wt.mutation_signal : nullptr);
  // Honor WriteTarget's static propagation override. External ref targets
  // force unconditional propagation because design-level contract bitmaps
  // may not cover instance-owned slots.
  if (wt.requires_static_dirty_propagation) {
    policy.requires_static_dirty_propagation = true;
  }

  auto result = StorePackedValue(ctx, view, rvalue, policy);
  if (!result) {
    throw common::InternalError("CommitPackedValue", "StorePackedValue failed");
  }
}

}  // namespace lyra::lowering::mir_to_llvm
