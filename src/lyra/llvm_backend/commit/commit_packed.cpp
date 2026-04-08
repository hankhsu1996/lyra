#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/packed_storage_view.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace detail {

void CommitPlainField(Context& ctx, llvm::Value* ptr, llvm::Value* value) {
  ctx.GetBuilder().CreateStore(value, ptr);
}

}  // namespace detail

void CommitPackedValue(
    Context& ctx, mir::PlaceId target, const PackedRValue& rvalue,
    TypeId type_id) {
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
    auto* raw = ConvertPackedRValueToLegacyLlvmValue(
        ctx, rvalue, ctx.GetPlaceLlvmType(target).value());
    ctx.GetBuilder().CreateStore(raw, wt.ptr);
    return;
  }

  // Design target: route through PSV with store plan.
  auto view = BuildWholeValueStorageView(ctx, wt.ptr, type_id, true);
  auto policy = BuildStorePolicyFromContext(
      ctx, wt.canonical_signal_id,
      wt.mutation_signal ? &*wt.mutation_signal : nullptr);

  auto result = StorePackedValue(ctx, view, rvalue, policy);
  if (!result) {
    throw common::InternalError("CommitPackedValue", "StorePackedValue failed");
  }
}

}  // namespace lyra::lowering::mir_to_llvm
