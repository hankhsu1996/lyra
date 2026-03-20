#include <cstdint>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
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

void CommitPackedValueRaw(
    Context& ctx, mir::PlaceId target, llvm::Value* value, TypeId type_id) {
  auto wt_or_err = commit::Access::GetWriteTarget(ctx, target);
  if (!wt_or_err) {
    throw common::InternalError(
        "CommitPackedValueRaw", "failed to resolve WriteTarget for target");
  }
  const auto& wt = *wt_or_err;

  // Non-design target: direct LLVM store (no notification needed).
  if (!wt.canonical_signal_id.has_value()) {
    ctx.GetBuilder().CreateStore(value, wt.ptr);
    return;
  }

  // Design target: route through PSV.
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[type_id];
  auto kind = type.Kind();
  if (kind == TypeKind::kEnum) {
    kind = types[type.AsEnum().base_type].Kind();
  }

  uint32_t semantic_bits = 0;
  bool is_four_state = false;
  llvm::Value* store_value = value;

  if (kind == TypeKind::kReal) {
    // Real (double): bitcast to i64, treat as 2-state 64-bit packed.
    semantic_bits = 64;
    is_four_state = false;
    store_value = ctx.GetBuilder().CreateBitCast(
        value, llvm::Type::getInt64Ty(ctx.GetLlvmContext()), "real.as.i64");
  } else if (kind == TypeKind::kShortReal) {
    // ShortReal (float): bitcast to i32, treat as 2-state 32-bit packed.
    semantic_bits = 32;
    is_four_state = false;
    store_value = ctx.GetBuilder().CreateBitCast(
        value, llvm::Type::getInt32Ty(ctx.GetLlvmContext()),
        "shortreal.as.i32");
  } else {
    semantic_bits = PackedBitWidth(type, types);
    is_four_state = ctx.IsPackedFourState(type);
  }

  auto rvalue =
      ConvertRawToPackedRValue(ctx, store_value, semantic_bits, is_four_state);
  auto view = BuildWholeValueStorageView(ctx, wt.ptr, type_id, true);
  auto policy = BuildStorePolicyFromContext(ctx, wt.canonical_signal_id);

  auto result = StorePackedValue(ctx, view, rvalue, policy);
  if (!result) {
    throw common::InternalError(
        "CommitPackedValueRaw", "StorePackedValue failed");
  }
}

}  // namespace lyra::lowering::mir_to_llvm
