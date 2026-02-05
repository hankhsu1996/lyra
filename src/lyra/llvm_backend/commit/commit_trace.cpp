#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

void EmitTraceMemoryDirtyIfDesignSlot(Context& ctx, mir::PlaceId target) {
  auto slot_id_opt = commit::Access::GetCanonicalRootSignalId(ctx, target);
  if (!slot_id_opt.has_value()) {
    return;
  }

  auto& builder = ctx.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());
  builder.CreateCall(
      ctx.GetLyraTraceMemoryDirty(),
      {ctx.GetEnginePointer(), llvm::ConstantInt::get(i32_ty, *slot_id_opt)});
}

}  // namespace lyra::lowering::mir_to_llvm
