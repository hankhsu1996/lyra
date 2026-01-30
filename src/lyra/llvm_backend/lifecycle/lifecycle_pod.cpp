#include "lyra/llvm_backend/lifecycle/lifecycle_pod.hpp"

#include <llvm/IR/Value.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

void CopyInitPod(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id) {
  auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, type_id);
  if (!llvm_type_result) {
    throw common::InternalError(
        "CopyInitPod", "failed to build LLVM type for POD type");
  }
  auto& builder = ctx.GetBuilder();
  llvm::Value* val = builder.CreateLoad(*llvm_type_result, src_ptr, "copy.val");
  builder.CreateStore(val, dst_ptr);
}

}  // namespace lyra::lowering::mir_to_llvm::detail
