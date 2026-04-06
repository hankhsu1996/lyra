#include "lyra/llvm_backend/deferred_thunk_abi.hpp"

#include <array>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/callable_abi.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"

namespace lyra::lowering::mir_to_llvm {

auto BuildDeferredExecContextType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  // Layout matches DeferredAssertionExecContext: { RuntimeInstance* instance }
  std::array<llvm::Type*, 1> fields = {ptr_ty};
  return llvm::StructType::get(ctx, fields);
}

auto BuildDeferredPayloadStructType(
    llvm::LLVMContext& llvm_ctx, const mir::CapturePayloadDesc& payload,
    const TypeArena& types, bool force_two_state) -> llvm::StructType* {
  std::vector<llvm::Type*> field_llvm_types;
  field_llvm_types.reserve(payload.field_types.size());
  for (TypeId field_type : payload.field_types) {
    auto* llvm_type =
        GetCallableAbiLlvmType(llvm_ctx, field_type, types, force_two_state);
    if (llvm_type == nullptr) {
      throw common::InternalError(
          "BuildDeferredPayloadStructType",
          "aggregate type in deferred payload (not supported)");
    }
    field_llvm_types.push_back(llvm_type);
  }
  return llvm::StructType::get(llvm_ctx, field_llvm_types);
}

}  // namespace lyra::lowering::mir_to_llvm
