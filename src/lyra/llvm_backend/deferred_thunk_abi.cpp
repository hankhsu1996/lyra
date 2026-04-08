#include "lyra/llvm_backend/deferred_thunk_abi.hpp"

#include <array>
#include <span>

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
  std::array<llvm::Type*, 1> fields = {ptr_ty};
  return llvm::StructType::get(ctx, fields);
}

auto BuildDeferredPayloadStructType(
    llvm::LLVMContext& llvm_ctx, std::span<const TypeId> field_types,
    const TypeArena& types, bool force_two_state) -> llvm::StructType* {
  std::vector<llvm::Type*> field_llvm_types;
  field_llvm_types.reserve(field_types.size());
  for (TypeId field_type : field_types) {
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

auto GetDeferredPassUserCallAction(const mir::DeferredAssertionSiteInfo& site)
    -> const mir::DeferredUserCallAction* {
  if (!site.pass_action.has_value()) return nullptr;
  return std::get_if<mir::DeferredUserCallAction>(&*site.pass_action);
}

auto GetDeferredFailUserCallAction(const mir::DeferredAssertionSiteInfo& site)
    -> const mir::DeferredUserCallAction* {
  if (!site.fail_action.has_value()) return nullptr;
  return std::get_if<mir::DeferredUserCallAction>(&*site.fail_action);
}

auto DeriveDeferredCallPlan(const mir::DeferredUserCallAction& action)
    -> DeferredDerivedCallPlan {
  DeferredDerivedCallPlan out;
  out.callee = action.callee;

  uint32_t payload_index = 0;
  uint32_t ref_index = 0;

  for (const auto& actual : action.actuals) {
    switch (actual.kind) {
      case mir::DeferredActualKind::kSnapshotValue:
        out.payload.field_types.push_back(actual.type);
        out.actuals.push_back({
            .kind = DeferredBindingKind::kPayloadField,
            .payload_index = payload_index++,
        });
        break;

      case mir::DeferredActualKind::kLiveRef:
        out.actuals.push_back({
            .kind = DeferredBindingKind::kLiveRef,
            .ref_index = ref_index++,
            .ref_place = actual.ref_place,
        });
        break;

      case mir::DeferredActualKind::kConstLiveRef:
        out.actuals.push_back({
            .kind = DeferredBindingKind::kConstLiveRef,
            .ref_index = ref_index++,
            .ref_place = actual.ref_place,
        });
        break;
    }
  }

  out.ref_count = ref_index;
  return out;
}

}  // namespace lyra::lowering::mir_to_llvm
