#include "lyra/llvm_backend/instruction/call.hpp"

#include <expected>
#include <vector>

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerCall(Context& context, const mir::Call& call) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& func = arena[call.callee];

  llvm::Function* callee = context.GetUserFunction(call.callee);
  TypeId return_type = func.signature.return_type;
  bool uses_sret = context.FunctionUsesSret(call.callee);
  bool is_void = callee->getReturnType()->isVoidTy() && !uses_sret;

  // Lower arguments
  std::vector<llvm::Value*> args;

  // For sret, first arg is out-param pointer
  if (uses_sret) {
    auto dest_ptr = context.GetPlacePointer(*call.dest);
    if (!dest_ptr) return std::unexpected(dest_ptr.error());

    // CONTRACT: Out-param calling convention for managed returns.
    // - Caller (here): Destroy() any existing value, making out slot
    //   uninitialized
    // - Callee: MUST fully initialize the out slot via MoveInit before
    //   returning
    // - Valid values include nullptr (represents empty string/container)
    // Do NOT remove this Destroy - callee assumes uninitialized destination.
    Destroy(context, *dest_ptr, return_type);
    args.push_back(*dest_ptr);
  }

  // Design pointer and engine pointer
  args.push_back(context.GetDesignPointer());
  args.push_back(context.GetEnginePointer());

  // User arguments
  for (const auto& operand : call.args) {
    auto val_result = LowerOperandRaw(context, operand);
    if (!val_result) return std::unexpected(val_result.error());
    args.push_back(*val_result);
  }

  // Emit the call
  llvm::Value* call_result = builder.CreateCall(callee, args);

  // Handle result
  if (is_void || !call.dest.has_value()) {
    // Void call or call for side effects only
    return {};
  }

  if (uses_sret) {
    // Result already written to dest by callee via out-param
    return {};
  }

  // Register return: commit result to dest
  // Function returns always move ownership (no source to clone from)
  return CommitValue(
      context, *call.dest, call_result, return_type, OwnershipPolicy::kMove);
}

}  // namespace lyra::lowering::mir_to_llvm
