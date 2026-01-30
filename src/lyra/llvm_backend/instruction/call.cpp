#include "lyra/llvm_backend/instruction/call.hpp"

#include <expected>
#include <format>
#include <vector>

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto LowerUserCall(
    Context& context, const mir::Call& call, mir::FunctionId func_id)
    -> Result<void> {
  // User function writebacks not yet supported - fail loud to match interpreter
  if (!call.writebacks.empty()) {
    throw common::InternalError(
        "LowerUserCall", "User function writebacks not yet supported");
  }

  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& func = arena[func_id];

  llvm::Function* callee = context.GetUserFunction(func_id);
  TypeId return_type = func.signature.return_type;
  bool uses_sret = context.FunctionUsesSret(func_id);
  bool is_void = callee->getReturnType()->isVoidTy() && !uses_sret;

  // Lower arguments
  std::vector<llvm::Value*> args;

  // For sret, first arg is out-param pointer - use ret.tmp as staging
  if (uses_sret && call.ret) {
    auto tmp_ptr = context.GetPlacePointer(call.ret->tmp);
    if (!tmp_ptr) return std::unexpected(tmp_ptr.error());

    // CONTRACT: Out-param calling convention for managed returns.
    //
    // Caller (here):
    //   Destroy() any existing value, making the out slot uninitialized.
    //   This is the ONLY place where we deliberately create uninitialized
    //   storage at an ABI boundary.
    //
    // Callee (DefineUserFunction exit block):
    //   MUST fully initialize the out slot via MoveInit before returning.
    //   Valid values include nullptr (represents empty string/container).
    //
    // Do NOT remove this Destroy - callee uses MoveInit which requires dst
    // to be uninitialized.
    Destroy(context, *tmp_ptr, return_type);
    args.push_back(*tmp_ptr);
  }

  // Design pointer and engine pointer
  args.push_back(context.GetDesignPointer());
  args.push_back(context.GetEnginePointer());

  // User arguments
  for (const auto& operand : call.in_args) {
    auto val_result = LowerOperandRaw(context, operand);
    if (!val_result) return std::unexpected(val_result.error());
    args.push_back(*val_result);
  }

  // Emit the call
  llvm::Value* call_result = builder.CreateCall(callee, args);

  // Handle result
  if (is_void || !call.ret) {
    // Void call or call for side effects only
    return {};
  }

  if (uses_sret) {
    // Result already written to ret.tmp by callee via out-param
    // Now commit to dest if statement form
    if (call.ret->dest.has_value()) {
      auto tmp_ptr = context.GetPlacePointer(call.ret->tmp);
      if (!tmp_ptr) return std::unexpected(tmp_ptr.error());
      auto tmp_type = context.GetPlaceLlvmType(call.ret->tmp);
      if (!tmp_type) return std::unexpected(tmp_type.error());
      llvm::Value* ret_val = builder.CreateLoad(*tmp_type, *tmp_ptr);
      return CommitValue(
          context, *call.ret->dest, ret_val, return_type,
          OwnershipPolicy::kMove);
    }
    // Expression form: Use(ret.tmp) handled by outer Assign
    return {};
  }

  // Register return: store to tmp, then commit to dest if statement form
  auto tmp_ptr = context.GetPlacePointer(call.ret->tmp);
  if (!tmp_ptr) return std::unexpected(tmp_ptr.error());
  builder.CreateStore(call_result, *tmp_ptr);

  if (call.ret->dest.has_value()) {
    return CommitValue(
        context, *call.ret->dest, call_result, return_type,
        OwnershipPolicy::kMove);
  }
  // Expression form: Use(ret.tmp) handled by outer Assign
  return {};
}

auto LowerSystemTfCall(
    Context& context, const mir::Call& call, SystemTfOpcode opcode)
    -> Result<void> {
  switch (opcode) {
    case SystemTfOpcode::kValuePlusargs:
      return LowerValuePlusargsCall(context, call);
    default:
      throw common::InternalError(
          "LowerSystemTfCall", std::format(
                                   "Unhandled system TF opcode in Call: {}",
                                   static_cast<int>(opcode)));
  }
}

}  // namespace

auto LowerCall(Context& context, const mir::Call& call) -> Result<void> {
  return std::visit(
      common::Overloaded{
          [&](mir::FunctionId func_id) -> Result<void> {
            return LowerUserCall(context, call, func_id);
          },
          [&](SystemTfOpcode opcode) -> Result<void> {
            return LowerSystemTfCall(context, call, opcode);
          },
      },
      call.callee);
}

auto LowerValuePlusargsCall(Context& context, const mir::Call& call)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  // Validate shape: 1 in_arg (query), optional ret (success), 1 writeback
  // (output)
  if (call.in_args.size() != 1) {
    throw common::InternalError(
        "LowerValuePlusargsCall",
        std::format("expected 1 in_arg, got {}", call.in_args.size()));
  }
  if (call.writebacks.size() != 1) {
    throw common::InternalError(
        "LowerValuePlusargsCall",
        std::format("expected 1 writeback, got {}", call.writebacks.size()));
  }

  // Lower query operand
  auto query_or_err = LowerOperand(context, call.in_args[0]);
  if (!query_or_err) return std::unexpected(query_or_err.error());

  // Get output tmp pointer
  const auto& wb = call.writebacks[0];
  auto output_tmp_ptr = context.GetPlacePointer(wb.tmp);
  if (!output_tmp_ptr) return std::unexpected(output_tmp_ptr.error());

  // Determine output kind and call runtime helper
  TypeId output_type = wb.type;
  bool is_string = types[output_type].Kind() == TypeKind::kString;

  llvm::Function* helper = is_string ? context.GetLyraPlusargsValueString()
                                     : context.GetLyraPlusargsValueInt();
  llvm::Value* success = builder.CreateCall(
      helper, {context.GetEnginePointer(), *query_or_err, *output_tmp_ptr});

  // Stage return (success) to tmp, then commit if statement form
  if (call.ret) {
    auto ret_tmp_ptr = context.GetPlacePointer(call.ret->tmp);
    if (!ret_tmp_ptr) return std::unexpected(ret_tmp_ptr.error());
    builder.CreateStore(success, *ret_tmp_ptr);

    // Commit only if statement form (dest has value)
    if (call.ret->dest.has_value()) {
      auto result = CommitValue(
          context, *call.ret->dest, success, call.ret->type,
          OwnershipPolicy::kMove);
      if (!result) return result;
    }
    // Expression form: Use(ret.tmp) handled by outer Assign
  }

  // Commit writeback ONLY if success (helper only writes to tmp on match)
  // This preserves $value$plusargs semantics: no match = no modification
  auto* func = builder.GetInsertBlock()->getParent();
  llvm::BasicBlock* commit_bb =
      llvm::BasicBlock::Create(builder.getContext(), "vp_commit", func);
  llvm::BasicBlock* merge_bb =
      llvm::BasicBlock::Create(builder.getContext(), "vp_merge", func);

  llvm::Value* is_match = builder.CreateICmpNE(
      success, llvm::ConstantInt::get(success->getType(), 0));
  builder.CreateCondBr(is_match, commit_bb, merge_bb);

  builder.SetInsertPoint(commit_bb);
  auto output_llvm_type = context.GetPlaceLlvmType(wb.tmp);
  if (!output_llvm_type) return std::unexpected(output_llvm_type.error());
  llvm::Value* output_val =
      builder.CreateLoad(*output_llvm_type, *output_tmp_ptr);
  auto commit_result = CommitValue(
      context, wb.dest, output_val, output_type, OwnershipPolicy::kMove);
  if (!commit_result) return commit_result;
  builder.CreateBr(merge_bb);

  builder.SetInsertPoint(merge_bb);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
