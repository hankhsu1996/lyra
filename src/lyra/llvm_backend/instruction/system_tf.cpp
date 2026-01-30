#include "lyra/llvm_backend/instruction/system_tf.hpp"

#include <expected>
#include <format>

#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerSystemTfRvalueValue(
    Context& context, const mir::Compute& compute,
    const mir::SystemTfRvalueInfo& info) -> Result<llvm::Value*> {
  switch (info.opcode) {
    case SystemTfOpcode::kFopen: {
      auto& builder = context.GetBuilder();
      const auto& rv = compute.value;

      if (rv.operands.size() != 1 && rv.operands.size() != 2) {
        throw common::InternalError(
            "LowerSystemTfRvalueValue:kFopen",
            std::format(
                "expected 1 or 2 operands, got {}", rv.operands.size()));
      }

      auto filename_or_err = LowerOperand(context, rv.operands[0]);
      if (!filename_or_err) return std::unexpected(filename_or_err.error());

      llvm::Value* result = nullptr;
      if (rv.operands.size() == 2) {
        auto mode_or_err = LowerOperand(context, rv.operands[1]);
        if (!mode_or_err) return std::unexpected(mode_or_err.error());
        result = builder.CreateCall(
            context.GetLyraFopenFd(),
            {context.GetEnginePointer(), *filename_or_err, *mode_or_err},
            "fopen.fd");
      } else {
        result = builder.CreateCall(
            context.GetLyraFopenMcd(),
            {context.GetEnginePointer(), *filename_or_err}, "fopen.mcd");
      }

      return result;
    }
    case SystemTfOpcode::kFclose:
      throw common::InternalError(
          "LowerSystemTfRvalueValue", "$fclose is an effect, not an rvalue");
    case SystemTfOpcode::kFflush:
      throw common::InternalError(
          "LowerSystemTfRvalueValue", "$fflush is an effect, not an rvalue");
  }
  throw common::InternalError(
      "LowerSystemTfRvalueValue", "unhandled SystemTfOpcode");
}

auto LowerSystemTfEffect(Context& context, const mir::SystemTfEffect& effect)
    -> Result<void> {
  switch (effect.opcode) {
    case SystemTfOpcode::kFclose: {
      if (effect.args.size() != 1) {
        throw common::InternalError(
            "LowerSystemTfEffect:kFclose",
            std::format("expected 1 arg, got {}", effect.args.size()));
      }
      auto& builder = context.GetBuilder();
      auto desc_or_err = LowerOperand(context, effect.args[0]);
      if (!desc_or_err) return std::unexpected(desc_or_err.error());
      builder.CreateCall(
          context.GetLyraFclose(), {context.GetEnginePointer(), *desc_or_err});
      return {};
    }
    case SystemTfOpcode::kFflush: {
      auto& builder = context.GetBuilder();
      auto* i1_ty = llvm::Type::getInt1Ty(context.GetLlvmContext());
      auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
      llvm::Value* has_desc = nullptr;
      llvm::Value* desc_val = nullptr;
      if (effect.args.empty()) {
        has_desc = llvm::ConstantInt::get(i1_ty, 0);
        desc_val = llvm::ConstantInt::get(i32_ty, 0);
      } else {
        has_desc = llvm::ConstantInt::get(i1_ty, 1);
        auto desc_or_err = LowerOperand(context, effect.args[0]);
        if (!desc_or_err) return std::unexpected(desc_or_err.error());
        desc_val = *desc_or_err;
      }
      builder.CreateCall(
          context.GetLyraFflush(),
          {context.GetEnginePointer(), has_desc, desc_val});
      return {};
    }
    case SystemTfOpcode::kFopen:
      throw common::InternalError(
          "LowerSystemTfEffect", "$fopen is an rvalue, not an effect");
  }
  throw common::InternalError(
      "LowerSystemTfEffect", "unhandled SystemTfOpcode");
}

}  // namespace lyra::lowering::mir_to_llvm
