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
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerSystemTfRvalue(
    Context& context, const mir::Rvalue& rvalue,
    const mir::SystemTfRvalueInfo& info) -> Result<RvalueValue> {
  switch (info.opcode) {
    case SystemTfOpcode::kFopen: {
      auto& builder = context.GetBuilder();

      if (rvalue.operands.size() != 1 && rvalue.operands.size() != 2) {
        throw common::InternalError(
            "LowerSystemTfRvalue:kFopen",
            std::format(
                "expected 1 or 2 operands, got {}", rvalue.operands.size()));
      }

      auto filename_or_err = LowerOperand(context, rvalue.operands[0]);
      if (!filename_or_err) return std::unexpected(filename_or_err.error());

      llvm::Value* result = nullptr;
      if (rvalue.operands.size() == 2) {
        auto mode_or_err = LowerOperand(context, rvalue.operands[1]);
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

      return RvalueValue::TwoState(result);
    }
    case SystemTfOpcode::kFclose:
      throw common::InternalError(
          "LowerSystemTfRvalue", "$fclose is an effect, not an rvalue");
    case SystemTfOpcode::kFflush:
      throw common::InternalError(
          "LowerSystemTfRvalue", "$fflush is an effect, not an rvalue");
    case SystemTfOpcode::kValuePlusargs:
      throw common::InternalError(
          "LowerSystemTfRvalue",
          "$value$plusargs should be lowered via Call, not SystemTfRvalueInfo");
  }
  throw common::InternalError(
      "LowerSystemTfRvalue", "unhandled SystemTfOpcode");
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
    case SystemTfOpcode::kValuePlusargs:
      throw common::InternalError(
          "LowerSystemTfEffect",
          "$value$plusargs should be lowered via Call, not SystemTfEffect");
  }
  throw common::InternalError(
      "LowerSystemTfEffect", "unhandled SystemTfOpcode");
}

}  // namespace lyra::lowering::mir_to_llvm
