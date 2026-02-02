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
#include "lyra/llvm_backend/format_lowering.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerSystemTfRvalue(
    Context& context, const mir::Rvalue& rvalue,
    const mir::SystemTfRvalueInfo& info) -> Result<RvalueValue> {
  switch (info.opcode) {
    case SystemTfOpcode::kFopen: {
      auto& builder = context.GetBuilder();
      const auto& typed_ops = info.typed_operands;

      if (typed_ops.empty() || typed_ops.size() > 2) {
        throw common::InternalError(
            "LowerSystemTfRvalue:kFopen",
            std::format("expected 1 or 2 operands, got {}", typed_ops.size()));
      }

      llvm::Value* result = nullptr;

      // Use nested WithStringHandle for filename (and optionally mode)
      auto status = WithStringHandle(
          context, typed_ops[0].operand, typed_ops[0].type,
          [&](llvm::Value* filename_handle) -> Result<void> {
            if (typed_ops.size() == 2) {
              // FD mode: $fopen(filename, mode) - nested WithStringHandle
              return WithStringHandle(
                  context, typed_ops[1].operand, typed_ops[1].type,
                  [&](llvm::Value* mode_handle) -> Result<void> {
                    result = builder.CreateCall(
                        context.GetLyraFopenFd(),
                        {context.GetEnginePointer(), filename_handle,
                         mode_handle},
                        "fopen.fd");
                    return {};
                  });
            }
            // MCD mode: $fopen(filename)
            result = builder.CreateCall(
                context.GetLyraFopenMcd(),
                {context.GetEnginePointer(), filename_handle}, "fopen.mcd");
            return {};
          });

      if (!status) return std::unexpected(status.error());
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
    case SystemTfOpcode::kRandom: {
      auto& builder = context.GetBuilder();
      llvm::Value* result = builder.CreateCall(
          context.GetLyraRandom(), {context.GetEnginePointer()}, "random");
      return RvalueValue::TwoState(result);
    }
    case SystemTfOpcode::kUrandom: {
      auto& builder = context.GetBuilder();
      llvm::Value* result = builder.CreateCall(
          context.GetLyraUrandom(), {context.GetEnginePointer()}, "urandom");
      return RvalueValue::TwoState(result);
    }
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
    case SystemTfOpcode::kRandom:
      throw common::InternalError(
          "LowerSystemTfEffect", "$random is an rvalue, not an effect");
    case SystemTfOpcode::kUrandom:
      throw common::InternalError(
          "LowerSystemTfEffect", "$urandom is an rvalue, not an effect");
  }
  throw common::InternalError(
      "LowerSystemTfEffect", "unhandled SystemTfOpcode");
}

}  // namespace lyra::lowering::mir_to_llvm
