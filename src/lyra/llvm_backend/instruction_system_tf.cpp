#include "lyra/llvm_backend/instruction_system_tf.hpp"

#include <expected>
#include <format>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerSystemTfRvalue(
    Context& context, const mir::Compute& compute,
    const mir::SystemTfRvalueInfo& info) -> Result<void> {
  switch (info.opcode) {
    case SystemTfOpcode::kFopen: {
      auto& builder = context.GetBuilder();
      const auto& rv = compute.value;

      if (rv.operands.size() != 1 && rv.operands.size() != 2) {
        throw common::InternalError(
            "LowerSystemTfRvalue:kFopen",
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

      auto target_ptr = context.GetPlacePointer(compute.target);
      if (!target_ptr) return std::unexpected(target_ptr.error());
      builder.CreateStore(result, *target_ptr);
      return {};
    }
    case SystemTfOpcode::kFclose:
      throw common::InternalError(
          "LowerSystemTfRvalue", "$fclose is an effect, not an rvalue");
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
    case SystemTfOpcode::kFopen:
      throw common::InternalError(
          "LowerSystemTfEffect", "$fopen is an rvalue, not an effect");
  }
  throw common::InternalError(
      "LowerSystemTfEffect", "unhandled SystemTfOpcode");
}

}  // namespace lyra::lowering::mir_to_llvm
