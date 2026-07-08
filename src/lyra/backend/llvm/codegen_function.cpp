#include "lyra/backend/llvm/codegen_function.hpp"

#include <cstdint>
#include <format>
#include <variant>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/base/overloaded.hpp"

namespace lyra::backend::llvm_backend {

CodeGenFunction::CodeGenFunction(
    CodeGenModule& module, const lir::Function& fn, llvm::Function* value)
    : module_(&module), fn_(&fn), value_(value), builder_(module.Context()) {
}

void CodeGenFunction::Run() {
  for (std::uint32_t i = 0; i < fn_->params.size(); ++i) {
    values_.emplace(fn_->params[i].value, value_->getArg(i));
  }
  for (std::uint32_t i = 0; i < fn_->blocks.size(); ++i) {
    auto* bb = llvm::BasicBlock::Create(
        module_->Context(), std::format("bb{}", i), value_);
    builder_.SetInsertPoint(bb);
    for (const lir::Instr& instr : fn_->blocks[i].instrs) {
      values_.emplace(instr.result.value, LowerInstr(instr));
    }
    LowerTerminator(fn_->blocks[i].terminator);
  }
}

void CodeGenFunction::LowerTerminator(const lir::Terminator& terminator) {
  std::visit(
      Overloaded{[&](const lir::ReturnTerm& ret) {
        if (value_->getReturnType()->isVoidTy()) {
          builder_.CreateRetVoid();
          return;
        }
        builder_.CreateRet(LowerOperand(*ret.value));
      }},
      terminator.data);
}

}  // namespace lyra::backend::llvm_backend
