#include "lyra/backend/llvm/codegen_function.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <variant>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/base/internal_error.hpp"
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
  // Every block exists before any is filled, so a branch resolves its successor
  // regardless of the order the blocks are emitted in.
  blocks_.reserve(fn_->blocks.size());
  for (std::uint32_t i = 0; i < fn_->blocks.size(); ++i) {
    blocks_.push_back(
        llvm::BasicBlock::Create(
            module_->Context(), std::format("bb{}", i), value_));
  }

  // A place local is frame storage: its slot is allocated once, in the entry
  // block, so every path that reaches it names the same address.
  builder_.SetInsertPoint(blocks_.front());
  for (std::uint32_t i = 0; i < fn_->values.size(); ++i) {
    const lir::ValueId id{i};
    const lir::Local& local = fn_->values.Get(id);
    if (local.kind == lir::LocalKind::kPlace) {
      values_.emplace(
          i, builder_.CreateAlloca(module_->Types().Map(local.type)));
    }
  }

  for (std::uint32_t i = 0; i < fn_->blocks.size(); ++i) {
    builder_.SetInsertPoint(blocks_[i]);
    for (const lir::Instr& instr : fn_->blocks[i].instrs) {
      values_.emplace(instr.result.value, LowerInstr(instr));
    }
    LowerTerminator(fn_->blocks[i].terminator);
  }
}

void CodeGenFunction::LowerTerminator(const lir::Terminator& terminator) {
  std::visit(
      Overloaded{
          [&](const lir::ReturnTerm& ret) {
            if (value_->getReturnType()->isVoidTy()) {
              builder_.CreateRetVoid();
              return;
            }
            builder_.CreateRet(LowerOperand(*ret.value));
          },
          [&](const lir::BranchTerm& br) {
            builder_.CreateBr(blocks_[br.target.value]);
          },
          [&](const lir::CondBranchTerm& br) {
            builder_.CreateCondBr(
                LowerOperand(br.condition), blocks_[br.if_true.value],
                blocks_[br.if_false.value]);
          },
          [&](const lir::UnreachableTerm&) { builder_.CreateUnreachable(); }},
      terminator.data);
}

auto CodeGenFunction::OperandType(const lir::Operand& operand) const
    -> lir::TypeId {
  const std::optional<lir::TypeId> type = lir::OperandType(*fn_, operand);
  if (!type) {
    throw InternalError("llvm codegen: a code reference has no type");
  }
  return *type;
}

auto CodeGenFunction::DomainOf(lir::TypeId type) const -> ValueDomain {
  return ValueDomainOf(module_->Unit(), type);
}

}  // namespace lyra::backend::llvm_backend
