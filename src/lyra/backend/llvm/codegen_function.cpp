#include "lyra/backend/llvm/codegen_function.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <variant>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type_query.hpp"

namespace lyra::backend::llvm_backend {

CodeGenFunction::CodeGenFunction(
    CodeGenModule& module, const lir::Function& fn, llvm::Function* value)
    : module_(&module), fn_(&fn), value_(value), builder_(module.Context()) {
}

auto CodeGenFunction::IsCoroutine() const -> bool {
  return lir::IsCoroutine(module_->Unit().types, fn_->result_type);
}

void CodeGenFunction::Run() {
  for (std::uint32_t i = 0; i < fn_->params.size(); ++i) {
    values_.emplace(fn_->params[i], value_->getArg(i));
  }

  // Every block exists before any is filled, so a branch resolves its successor
  // regardless of the order the blocks are emitted in.
  blocks_.reserve(fn_->blocks.size());
  for (std::uint32_t i = 0; i < fn_->blocks.size(); ++i) {
    blocks_.push_back(
        llvm::BasicBlock::Create(
            module_->Context(), std::format("bb{}", i), value_));
  }

  // A coroutine body opens with its ramp -- identity, frame, begin -- ahead of
  // the body: a local that must survive a suspension is moved into the
  // coroutine frame only if it is allocated once the frame exists.
  llvm::BasicBlock* entry = blocks_.front();
  if (IsCoroutine()) {
    entry = llvm::BasicBlock::Create(
        module_->Context(), "coro.ramp", value_, blocks_.front());
    builder_.SetInsertPoint(entry);
    OpenCoroutine();
  }

  // A place local is frame storage: its slot is allocated once, in the entry
  // block, so every path that reaches it names the same address.
  builder_.SetInsertPoint(entry);
  for (std::uint32_t i = 0; i < fn_->values.size(); ++i) {
    const lir::ValueId id{i};
    const lir::Local& local = fn_->values.Get(id);
    if (local.kind == lir::LocalKind::kPlace) {
      values_.emplace(
          id, builder_.CreateAlloca(module_->Types().Map(local.type)));
    }
  }
  if (IsCoroutine()) {
    builder_.CreateBr(blocks_.front());
  }

  for (std::uint32_t i = 0; i < fn_->blocks.size(); ++i) {
    builder_.SetInsertPoint(blocks_[i]);
    for (const lir::Instr& instr : fn_->blocks[i].instrs) {
      values_.emplace(instr.result, LowerInstr(instr));
    }
    LowerTerminator(fn_->blocks[i].terminator);
  }
}

void CodeGenFunction::OpenCoroutine() {
  llvm::Module& mod = module_->Module();
  llvm::LLVMContext& ctx = module_->Context();
  llvm::PointerType* ptr_ty = builder_.getPtrTy();
  llvm::Type* size_ty = builder_.getInt64Ty();

  // The coroutine passes split only a body that declares itself one.
  value_->setPresplitCoroutine();

  llvm::Constant* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);
  coro_id_ = builder_.CreateCall(
      llvm::Intrinsic::getDeclaration(&mod, llvm::Intrinsic::coro_id),
      {builder_.getInt32(0), null_ptr, null_ptr, null_ptr});
  llvm::Value* size = builder_.CreateCall(
      llvm::Intrinsic::getDeclaration(
          &mod, llvm::Intrinsic::coro_size, {size_ty}),
      {});
  llvm::Value* memory = builder_.CreateCall(
      mod.getOrInsertFunction("malloc", ptr_ty, size_ty), {size});
  coro_handle_ = builder_.CreateCall(
      llvm::Intrinsic::getDeclaration(&mod, llvm::Intrinsic::coro_begin),
      {coro_id_, memory});

  coro_final_ = llvm::BasicBlock::Create(ctx, "coro.final", value_);
  coro_cleanup_ = llvm::BasicBlock::Create(ctx, "coro.cleanup", value_);
  coro_end_ = llvm::BasicBlock::Create(ctx, "coro.end", value_);

  builder_.SetInsertPoint(coro_cleanup_);
  llvm::Value* frame = builder_.CreateCall(
      llvm::Intrinsic::getDeclaration(&mod, llvm::Intrinsic::coro_free),
      {coro_id_, coro_handle_});
  builder_.CreateCall(
      mod.getOrInsertFunction("free", builder_.getVoidTy(), ptr_ty), {frame});
  builder_.CreateBr(coro_end_);

  builder_.SetInsertPoint(coro_end_);
  builder_.CreateCall(
      llvm::Intrinsic::getDeclaration(&mod, llvm::Intrinsic::coro_end),
      {coro_handle_, builder_.getInt1(false)});
  builder_.CreateRet(coro_handle_);

  // A body that runs to completion suspends one final time, so its owner still
  // reads the handle as done before destroying it.
  builder_.SetInsertPoint(coro_final_);
  EmitCoroutineSuspend(nullptr, true);
}

void CodeGenFunction::EmitCoroutineSuspend(
    llvm::BasicBlock* resume, bool is_final) {
  llvm::Module& mod = module_->Module();
  llvm::Value* save =
      is_final ? llvm::cast<llvm::Value>(
                     llvm::ConstantTokenNone::get(module_->Context()))
               : llvm::cast<llvm::Value>(builder_.CreateCall(
                     llvm::Intrinsic::getDeclaration(
                         &mod, llvm::Intrinsic::coro_save),
                     {coro_handle_}));
  llvm::Value* arm = builder_.CreateCall(
      llvm::Intrinsic::getDeclaration(&mod, llvm::Intrinsic::coro_suspend),
      {save, builder_.getInt1(is_final)});
  // The suspension's three arms: the caller regains control (the default), the
  // body resumes where it left off, or the frame is destroyed.
  llvm::SwitchInst* arms = builder_.CreateSwitch(arm, coro_end_, 2);
  if (resume != nullptr) {
    arms->addCase(builder_.getInt8(0), resume);
  }
  arms->addCase(builder_.getInt8(1), coro_cleanup_);
}

void CodeGenFunction::LowerTerminator(const lir::Terminator& terminator) {
  std::visit(
      Overloaded{
          [&](const lir::ReturnTerm& ret) {
            if (IsCoroutine()) {
              // A coroutine completes through its final suspension; its owner
              // reads completion from the handle, never from a returned value.
              builder_.CreateBr(coro_final_);
              return;
            }
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
          [&](const lir::SuspendTerm& s) {
            // The wakeup source was registered by the calls preceding this
            // terminator; the suspension only hands control back and names
            // where the body resumes.
            EmitCoroutineSuspend(blocks_[s.resume.value], false);
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
