#include "lyra/lowering/mir_to_llvm/process.hpp"

#include <format>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/lowering/mir_to_llvm/instruction.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void LowerJump(
    Context& context, const mir::Jump& jump,
    const std::vector<llvm::BasicBlock*>& blocks) {
  context.GetBuilder().CreateBr(blocks[jump.target.value]);
}

void LowerBranch(
    Context& context, const mir::Branch& branch,
    const std::vector<llvm::BasicBlock*>& blocks) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  // Load condition value
  llvm::AllocaInst* cond_alloca = context.GetPlaceStorage(branch.condition);
  if (cond_alloca == nullptr) {
    throw common::InternalError(
        "LowerBranch", "branch condition place not found");
  }
  llvm::Value* cond_val =
      builder.CreateLoad(cond_alloca->getAllocatedType(), cond_alloca, "cond");

  // Truncate to i1 only if needed
  if (!cond_val->getType()->isIntegerTy(1)) {
    cond_val = builder.CreateTrunc(
        cond_val, llvm::Type::getInt1Ty(llvm_ctx), "cond.i1");
  }

  builder.CreateCondBr(
      cond_val, blocks[branch.then_target.value],
      blocks[branch.else_target.value]);
}

void LowerReturn(Context& context, llvm::BasicBlock* exit_block) {
  context.GetBuilder().CreateBr(exit_block);
}

void LowerFinish(Context& context, llvm::BasicBlock* exit_block) {
  // $finish terminates the simulation - jump to exit block
  context.GetBuilder().CreateBr(exit_block);
}

void LowerTerminator(
    Context& context, const mir::Terminator& term,
    const std::vector<llvm::BasicBlock*>& blocks,
    llvm::BasicBlock* exit_block) {
  // Set origin for error reporting
  context.SetCurrentOrigin(term.origin);

  std::visit(
      Overloaded{
          [&](const mir::Jump& t) { LowerJump(context, t, blocks); },
          [&](const mir::Branch& t) { LowerBranch(context, t, blocks); },
          [&](const mir::Return&) { LowerReturn(context, exit_block); },
          [&](const mir::Finish&) { LowerFinish(context, exit_block); },
          [&](const auto&) {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
                "terminator not yet supported");
          },
      },
      term.data);
}

}  // namespace

void LowerProcess(
    Context& context, const mir::Process& process,
    llvm::BasicBlock* exit_block) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  // Get current function from builder's insert block
  llvm::BasicBlock* entry_block = builder.GetInsertBlock();
  llvm::Function* func = entry_block->getParent();

  // Phase 1: Create all LLVM basic blocks upfront (enables forward refs)
  std::vector<llvm::BasicBlock*> llvm_blocks;
  llvm_blocks.reserve(process.blocks.size());
  for (size_t i = 0; i < process.blocks.size(); ++i) {
    auto* bb = llvm::BasicBlock::Create(llvm_ctx, std::format("bb{}", i), func);
    llvm_blocks.push_back(bb);
  }

  // Bridge from current insert point to bb0
  builder.CreateBr(llvm_blocks[0]);

  // Phase 2: Lower each block
  for (size_t i = 0; i < process.blocks.size(); ++i) {
    const auto& block = process.blocks[i];
    builder.SetInsertPoint(llvm_blocks[i]);

    // Lower all instructions
    for (const auto& instruction : block.instructions) {
      LowerInstruction(context, instruction);
    }

    // Lower terminator (exactly one per block)
    LowerTerminator(context, block.terminator, llvm_blocks, exit_block);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
