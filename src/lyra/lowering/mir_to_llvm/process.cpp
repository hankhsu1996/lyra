#include "lyra/lowering/mir_to_llvm/process.hpp"

#include <format>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
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

  // Load condition value from place storage
  llvm::Value* cond_ptr = context.GetPlacePointer(branch.condition);
  llvm::Type* cond_type = context.GetPlaceLlvmType(branch.condition);
  llvm::Value* cond_val = builder.CreateLoad(cond_type, cond_ptr, "cond");

  // 4-state struct: extract known-true bits (a & ~b)
  if (cond_type->isStructTy()) {
    auto* a = builder.CreateExtractValue(cond_val, 0, "cond.a");
    auto* b = builder.CreateExtractValue(cond_val, 1, "cond.b");
    auto* not_b = builder.CreateNot(b, "cond.notb");
    cond_val = builder.CreateAnd(a, not_b, "cond.known");
  }

  // Truncate to i1 only if needed
  if (!cond_val->getType()->isIntegerTy(1)) {
    auto* zero = llvm::ConstantInt::get(cond_val->getType(), 0);
    cond_val = builder.CreateICmpNE(cond_val, zero, "cond.bool");
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

void LowerDelay(
    Context& context, const mir::Delay& delay, llvm::BasicBlock* exit_block) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  // Get pointer to suspend record via state->header.suspend
  llvm::Value* suspend_record = context.GetSuspendRecordPointer();

  // Write SuspendTag::kDelay (1) to tag field at offset 0
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

  auto* tag_ptr = builder.CreateStructGEP(
      context.GetSuspendRecordType(), suspend_record, 0, "tag_ptr");
  builder.CreateStore(llvm::ConstantInt::get(i8_ty, 1), tag_ptr);  // kDelay = 1

  // Write ticks to delay_ticks field at offset 1
  auto* ticks_ptr = builder.CreateStructGEP(
      context.GetSuspendRecordType(), suspend_record, 1, "ticks_ptr");
  builder.CreateStore(llvm::ConstantInt::get(i64_ty, delay.ticks), ticks_ptr);

  // Write resume block ID to resume_block field at offset 2
  auto* resume_ptr = builder.CreateStructGEP(
      context.GetSuspendRecordType(), suspend_record, 2, "resume_ptr");
  builder.CreateStore(
      llvm::ConstantInt::get(i32_ty, delay.resume.value), resume_ptr);

  // Yield to runtime by branching to exit
  builder.CreateBr(exit_block);
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
          [&](const mir::Delay& d) { LowerDelay(context, d, exit_block); },
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

auto GenerateProcessFunction(
    Context& context, const mir::Process& process, const std::string& name)
    -> llvm::Function* {
  auto& llvm_ctx = context.GetLlvmContext();
  auto& module = context.GetModule();

  // Create function type: void(ptr %state, i32 %resume_block)
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* fn_type = llvm::FunctionType::get(
      llvm::Type::getVoidTy(llvm_ctx), {ptr_ty, i32_ty}, false);

  auto* func = llvm::Function::Create(
      fn_type, llvm::Function::ExternalLinkage, name, &module);

  // Name the arguments
  auto* state_arg = func->getArg(0);
  auto* resume_block_arg = func->getArg(1);
  state_arg->setName("state");
  resume_block_arg->setName("resume_block");

  // Create blocks
  auto* entry_block = llvm::BasicBlock::Create(llvm_ctx, "entry", func);
  auto* exit_block = llvm::BasicBlock::Create(llvm_ctx, "exit", func);

  // Create all MIR basic blocks upfront (enables forward refs)
  std::vector<llvm::BasicBlock*> llvm_blocks;
  llvm_blocks.reserve(process.blocks.size());
  for (size_t i = 0; i < process.blocks.size(); ++i) {
    auto* bb = llvm::BasicBlock::Create(llvm_ctx, std::format("bb{}", i), func);
    llvm_blocks.push_back(bb);
  }

  auto& builder = context.GetBuilder();

  // Entry block: set up cached pointers and switch dispatch
  builder.SetInsertPoint(entry_block);

  // Set up context with state pointer
  context.SetStatePointer(state_arg);

  // Load design pointer from state->header.design
  // state->header is field 0, header->design is field 1
  auto* header_ptr = builder.CreateStructGEP(
      context.GetProcessStateType(), state_arg, 0, "header_ptr");
  auto* design_ptr_ptr = builder.CreateStructGEP(
      context.GetHeaderType(), header_ptr, 1, "design_ptr_ptr");
  auto* design_ptr = builder.CreateLoad(ptr_ty, design_ptr_ptr, "design_ptr");
  context.SetDesignPointer(design_ptr);

  // Compute frame pointer: state->frame (field 1)
  auto* frame_ptr = builder.CreateStructGEP(
      context.GetProcessStateType(), state_arg, 1, "frame_ptr");
  context.SetFramePointer(frame_ptr);

  // Create switch with bb0 as default (initial execution)
  auto* sw = builder.CreateSwitch(
      resume_block_arg, llvm_blocks[0],
      static_cast<unsigned>(process.blocks.size()));

  // Add cases for each block (block 0 is already the default)
  for (size_t i = 0; i < process.blocks.size(); ++i) {
    sw->addCase(llvm::ConstantInt::get(i32_ty, i), llvm_blocks[i]);
  }

  // Lower each MIR block
  for (size_t i = 0; i < process.blocks.size(); ++i) {
    const auto& block = process.blocks[i];
    builder.SetInsertPoint(llvm_blocks[i]);

    // Lower all instructions
    for (const auto& instruction : block.instructions) {
      LowerInstruction(context, instruction);
    }

    // Lower terminator
    LowerTerminator(context, block.terminator, llvm_blocks, exit_block);
  }

  // Exit block: just return
  builder.SetInsertPoint(exit_block);
  builder.CreateRetVoid();

  // Clear cached pointers for next function
  context.SetStatePointer(nullptr);
  context.SetDesignPointer(nullptr);
  context.SetFramePointer(nullptr);

  return func;
}

}  // namespace lyra::lowering::mir_to_llvm
