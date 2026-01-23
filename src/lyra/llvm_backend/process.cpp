#include "lyra/llvm_backend/process.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <variant>
#include <vector>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction.hpp"
#include "lyra/mir/routine.hpp"
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
  // $finish must return control to main()'s exit block — never call
  // exit()/abort(). The exit block runs LyraSnapshotVars + LyraReportTime for
  // test harness output.
  context.GetBuilder().CreateBr(exit_block);
}

void LowerDelay(
    Context& context, const mir::Delay& delay, llvm::BasicBlock* exit_block) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  llvm::Value* suspend_record = context.GetSuspendRecordPointer();

  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

  auto* tag_ptr = builder.CreateStructGEP(
      context.GetSuspendRecordType(), suspend_record, 0, "tag_ptr");
  builder.CreateStore(llvm::ConstantInt::get(i8_ty, 1), tag_ptr);

  auto* ticks_ptr = builder.CreateStructGEP(
      context.GetSuspendRecordType(), suspend_record, 1, "ticks_ptr");
  builder.CreateStore(llvm::ConstantInt::get(i64_ty, delay.ticks), ticks_ptr);

  auto* resume_ptr = builder.CreateStructGEP(
      context.GetSuspendRecordType(), suspend_record, 2, "resume_ptr");
  builder.CreateStore(
      llvm::ConstantInt::get(i32_ty, delay.resume.value), resume_ptr);

  builder.CreateBr(exit_block);
}

void LowerWait(
    Context& context, const mir::Wait& wait, llvm::BasicBlock* exit_block) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  llvm::Value* suspend_record = context.GetSuspendRecordPointer();
  auto* suspend_type = context.GetSuspendRecordType();

  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  // Write kWait (2) to tag
  auto* tag_ptr = builder.CreateStructGEP(suspend_type, suspend_record, 0);
  builder.CreateStore(llvm::ConstantInt::get(i8_ty, 2), tag_ptr);

  // Write resume block
  auto* resume_ptr = builder.CreateStructGEP(suspend_type, suspend_record, 2);
  builder.CreateStore(
      llvm::ConstantInt::get(i32_ty, wait.resume.value), resume_ptr);

  // Write num_triggers
  auto num_triggers = static_cast<uint32_t>(wait.triggers.size());
  auto* num_ptr = builder.CreateStructGEP(suspend_type, suspend_record, 3);
  builder.CreateStore(llvm::ConstantInt::get(i32_ty, num_triggers), num_ptr);

  // Write each trigger
  auto* triggers_ptr = builder.CreateStructGEP(suspend_type, suspend_record, 4);
  auto* array_type =
      llvm::cast<llvm::ArrayType>(suspend_type->getElementType(4));
  auto* trigger_type =
      llvm::cast<llvm::StructType>(array_type->getElementType());

  for (uint32_t i = 0; i < num_triggers; ++i) {
    auto* elem_ptr = builder.CreateConstGEP2_32(array_type, triggers_ptr, 0, i);

    // Write signal_id (field 0)
    auto* signal_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 0);
    builder.CreateStore(
        llvm::ConstantInt::get(i32_ty, wait.triggers[i].signal.value),
        signal_ptr);

    // Write edge (field 1)
    auto* edge_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 1);
    builder.CreateStore(
        llvm::ConstantInt::get(
            i8_ty, static_cast<uint8_t>(wait.triggers[i].edge)),
        edge_ptr);
  }

  builder.CreateBr(exit_block);
}

void LowerRepeat(Context& context, llvm::BasicBlock* exit_block) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  llvm::Value* suspend_record = context.GetSuspendRecordPointer();
  auto* suspend_type = context.GetSuspendRecordType();

  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  // Write kRepeat (3) to tag
  auto* tag_ptr = builder.CreateStructGEP(suspend_type, suspend_record, 0);
  builder.CreateStore(llvm::ConstantInt::get(i8_ty, 3), tag_ptr);

  // Write 0 to resume_block (restart at entry)
  auto* resume_ptr = builder.CreateStructGEP(suspend_type, suspend_record, 2);
  builder.CreateStore(llvm::ConstantInt::get(i32_ty, 0), resume_ptr);

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
          [&](const mir::Wait& w) { LowerWait(context, w, exit_block); },
          [&](const mir::Repeat&) { LowerRepeat(context, exit_block); },
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

  // Load engine pointer from state->header.engine (field 2)
  auto* engine_ptr_ptr = builder.CreateStructGEP(
      context.GetHeaderType(), header_ptr, 2, "engine_ptr_ptr");
  auto* engine_ptr = builder.CreateLoad(ptr_ty, engine_ptr_ptr, "engine_ptr");
  context.SetEnginePointer(engine_ptr);

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
  context.SetEnginePointer(nullptr);

  return func;
}

}  // namespace lyra::lowering::mir_to_llvm
