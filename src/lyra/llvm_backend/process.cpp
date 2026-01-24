#include "lyra/llvm_backend/process.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <variant>
#include <vector>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto LoadConditionAsI1(Context& context, mir::PlaceId place_id)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  llvm::Value* cond_ptr = context.GetPlacePointer(place_id);
  llvm::Type* cond_type = context.GetPlaceLlvmType(place_id);
  llvm::Value* cond_val = builder.CreateLoad(cond_type, cond_ptr, "cond");

  // 4-state struct: extract known-true bits (a & ~b)
  if (cond_type->isStructTy()) {
    auto* a = builder.CreateExtractValue(cond_val, 0, "cond.a");
    auto* b = builder.CreateExtractValue(cond_val, 1, "cond.b");
    auto* not_b = builder.CreateNot(b, "cond.notb");
    cond_val = builder.CreateAnd(a, not_b, "cond.known");
  }

  // Truncate to i1
  if (!cond_val->getType()->isIntegerTy(1)) {
    auto* zero = llvm::ConstantInt::get(cond_val->getType(), 0);
    cond_val = builder.CreateICmpNE(cond_val, zero, "cond.bool");
  }
  return cond_val;
}

void LowerJump(
    Context& context, const mir::Jump& jump,
    const std::vector<llvm::BasicBlock*>& blocks) {
  context.GetBuilder().CreateBr(blocks[jump.target.value]);
}

void LowerBranch(
    Context& context, const mir::Branch& branch,
    const std::vector<llvm::BasicBlock*>& blocks) {
  auto* cond_val = LoadConditionAsI1(context, branch.condition);
  context.GetBuilder().CreateCondBr(
      cond_val, blocks[branch.then_target.value],
      blocks[branch.else_target.value]);
}

void LowerReturn(Context& context, llvm::BasicBlock* exit_block) {
  context.GetBuilder().CreateBr(exit_block);
}

void LowerFinish(Context& context, llvm::BasicBlock* exit_block) {
  // $finish stops the simulation engine, then returns to the exit block which
  // runs LyraSnapshotVars + LyraReportTime for test harness output.
  context.GetBuilder().CreateCall(
      context.GetLyraFinishSimulation(), {context.GetEnginePointer()});
  context.GetBuilder().CreateBr(exit_block);
}

void LowerDelay(
    Context& context, const mir::Delay& delay, llvm::BasicBlock* exit_block) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  builder.CreateCall(
      context.GetLyraSuspendDelay(),
      {context.GetStatePointer(), llvm::ConstantInt::get(i64_ty, delay.ticks),
       llvm::ConstantInt::get(i32_ty, delay.resume.value)});

  builder.CreateBr(exit_block);
}

void LowerWait(
    Context& context, const mir::Wait& wait, llvm::BasicBlock* exit_block) {
  static_assert(sizeof(runtime::WaitTriggerRecord) == 8);

  if (wait.triggers.size() > runtime::kMaxInlineTriggers) {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kFeature,
        context.GetCurrentOrigin(),
        std::format(
            "too many wait triggers ({}, max {})", wait.triggers.size(),
            runtime::kMaxInlineTriggers));
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  auto num_triggers = static_cast<uint32_t>(wait.triggers.size());

  // WaitTriggerRecord layout: {i32 signal_id, i8 edge, [3 x i8] padding}
  auto* trigger_type = llvm::StructType::get(
      llvm_ctx, {i32_ty, i8_ty, llvm::ArrayType::get(i8_ty, 3)});
  auto* array_type = llvm::ArrayType::get(trigger_type, num_triggers);

  // Create local alloca for trigger array
  auto* triggers_alloca = builder.CreateAlloca(array_type, nullptr, "triggers");

  // Fill each trigger element
  for (uint32_t i = 0; i < num_triggers; ++i) {
    auto* elem_ptr =
        builder.CreateConstGEP2_32(array_type, triggers_alloca, 0, i);

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

  // Call LyraSuspendWait(state, resume_block, triggers, num_triggers)
  builder.CreateCall(
      context.GetLyraSuspendWait(),
      {context.GetStatePointer(),
       llvm::ConstantInt::get(i32_ty, wait.resume.value), triggers_alloca,
       llvm::ConstantInt::get(i32_ty, num_triggers)});

  builder.CreateBr(exit_block);
}

void LowerRepeat(Context& context, llvm::BasicBlock* exit_block) {
  auto& builder = context.GetBuilder();

  builder.CreateCall(
      context.GetLyraSuspendRepeat(), {context.GetStatePointer()});

  builder.CreateBr(exit_block);
}

auto ComputeOverlapMessage(const mir::QualifiedDispatch& d) -> std::string {
  const char* what = (d.statement_kind == mir::DispatchStatementKind::kIf)
                         ? "multiple conditions true"
                         : "multiple case items match";
  const char* qualifier =
      (d.qualifier == mir::DispatchQualifier::kUnique) ? "unique" : "unique0";
  const char* stmt =
      (d.statement_kind == mir::DispatchStatementKind::kIf) ? "if" : "case";
  return std::format("warning: {} in {} {}\n", what, qualifier, stmt);
}

auto ComputeNomatchMessage(const mir::QualifiedDispatch& d) -> std::string {
  const char* what = (d.statement_kind == mir::DispatchStatementKind::kIf)
                         ? "no condition matched"
                         : "no matching case item";
  const char* stmt =
      (d.statement_kind == mir::DispatchStatementKind::kIf) ? "if" : "case";
  return std::format("warning: {} in unique {}\n", what, stmt);
}

void LowerQualifiedDispatch(
    Context& context, const mir::QualifiedDispatch& dispatch,
    const std::vector<llvm::BasicBlock*>& blocks) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* func = builder.GetInsertBlock()->getParent();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  auto* else_target = blocks[dispatch.targets.back().value];

  // Load all conditions as i1
  std::vector<llvm::Value*> conds;
  conds.reserve(dispatch.conditions.size());
  for (auto place_id : dispatch.conditions) {
    conds.push_back(LoadConditionAsI1(context, place_id));
  }

  // No conditions: jump directly to else target
  if (conds.empty()) {
    builder.CreateBr(else_target);
    return;
  }

  // Count true conditions (i32 accumulator)
  llvm::Value* cnt = llvm::ConstantInt::get(i32_ty, 0);
  for (auto* cond : conds) {
    auto* ext = builder.CreateZExt(cond, i32_ty);
    cnt = builder.CreateAdd(cnt, ext);
  }

  // Overlap warning: cnt > 1
  auto overlap_msg = ComputeOverlapMessage(dispatch);
  auto* overlap_str =
      builder.CreateGlobalStringPtr(overlap_msg, "qd.overlap.str");
  auto* after_overlap =
      llvm::BasicBlock::Create(llvm_ctx, "qd.after_overlap", func);
  auto* overlap_bb =
      llvm::BasicBlock::Create(llvm_ctx, "qd.warn_overlap", func);
  auto* overlap_cond =
      builder.CreateICmpUGT(cnt, llvm::ConstantInt::get(i32_ty, 1));
  builder.CreateCondBr(overlap_cond, overlap_bb, after_overlap);

  builder.SetInsertPoint(overlap_bb);
  builder.CreateCall(context.GetLyraPrintLiteral(), {overlap_str});
  builder.CreateBr(after_overlap);

  builder.SetInsertPoint(after_overlap);

  // No-match warning: only for kUnique && !has_else
  if (dispatch.qualifier == mir::DispatchQualifier::kUnique &&
      !dispatch.has_else) {
    auto nomatch_msg = ComputeNomatchMessage(dispatch);
    auto* nomatch_str =
        builder.CreateGlobalStringPtr(nomatch_msg, "qd.nomatch.str");
    auto* dispatch_bb = llvm::BasicBlock::Create(llvm_ctx, "qd.dispatch", func);
    auto* nomatch_bb =
        llvm::BasicBlock::Create(llvm_ctx, "qd.warn_nomatch", func);
    auto* nomatch_cond =
        builder.CreateICmpEQ(cnt, llvm::ConstantInt::get(i32_ty, 0));
    builder.CreateCondBr(nomatch_cond, nomatch_bb, dispatch_bb);

    builder.SetInsertPoint(nomatch_bb);
    builder.CreateCall(context.GetLyraPrintLiteral(), {nomatch_str});
    builder.CreateBr(dispatch_bb);

    builder.SetInsertPoint(dispatch_bb);
  }

  // Dispatch cascade: first true condition wins
  for (size_t i = 0; i < conds.size(); ++i) {
    auto* target = blocks[dispatch.targets[i].value];
    auto* next_check =
        (i + 1 < conds.size())
            ? llvm::BasicBlock::Create(
                  llvm_ctx, std::format("qd.check{}", i + 1), func)
            : else_target;
    builder.CreateCondBr(conds[i], target, next_check);
    if (i + 1 < conds.size()) {
      builder.SetInsertPoint(next_check);
    }
  }
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
          [&](const mir::QualifiedDispatch& d) {
            LowerQualifiedDispatch(context, d, blocks);
          },
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
