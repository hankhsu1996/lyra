#include "lyra/llvm_backend/process.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <variant>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction.hpp"
#include "lyra/llvm_backend/layout.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto LoadConditionAsI1(Context& context, mir::PlaceId place_id)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();

  auto cond_ptr_or_err = context.GetPlacePointer(place_id);
  if (!cond_ptr_or_err) return std::unexpected(cond_ptr_or_err.error());
  llvm::Value* cond_ptr = *cond_ptr_or_err;

  auto cond_type_or_err = context.GetPlaceLlvmType(place_id);
  if (!cond_type_or_err) return std::unexpected(cond_type_or_err.error());
  llvm::Type* cond_type = *cond_type_or_err;

  llvm::Value* cond_val = builder.CreateLoad(cond_type, cond_ptr, "cond");

  // 4-state struct: extract known-true bits (a & ~b)
  if (cond_type->isStructTy()) {
    auto* a = builder.CreateExtractValue(cond_val, 0, "cond.a");
    auto* b = builder.CreateExtractValue(cond_val, 1, "cond.b");
    auto* not_b = builder.CreateNot(b, "cond.notb");
    cond_val = builder.CreateAnd(a, not_b, "cond.known");
  }

  // Convert to i1
  if (!cond_val->getType()->isIntegerTy(1)) {
    if (cond_val->getType()->isFloatingPointTy()) {
      auto* zero = llvm::ConstantFP::get(cond_val->getType(), 0.0);
      cond_val = builder.CreateFCmpUNE(cond_val, zero, "cond.bool");
    } else {
      auto* zero = llvm::ConstantInt::get(cond_val->getType(), 0);
      cond_val = builder.CreateICmpNE(cond_val, zero, "cond.bool");
    }
  }
  return cond_val;
}

void LowerJump(
    Context& context, const mir::Jump& jump,
    const std::vector<llvm::BasicBlock*>& blocks) {
  context.GetBuilder().CreateBr(blocks[jump.target.value]);
}

auto LowerBranch(
    Context& context, const mir::Branch& branch,
    const std::vector<llvm::BasicBlock*>& blocks) -> Result<void> {
  auto cond_or_err = LoadConditionAsI1(context, branch.condition);
  if (!cond_or_err) return std::unexpected(cond_or_err.error());
  context.GetBuilder().CreateCondBr(
      *cond_or_err, blocks[branch.then_target.value],
      blocks[branch.else_target.value]);
  return {};
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

auto LowerWait(
    Context& context, const mir::Wait& wait, llvm::BasicBlock* exit_block)
    -> Result<void> {
  static_assert(sizeof(runtime::WaitTriggerRecord) == 8);

  if (wait.triggers.size() > runtime::kMaxInlineTriggers) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        std::format(
            "too many wait triggers ({}, max {})", wait.triggers.size(),
            runtime::kMaxInlineTriggers),
        UnsupportedCategory::kFeature));
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
  return {};
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

auto LowerQualifiedDispatch(
    Context& context, const mir::QualifiedDispatch& dispatch,
    const std::vector<llvm::BasicBlock*>& blocks) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* func = builder.GetInsertBlock()->getParent();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  auto* else_target = blocks[dispatch.targets.back().value];

  // Load all conditions as i1
  std::vector<llvm::Value*> conds;
  conds.reserve(dispatch.conditions.size());
  for (auto place_id : dispatch.conditions) {
    auto cond_or_err = LoadConditionAsI1(context, place_id);
    if (!cond_or_err) return std::unexpected(cond_or_err.error());
    conds.push_back(*cond_or_err);
  }

  // No conditions: jump directly to else target
  if (conds.empty()) {
    builder.CreateBr(else_target);
    return {};
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
  return {};
}

auto LowerTerminator(
    Context& context, const mir::Terminator& term,
    const std::vector<llvm::BasicBlock*>& blocks, llvm::BasicBlock* exit_block)
    -> Result<void> {
  // Set origin for error reporting.
  // OriginScope preserves outer (function/process) origin if term.origin is
  // Invalid.
  OriginScope origin_scope(context, term.origin);

  return std::visit(
      common::Overloaded{
          [&](const mir::Jump& t) -> Result<void> {
            LowerJump(context, t, blocks);
            return {};
          },
          [&](const mir::Branch& t) -> Result<void> {
            return LowerBranch(context, t, blocks);
          },
          [&](const mir::Return&) -> Result<void> {
            LowerReturn(context, exit_block);
            return {};
          },
          [&](const mir::Finish&) -> Result<void> {
            LowerFinish(context, exit_block);
            return {};
          },
          [&](const mir::Delay& d) -> Result<void> {
            LowerDelay(context, d, exit_block);
            return {};
          },
          [&](const mir::Wait& w) -> Result<void> {
            return LowerWait(context, w, exit_block);
          },
          [&](const mir::Repeat&) -> Result<void> {
            LowerRepeat(context, exit_block);
            return {};
          },
          [&](const mir::QualifiedDispatch& d) -> Result<void> {
            return LowerQualifiedDispatch(context, d, blocks);
          },
          [&](const auto&) -> Result<void> {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(), "terminator not yet supported",
                    UnsupportedCategory::kFeature));
          },
      },
      term.data);
}

}  // namespace

auto GenerateProcessFunction(
    Context& context, const mir::Process& process, const std::string& name)
    -> Result<llvm::Function*> {
  // Process-level origin scope for errors during code generation.
  // If process.origin is Invalid, this is a no-op.
  OriginScope proc_scope(context, process.origin);

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
      auto result = LowerInstruction(context, instruction);
      if (!result) return std::unexpected(result.error());
    }

    // Lower terminator
    auto term_result =
        LowerTerminator(context, block.terminator, llvm_blocks, exit_block);
    if (!term_result) return std::unexpected(term_result.error());
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

auto DeclareUserFunction(
    Context& context, mir::FunctionId func_id, const std::string& name)
    -> Result<llvm::Function*> {
  auto& module = context.GetModule();
  const auto& arena = context.GetMirArena();
  const auto& func = arena[func_id];

  // Build LLVM function type from MIR signature
  auto fn_type_or_err = context.BuildUserFunctionType(func.signature);
  if (!fn_type_or_err) return std::unexpected(fn_type_or_err.error());
  llvm::FunctionType* fn_type = *fn_type_or_err;

  // Create function declaration
  auto* llvm_func = llvm::Function::Create(
      fn_type, llvm::Function::InternalLinkage, name, &module);

  // Register in context for call resolution
  context.RegisterUserFunction(func_id, llvm_func);

  return llvm_func;
}

namespace {

// Collect all unique place roots (Local or Temp) from a function.
// Storage is per-root, NOT per-PlaceId. Multiple PlaceIds with the same root
// (but different projections) share the same storage.
struct PlaceCollector {
  // Collect PlaceRoot directly, keyed by root identity.
  // No arena scanning needed - we just extract the root from any place.
  std::unordered_map<PlaceRootKey, mir::PlaceRoot, PlaceRootKeyHash> roots;

  void CollectFromPlace(mir::PlaceId place_id, const mir::Arena& arena) {
    const auto& place = arena[place_id];

    // Only collect Local and Temp roots (Design roots go in design state)
    if (place.root.kind == mir::PlaceRoot::Kind::kLocal ||
        place.root.kind == mir::PlaceRoot::Kind::kTemp) {
      PlaceRootKey key{.kind = place.root.kind, .id = place.root.id};
      // try_emplace: only insert if key not present
      roots.try_emplace(key, place.root);
    }
  }

  void CollectFromOperand(const mir::Operand& op, const mir::Arena& arena) {
    if (op.kind == mir::Operand::Kind::kUse) {
      CollectFromPlace(std::get<mir::PlaceId>(op.payload), arena);
    }
  }

  void CollectFromFunction(const mir::Function& func, const mir::Arena& arena) {
    for (const auto& block : func.blocks) {
      for (const auto& inst : block.instructions) {
        std::visit(
            [&](const auto& data) {
              using T = std::decay_t<decltype(data)>;
              if constexpr (std::is_same_v<T, mir::Assign>) {
                CollectFromPlace(data.target, arena);
                CollectFromOperand(data.source, arena);
              } else if constexpr (std::is_same_v<T, mir::Compute>) {
                CollectFromPlace(data.target, arena);
                for (const auto& op : data.value.operands) {
                  CollectFromOperand(op, arena);
                }
              } else if constexpr (std::is_same_v<T, mir::GuardedAssign>) {
                CollectFromPlace(data.target, arena);
                CollectFromOperand(data.source, arena);
                CollectFromOperand(data.validity, arena);
              } else if constexpr (std::is_same_v<T, mir::Effect>) {
                // Effects may have operands too
                std::visit(
                    [&](const auto& eff) {
                      if constexpr (requires { eff.operands; }) {
                        for (const auto& eop : eff.operands) {
                          CollectFromOperand(eop, arena);
                        }
                      }
                    },
                    data.op);
              } else if constexpr (std::is_same_v<T, mir::NonBlockingAssign>) {
                CollectFromPlace(data.target, arena);
                CollectFromOperand(data.source, arena);
              }
            },
            inst.data);
      }
      // Collect from terminator operands
      std::visit(
          [&](const auto& term) {
            using T = std::decay_t<decltype(term)>;
            if constexpr (std::is_same_v<T, mir::Branch>) {
              CollectFromPlace(term.condition, arena);
            } else if constexpr (std::is_same_v<T, mir::Return>) {
              if (term.value.has_value()) {
                CollectFromOperand(*term.value, arena);
              }
            } else if constexpr (std::is_same_v<T, mir::QualifiedDispatch>) {
              for (auto cond : term.conditions) {
                CollectFromPlace(cond, arena);
              }
            }
          },
          block.terminator.data);
    }
  }
};

}  // namespace

auto DefineUserFunction(
    Context& context, mir::FunctionId func_id, llvm::Function* llvm_func)
    -> Result<void> {
  auto& llvm_ctx = context.GetLlvmContext();
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& func = arena[func_id];

  // Function-level origin scope for prologue errors.
  // If func.origin is Invalid, this is a no-op.
  OriginScope func_scope(context, func.origin);

  // Create blocks
  auto* entry_block = llvm::BasicBlock::Create(llvm_ctx, "entry", llvm_func);
  auto* exit_block = llvm::BasicBlock::Create(llvm_ctx, "exit", llvm_func);

  // Create all MIR basic blocks upfront
  std::vector<llvm::BasicBlock*> llvm_blocks;
  llvm_blocks.reserve(func.blocks.size());
  for (size_t i = 0; i < func.blocks.size(); ++i) {
    auto* bb =
        llvm::BasicBlock::Create(llvm_ctx, std::format("bb{}", i), llvm_func);
    llvm_blocks.push_back(bb);
  }

  // Entry block setup
  builder.SetInsertPoint(entry_block);
  context.BeginFunction(*llvm_func);

  // First argument is DesignState*, second is Engine*
  auto* design_arg = llvm_func->getArg(0);
  design_arg->setName("design");
  context.SetDesignPointer(design_arg);

  auto* engine_arg = llvm_func->getArg(1);
  engine_arg->setName("engine");
  context.SetEnginePointer(engine_arg);

  // Collect all local/temp places referenced in the function
  PlaceCollector collector;
  collector.CollectFromFunction(func, arena);

  // Variable to hold return value (for non-void functions)
  // Must be allocated before any branches
  llvm::Value* return_value_ptr = nullptr;
  const Type& ret_type = types[func.signature.return_type];
  if (ret_type.Kind() != TypeKind::kVoid) {
    llvm::Type* ret_llvm_type = llvm_func->getReturnType();
    return_value_ptr = builder.CreateAlloca(ret_llvm_type, nullptr, "retval");
  }

  // Pre-allocate storage for parameter locals and store arg values.
  // Use explicit param_local_slots mapping - do NOT assume param i = local i.
  // Arguments start at index 2 (after design and engine pointers).
  for (size_t i = 0; i < func.signature.params.size(); ++i) {
    // Set parameter-specific origin for prologue errors
    common::OriginId param_origin = (i < func.param_origins.size())
                                        ? func.param_origins[i]
                                        : common::OriginId::Invalid();
    OriginScope param_scope(context, param_origin);

    uint32_t local_slot = func.param_local_slots[i];
    PlaceRootKey key{
        .kind = mir::PlaceRoot::Kind::kLocal,
        .id = static_cast<int>(local_slot)};
    auto it = collector.roots.find(key);
    if (it != collector.roots.end()) {
      // Create alloca for this parameter local
      auto alloca_or_err = context.GetOrCreatePlaceStorage(it->second);
      if (!alloca_or_err) return std::unexpected(alloca_or_err.error());
      llvm::AllocaInst* alloca = *alloca_or_err;

      // Store argument value into the alloca (offset by 2 for design+engine)
      llvm::Value* arg_val = llvm_func->getArg(static_cast<unsigned>(i + 2));
      arg_val->setName(std::format("arg{}", i));
      builder.CreateStore(arg_val, alloca);
    }
  }

  // Jump to first MIR block
  builder.CreateBr(llvm_blocks[func.entry.value]);

  // Lower each MIR block
  for (size_t i = 0; i < func.blocks.size(); ++i) {
    const auto& block = func.blocks[i];
    builder.SetInsertPoint(llvm_blocks[i]);

    // Lower all instructions
    for (const auto& instruction : block.instructions) {
      auto result = LowerInstruction(context, instruction);
      if (!result) return std::unexpected(result.error());
    }

    // Lower terminator.
    // OriginScope preserves func.origin if terminator.origin is Invalid.
    OriginScope term_scope(context, block.terminator.origin);
    auto term_result = std::visit(
        common::Overloaded{
            [&](const mir::Jump& t) -> Result<void> {
              LowerJump(context, t, llvm_blocks);
              return {};
            },
            [&](const mir::Branch& t) -> Result<void> {
              return LowerBranch(context, t, llvm_blocks);
            },
            [&](const mir::Return& t) -> Result<void> {
              // Store return value if present and non-void
              if (t.value.has_value() && return_value_ptr != nullptr) {
                auto val_result = LowerOperandRaw(context, *t.value);
                if (!val_result) return std::unexpected(val_result.error());
                builder.CreateStore(*val_result, return_value_ptr);
              }
              builder.CreateBr(exit_block);
              return {};
            },
            [&](const mir::Finish&) -> Result<void> {
              // $finish in a function - just return (shouldn't happen normally)
              builder.CreateBr(exit_block);
              return {};
            },
            [&](const mir::QualifiedDispatch& d) -> Result<void> {
              return LowerQualifiedDispatch(context, d, llvm_blocks);
            },
            [&](const auto&) -> Result<void> {
              return std::unexpected(
                  context.GetDiagnosticContext().MakeUnsupported(
                      context.GetCurrentOrigin(),
                      "terminator not yet supported in user function",
                      UnsupportedCategory::kFeature));
            },
        },
        block.terminator.data);
    if (!term_result) return std::unexpected(term_result.error());
  }

  // Exit block: return the value
  builder.SetInsertPoint(exit_block);
  if (ret_type.Kind() == TypeKind::kVoid) {
    builder.CreateRetVoid();
  } else {
    llvm::Value* ret_val =
        builder.CreateLoad(llvm_func->getReturnType(), return_value_ptr, "ret");
    builder.CreateRet(ret_val);
  }

  // Clean up function scope
  context.EndFunction();
  context.SetDesignPointer(nullptr);

  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
