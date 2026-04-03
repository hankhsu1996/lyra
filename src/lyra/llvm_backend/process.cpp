#include "lyra/llvm_backend/process.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/index_plan.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/activation_local.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/compute/rvalue.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/context_scope.hpp"
#include "lyra/llvm_backend/contract_executor.hpp"
#include "lyra/llvm_backend/instruction/display.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/observer_abi.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/llvm_backend/statement.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/passes/canonical_loop_analysis.hpp"
#include "lyra/mir/passes/non_yielding_loop_analysis.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"
#include "lyra/runtime/simulation.hpp"
#include "lyra/runtime/suspend_record.hpp"
#include "lyra/runtime/trap.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Validate that a process meets the init execution contract:
// no suspension-capable terminators (Delay, Wait, Repeat).
// Init processes run to completion before the simulation engine exists.
// If MIR gains new suspension-capable terminators, add them here.
void ValidateInitProcessContract(const mir::Process& process) {
  for (const auto& block : process.blocks) {
    const char* suspension_name = nullptr;
    std::visit(
        [&](const auto& term) {
          using T = std::decay_t<decltype(term)>;
          if constexpr (std::is_same_v<T, mir::Delay>) {
            suspension_name = "Delay";
          } else if constexpr (std::is_same_v<T, mir::Wait>) {
            suspension_name = "Wait";
          } else if constexpr (std::is_same_v<T, mir::Repeat>) {
            suspension_name = "Repeat";
          }
        },
        block.terminator.data);
    if (suspension_name != nullptr) {
      throw common::InternalError(
          "ValidateInitProcessContract",
          std::format(
              "init process contains {} terminator; init processes must "
              "run to completion without suspension",
              suspension_name));
    }
  }
}

// Build a sorted vector of bounded latch block indices for cheap lookup.
// Routines typically have very few loops, so a tiny sorted vector with
// binary search is simpler and cheaper than a hash set.
auto BuildBoundedLatchSet(
    const std::vector<mir::passes::BoundedBackEdge>& bounded)
    -> std::vector<uint32_t> {
  std::vector<uint32_t> result;
  result.reserve(bounded.size());
  for (const auto& edge : bounded) {
    result.push_back(edge.latch_block);
  }
  std::ranges::sort(result);
  auto [first, last] = std::ranges::unique(result);
  result.erase(first, last);
  return result;
}

// Build a sorted vector of block indices inside deferred-notification regions.
// Invariant: exit_block must never appear in region_blocks. If it did,
// codegen would both suppress notifications and emit deferred marks for the
// same block, which is contradictory. The analysis guarantees this by
// construction (region_blocks = header + body + latch, exit is the
// successor outside the loop), but we verify it here as a backstop.
auto BuildDeferredPolicyBlocks(
    const std::vector<mir::passes::DeferredNotificationLoop>& loops)
    -> std::vector<uint32_t> {
  std::vector<uint32_t> result;
  for (const auto& loop : loops) {
    for (uint32_t bi : loop.region_blocks) {
      if (bi == loop.exit_block) {
        throw common::InternalError(
            "BuildDeferredPolicyBlocks",
            "exit_block must not appear in region_blocks");
      }
      result.push_back(bi);
    }
  }
  std::ranges::sort(result);
  auto [first, last] = std::ranges::unique(result);
  result.erase(first, last);
  return result;
}

// Emit deferred dirty-mark calls for a set of canonical notified roots.
//
// This models the loop-exit edge effect: unconditional full-slot marks
// for every root that may have been written within the qualifying loop.
// Semantically, the marks belong to the edge leaving the loop, not to
// the successor block's own logic. They must be emitted before any
// non-PHI work in the successor block.
void EmitLoopExitDeferredMarks(
    Context& context, const std::vector<mir::SignalRef>& roots) {
  auto& builder = context.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
  for (const auto& root : roots) {
    bool static_hit = context.RequiresStaticDirtyPropagation(root);
    auto signal_id = context.EmitMutationTargetSignalCoord(root);

    auto emit_mark = [&]() {
      if (signal_id.IsLocal()) {
        builder.CreateCall(
            context.GetLyraMarkDirtyLocal(),
            {context.GetEnginePointer(),
             signal_id.GetInstancePointer(context.GetInstancePointer()),
             signal_id.Emit(builder), llvm::ConstantInt::get(i32_ty, 0),
             llvm::ConstantInt::get(i32_ty, 0)});
      } else {
        builder.CreateCall(
            context.GetLyraMarkDirtyGlobal(),
            {context.GetEnginePointer(), signal_id.Emit(builder),
             llvm::ConstantInt::get(i32_ty, 0),
             llvm::ConstantInt::get(i32_ty, 0)});
      }
    };

    if (static_hit) {
      emit_mark();
    } else {
      context.EmitTraceBranch(
          root, "deferred.mark", "deferred.skip", emit_mark, []() {});
    }
  }
}

// Canonical memory layout type for ProcessOutcome: { i32, i32, i32, i32 }.
// Used only as a memory shape for stores through the %out pointer.
// No struct return across the JIT boundary.
auto GetProcessOutcomeType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  return llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty, i32_ty});
}

// Store ProcessOutcome{tag, reason, a, b} into %out as a single aggregate
// store.
void EmitStoreOutcome(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& ctx, llvm::Value* out_ptr,
    uint32_t tag, uint32_t reason, llvm::Value* a, llvm::Value* b) {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* outcome_ty = GetProcessOutcomeType(ctx);

  llvm::Value* outcome = llvm::UndefValue::get(outcome_ty);
  outcome = builder.CreateInsertValue(
      outcome, llvm::ConstantInt::get(i32_ty, tag), {0});
  outcome = builder.CreateInsertValue(
      outcome, llvm::ConstantInt::get(i32_ty, reason), {1});
  outcome = builder.CreateInsertValue(outcome, a, {2});
  outcome = builder.CreateInsertValue(outcome, b, {3});
  builder.CreateStore(outcome, out_ptr);
}

// Store kOk outcome {kOk, 0, 0, 0} into %out.
void EmitStoreOkOutcome(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& ctx, llvm::Value* out_ptr) {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  EmitStoreOutcome(
      builder, ctx, out_ptr,
      static_cast<uint32_t>(runtime::ProcessExitCode::kOk), 0, zero, zero);
}

// =============================================================================
// Block Params and PHI Wiring
// =============================================================================
//
// MIR block params model SSA phi-like values at CFG join points (e.g., ternary
// merge blocks). LLVM lowering creates PHI nodes for these params.
//
// Why we pre-create PHIs and wire later:
// 1. PHIs must be at the start of their block (LLVM requirement)
// 2. Incoming values come from predecessor blocks that are lowered later
// 3. We can't add PHI incoming values until predecessors exist
//
// Lifecycle:
// 1. SetupBlockParamPhis: Create PHIs at block entry, bind temp_ids
// 2. Lower blocks: Terminators record pending edges via LowerJump/LowerBranch
// 3. WirePhiIncomingEdges: Add incoming values from pending edges
// 4. ValidatePhiWiring: Verify every PHI has correct predecessor count
//
// Edge args are lowered BEFORE the terminator is created (while still in the
// predecessor block) to ensure loads happen in the correct block context.
// =============================================================================

// Tracks PHI nodes for block params and pending incoming edges.
// Used to defer PHI wiring until all blocks are lowered.
struct PhiWiringState {
  // For 4-state params, split into two scalar PHIs (known + unknown).
  // For 2-state params, unknown is nullptr.
  struct PhiEntry {
    llvm::PHINode* known = nullptr;
    // nullptr for 2-state params
    llvm::PHINode* unknown = nullptr;
  };

  // Map from (block_index, param_index) to the PHI entry
  std::unordered_map<uint64_t, PhiEntry> phis;

  // Pending edges: (predecessor_block, successor_block_index, arg_values)
  struct PendingEdge {
    llvm::BasicBlock* pred;
    size_t succ_idx;
    std::vector<llvm::Value*> args;
  };
  std::vector<PendingEdge> pending_edges;

  static auto MakeKey(size_t block_idx, size_t param_idx) -> uint64_t {
    return (static_cast<uint64_t>(block_idx) << 32) |
           static_cast<uint64_t>(param_idx);
  }

  // Validate that all PHIs have been properly wired.
  void ValidatePhiWiring(
      const std::vector<llvm::BasicBlock*>& llvm_blocks,
      std::string_view func_name) const {
    for (const auto& [key, entry] : phis) {
      size_t block_idx = key >> 32;
      size_t param_idx = key & 0xFFFFFFFF;
      llvm::BasicBlock* bb = llvm_blocks[block_idx];

      size_t pred_count = 0;
      for (auto it = llvm::pred_begin(bb); it != llvm::pred_end(bb); ++it) {
        ++pred_count;
      }

      size_t known_count = entry.known->getNumIncomingValues();
      if (known_count != pred_count) {
        std::string preds;
        for (auto it = llvm::pred_begin(bb); it != llvm::pred_end(bb); ++it) {
          if (!preds.empty()) preds += ", ";
          preds += (*it)->getName().str();
        }

        std::string incoming_preds;
        for (unsigned i = 0; i < entry.known->getNumIncomingValues(); ++i) {
          if (!incoming_preds.empty()) incoming_preds += ", ";
          incoming_preds += entry.known->getIncomingBlock(i)->getName().str();
        }

        throw common::InternalError(
            "PHI wiring",
            std::format(
                "function '{}' block {} param {}: known PHI has {} incoming "
                "values but block has {} predecessors. LLVM preds: [{}], PHI "
                "preds: [{}]",
                func_name, block_idx, param_idx, known_count, pred_count, preds,
                incoming_preds));
      }

      if (entry.unknown != nullptr) {
        size_t unk_count = entry.unknown->getNumIncomingValues();
        if (unk_count != pred_count) {
          throw common::InternalError(
              "PHI wiring",
              std::format(
                  "function '{}' block {} param {}: unknown PHI has {} "
                  "incoming values but block has {} predecessors",
                  func_name, block_idx, param_idx, unk_count, pred_count));
        }
        if (unk_count != known_count) {
          throw common::InternalError(
              "PHI wiring",
              std::format(
                  "function '{}' block {} param {}: known PHI has {} incoming "
                  "values but unknown PHI has {}",
                  func_name, block_idx, param_idx, known_count, unk_count));
        }
      }
    }
  }

  // Wire up PHI incoming values from pending edges.
  // Call this after all blocks have been lowered.
  //
  // The decision of whether to split-wire or single-wire is based on
  // entry.unknown != nullptr, NOT on isStructTy() of the incoming value.
  // isStructTy() is used only for unpacking the already-reconstructed raw
  // incoming value, never for deciding whether the destination is split.
  void WireIncomingEdges(llvm::IRBuilder<>& builder) {
    for (const auto& edge : pending_edges) {
      for (size_t j = 0; j < edge.args.size(); ++j) {
        const auto& entry = phis[MakeKey(edge.succ_idx, j)];
        llvm::Value* arg = edge.args[j];

        if (entry.unknown == nullptr) {
          // 2-state param: single PHI, handle integer width mismatch.
          if (entry.known->getType() != arg->getType()) {
            if (entry.known->getType()->isIntegerTy() &&
                arg->getType()->isIntegerTy()) {
              builder.SetInsertPoint(edge.pred->getTerminator());
              arg = builder.CreateZExtOrTrunc(
                  arg, entry.known->getType(), "phi.coerce");
            } else {
              throw common::InternalError(
                  "PHI wiring",
                  std::format(
                      "block {} param {}: 2-state PHI type mismatch",
                      edge.succ_idx, j));
            }
          }
          entry.known->addIncoming(arg, edge.pred);
        } else {
          // 4-state param: split PHI pair.
          auto* plane_type = entry.known->getType();
          builder.SetInsertPoint(edge.pred->getTerminator());

          if (arg->getType()->isStructTy()) {
            // Source is 4-state: extract planes from struct.
            auto fs = ExtractFourState(builder, arg);
            auto* known_val = builder.CreateZExtOrTrunc(
                fs.value, plane_type, "phi.val.coerce");
            auto* unknown_val = builder.CreateZExtOrTrunc(
                fs.unknown, plane_type, "phi.unk.coerce");
            entry.known->addIncoming(known_val, edge.pred);
            entry.unknown->addIncoming(unknown_val, edge.pred);
          } else {
            // Source is 2-state scalar: adapt for split PHIs.
            auto* known_val =
                builder.CreateZExtOrTrunc(arg, plane_type, "phi.val.coerce");
            auto* zero = llvm::ConstantInt::get(plane_type, 0);
            entry.known->addIncoming(known_val, edge.pred);
            entry.unknown->addIncoming(zero, edge.pred);
          }
        }
      }
    }
  }
};

// Create PHI nodes for all blocks with params and bind their temp_ids.
// Must be called after ClearTemps() and before lowering any blocks.
// Returns the PhiWiringState to be used for recording edges and wiring.
auto SetupBlockParamPhis(
    Context& context, const std::vector<mir::BasicBlock>& mir_blocks,
    const std::vector<llvm::BasicBlock*>& llvm_blocks)
    -> Result<PhiWiringState> {
  auto& builder = context.GetBuilder();
  PhiWiringState phi_state;

  for (size_t i = 0; i < mir_blocks.size(); ++i) {
    const auto& block = mir_blocks[i];
    if (!block.params.empty()) {
      builder.SetInsertPoint(llvm_blocks[i], llvm_blocks[i]->begin());
      for (size_t j = 0; j < block.params.size(); ++j) {
        const auto& param = block.params[j];

        if (context.IsFourState(param.type)) {
          // 4-state: create split PHI pair using canonical plane type.
          auto* plane_type = GetFourStatePlaneType(context, param.type);
          auto* known_phi =
              builder.CreatePHI(plane_type, 2, std::format("phi{}.val", j));
          auto* unknown_phi =
              builder.CreatePHI(plane_type, 2, std::format("phi{}.unk", j));
          phi_state.phis[PhiWiringState::MakeKey(i, j)] = {
              .known = known_phi, .unknown = unknown_phi};
          // Bind split TempValue.
          context.BindTempValue(
              param.temp_id, TempValue{
                                 .declared_type = param.type,
                                 .domain = ValueDomain::kFourState,
                                 .value = known_phi,
                                 .unknown = unknown_phi});
        } else {
          // 2-state: single scalar PHI.
          auto llvm_ty_or_err = GetLlvmTypeForType(context, param.type);
          if (!llvm_ty_or_err) return std::unexpected(llvm_ty_or_err.error());
          auto* phi =
              builder.CreatePHI(*llvm_ty_or_err, 2, std::format("phi{}", j));
          phi_state.phis[PhiWiringState::MakeKey(i, j)] = {
              .known = phi, .unknown = nullptr};
          context.BindTempValue(
              param.temp_id, TempValue{
                                 .declared_type = param.type,
                                 .domain = ValueDomain::kTwoState,
                                 .value = phi,
                                 .unknown = nullptr});
        }
      }
    }
  }
  return phi_state;
}

// Verify LLVM function and throw InternalError on failure.
// Provides detailed error message with function name and verification output.
void VerifyLlvmFunction(llvm::Function* func, const char* caller) {
  std::string err_str;
  llvm::raw_string_ostream err_stream(err_str);
  if (llvm::verifyFunction(*func, &err_stream)) {
    throw common::InternalError(
        caller, std::format(
                    "LLVM IR verification failed for '{}': {}",
                    func->getName().str(), err_str));
  }
}

auto LoadConditionAsI1(
    Context& context, SlotAccessResolver& resolver, const mir::Operand& operand)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();

  auto cond_val_or_err = LowerOperandRaw(context, resolver, operand);
  if (!cond_val_or_err) return std::unexpected(cond_val_or_err.error());
  llvm::Value* cond_val = *cond_val_or_err;

  // 4-state struct: extract known-true bits (a & ~b)
  if (cond_val->getType()->isStructTy()) {
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

auto LowerEdgeArgs(
    Context& context, SlotAccessResolver& resolver,
    const std::vector<mir::Operand>& args)
    -> Result<std::vector<llvm::Value*>> {
  std::vector<llvm::Value*> llvm_args;
  llvm_args.reserve(args.size());
  for (const auto& arg : args) {
    auto val_or_err = LowerOperandRaw(context, resolver, arg);
    if (!val_or_err) return std::unexpected(val_or_err.error());
    llvm_args.push_back(*val_or_err);
  }
  return llvm_args;
}

// Check if a terminator has any back-edge targets (target index <= source
// index).
auto HasBackEdge(const mir::Terminator& term, size_t current_block_idx)
    -> bool {
  return std::visit(
      common::Overloaded{
          [&](const mir::Jump& t) -> bool {
            return static_cast<size_t>(t.target.value) <= current_block_idx;
          },
          [&](const mir::Branch& t) -> bool {
            return static_cast<size_t>(t.then_target.value) <=
                       current_block_idx ||
                   static_cast<size_t>(t.else_target.value) <=
                       current_block_idx;
          },
          [](const auto&) -> bool { return false; },
      },
      term.data);
}

// Emit a back-edge guard before a loop back-edge terminator.
// Decrements the TLS iteration limit counter; if exhausted, stores a kTrap
// ProcessOutcome into %out and returns void. The runtime iteration limit
// policy (bounded or saturating-max) determines whether trapping occurs.
//
// limit_ptr is the TLS counter pointer, hoisted to process entry so the
// LyraIterationLimitPtr() call happens once per activation instead of per
// back-edge.
void EmitBackEdgeGuard(
    Context& context, common::OriginId origin, llvm::Function* func,
    llvm::Value* out_ptr, llvm::Value* limit_ptr) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  uint32_t site_id = context.RegisterBackEdgeSite(origin);

  auto* limit = builder.CreateLoad(i32_ty, limit_ptr, "limit");
  auto* new_limit =
      builder.CreateSub(limit, llvm::ConstantInt::get(i32_ty, 1), "limit_dec");
  builder.CreateStore(new_limit, limit_ptr);

  auto* exhausted =
      builder.CreateICmpEQ(new_limit, llvm::ConstantInt::get(i32_ty, 0));

  auto* trap_bb = llvm::BasicBlock::Create(llvm_ctx, "loop_trap", func);
  auto* continue_bb = llvm::BasicBlock::Create(llvm_ctx, "loop_continue", func);

  builder.CreateCondBr(exhausted, trap_bb, continue_bb);

  // Trap block: store kTrap outcome into %out and return void
  builder.SetInsertPoint(trap_bb);
  EmitStoreOutcome(
      builder, llvm_ctx, out_ptr,
      static_cast<uint32_t>(runtime::ProcessExitCode::kTrap),
      static_cast<uint32_t>(runtime::TrapReason::kIterationLimitExceeded),
      llvm::ConstantInt::get(i32_ty, site_id),
      llvm::ConstantInt::get(i32_ty, 0));
  builder.CreateRetVoid();

  // Continue block: terminator will be emitted here
  builder.SetInsertPoint(continue_bb);
}

auto LowerJump(
    Context& context, SlotAccessResolver& resolver, const mir::Jump& jump,
    const std::vector<llvm::BasicBlock*>& blocks, PhiWiringState& phi_state)
    -> Result<void> {
  auto* current_bb = context.GetBuilder().GetInsertBlock();

  std::vector<llvm::Value*> arg_values;
  if (!jump.args.empty()) {
    auto args_or_err = LowerEdgeArgs(context, resolver, jump.args);
    if (!args_or_err) return std::unexpected(args_or_err.error());
    arg_values = std::move(*args_or_err);
  }

  // Create the branch terminator
  context.GetBuilder().CreateBr(blocks[jump.target.value]);

  // Record edge args for PHI wiring (using pre-lowered values)
  if (!arg_values.empty()) {
    phi_state.pending_edges.push_back(
        {.pred = current_bb,
         .succ_idx = static_cast<size_t>(jump.target.value),
         .args = std::move(arg_values)});
  }
  return {};
}

auto LowerJump(
    Context& context, const mir::Jump& jump,
    const std::vector<llvm::BasicBlock*>& blocks, PhiWiringState& phi_state)
    -> Result<void> {
  CanonicalSlotAccess canonical(context);
  return LowerJump(context, canonical, jump, blocks, phi_state);
}

auto LowerBranch(
    Context& context, SlotAccessResolver& resolver, const mir::Branch& branch,
    const std::vector<llvm::BasicBlock*>& blocks, PhiWiringState& phi_state)
    -> Result<void> {
  auto* current_bb = context.GetBuilder().GetInsertBlock();
  auto cond_or_err = LoadConditionAsI1(context, resolver, branch.condition);
  if (!cond_or_err) return std::unexpected(cond_or_err.error());

  std::vector<llvm::Value*> then_llvm_args;
  std::vector<llvm::Value*> else_llvm_args;
  if (!branch.then_args.empty()) {
    auto args_or_err = LowerEdgeArgs(context, resolver, branch.then_args);
    if (!args_or_err) return std::unexpected(args_or_err.error());
    then_llvm_args = std::move(*args_or_err);
  }
  if (!branch.else_args.empty()) {
    auto args_or_err = LowerEdgeArgs(context, resolver, branch.else_args);
    if (!args_or_err) return std::unexpected(args_or_err.error());
    else_llvm_args = std::move(*args_or_err);
  }

  context.GetBuilder().CreateCondBr(
      *cond_or_err, blocks[branch.then_target.value],
      blocks[branch.else_target.value]);

  // Record edge args for PHI wiring
  if (!then_llvm_args.empty()) {
    phi_state.pending_edges.push_back(
        {.pred = current_bb,
         .succ_idx = static_cast<size_t>(branch.then_target.value),
         .args = std::move(then_llvm_args)});
  }
  if (!else_llvm_args.empty()) {
    phi_state.pending_edges.push_back(
        {.pred = current_bb,
         .succ_idx = static_cast<size_t>(branch.else_target.value),
         .args = std::move(else_llvm_args)});
  }
  return {};
}

auto LowerBranch(
    Context& context, const mir::Branch& branch,
    const std::vector<llvm::BasicBlock*>& blocks, PhiWiringState& phi_state)
    -> Result<void> {
  CanonicalSlotAccess canonical(context);
  return LowerBranch(context, canonical, branch, blocks, phi_state);
}

void LowerReturn(Context& context, llvm::BasicBlock* exit_block) {
  context.GetBuilder().CreateBr(exit_block);
}

auto LowerFinish(
    Context& context, const mir::Finish& finish, llvm::BasicBlock* exit_block)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);

  auto kind_val = static_cast<uint32_t>(finish.kind);

  // Message is already a string handle from SFormat lowering, or null
  llvm::Value* message = llvm::ConstantPointerNull::get(ptr_ty);
  if (finish.message.has_value()) {
    auto msg_or_err = LowerOperand(context, *finish.message);
    if (!msg_or_err) return std::unexpected(msg_or_err.error());
    message = *msg_or_err;
  }

  builder.CreateCall(
      context.GetLyraTerminate(),
      {context.GetEnginePointer(), llvm::ConstantInt::get(i32_ty, kind_val),
       llvm::ConstantInt::get(i32_ty, static_cast<uint32_t>(finish.level)),
       message});

  builder.CreateBr(exit_block);
  return {};
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

struct ObservationRange {
  uint32_t byte_offset = 0;
  uint32_t byte_size = 0;  // 0 = full slot
  uint8_t bit_index = 0;
};

auto ResolveObservationRange(Context& context, const mir::WaitTrigger& trigger)
    -> ObservationRange {
  if (!trigger.observed_place) return {};

  const auto& mir_arena = context.GetMirArena();
  const auto& place = mir_arena[*trigger.observed_place];

  auto resolver =
      [&context](const mir::Operand& op) -> std::optional<uint64_t> {
    if (op.kind != mir::Operand::Kind::kUseTemp) return std::nullopt;
    auto temp_id = std::get<mir::TempId>(op.payload);
    if (!context.HasTemp(temp_id.value)) return std::nullopt;
    auto* ci = context.TryGetTempConstantInt(temp_id.value);
    if (ci != nullptr) {
      return ci->getZExtValue();
    }
    return std::nullopt;
  };
  auto slot_id = common::SlotId{static_cast<uint32_t>(place.root.id)};
  const auto& spec = context.GetDesignSlotStorageSpec(slot_id);
  const auto& spec_arena = context.GetDesignStorageSpecArena();
  auto range = ResolveByteRange(spec, spec_arena, place, resolver);
  if (range.kind == RangeKind::kPrecise) {
    return {
        .byte_offset = range.byte_offset,
        .byte_size = range.byte_size,
        .bit_index = range.bit_index};
  }
  if (trigger.edge != common::EdgeKind::kAnyChange) {
    throw common::InternalError(
        "ResolveObservationRange",
        "sub-slot edge observation resolved to full-slot fallback; "
        "the projection chain could not be resolved to a precise "
        "byte range");
  }
  return {};
}

// Emit IR to compute dynamic byte_offset/bit_index from the observed_place's
// storage offset. Uses the already-lowered MIR storage_offset operand and
// emits bounds checking. Stores results into the WaitTriggerRecord fields.
// On OOB, byte_size=0 is stored as a sentinel for the runtime.
void EmitDynamicBitTarget(
    Context& context, llvm::Value* elem_ptr, llvm::Type* trigger_type,
    const mir::WaitTrigger& trigger) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

  // Get storage bit offset from the observed_place's BitRangeProjection.
  const auto& arena = context.GetMirArena();
  const auto& place = arena[*trigger.observed_place];
  const auto& last_proj = place.projections.back();
  const auto& bit_range = std::get<mir::BitRangeProjection>(last_proj.info);

  // Lower the bit_offset operand to get the LLVM Value.
  auto offset_or_err = LowerOperand(context, bit_range.bit_offset);
  if (!offset_or_err) {
    throw common::InternalError(
        "EmitDynamicBitTarget", "failed to lower storage offset operand");
  }
  auto* offset_val = *offset_or_err;
  auto* offset_i64 =
      builder.CreateSExtOrTrunc(offset_val, i64_ty, "idx.offset");

  // Bounds check: 0 <= offset < total_bits.
  const auto& lb = *trigger.late_bound;
  auto* total_bits = llvm::ConstantInt::get(i64_ty, lb.mapping.total_bits);
  auto* zero_64 = llvm::ConstantInt::get(i64_ty, 0);
  auto* ge_zero = builder.CreateICmpSGE(offset_i64, zero_64, "idx.ge0");
  auto* lt_total =
      builder.CreateICmpSLT(offset_i64, total_bits, "idx.lt_total");
  auto* in_bounds = builder.CreateAnd(ge_zero, lt_total, "idx.inbounds");

  // Compute byte_offset and bit_index.
  auto* eight = llvm::ConstantInt::get(i64_ty, 8);
  auto* byte_off_64 = builder.CreateSDiv(offset_i64, eight, "idx.byteoff");
  auto* bit_idx_64 = builder.CreateSRem(offset_i64, eight, "idx.bitidx");
  auto* byte_off_32 = builder.CreateTrunc(byte_off_64, i32_ty);
  auto* bit_idx_8 = builder.CreateTrunc(bit_idx_64, i8_ty);

  // Select: in-bounds -> computed values, OOB -> zero (valid but inactive).
  auto* zero_32 = llvm::ConstantInt::get(i32_ty, 0);
  auto* zero_8 = llvm::ConstantInt::get(i8_ty, 0);
  auto* one_32 = llvm::ConstantInt::get(i32_ty, 1);
  auto* sel_byte_off = builder.CreateSelect(in_bounds, byte_off_32, zero_32);
  auto* sel_bit_idx = builder.CreateSelect(in_bounds, bit_idx_8, zero_8);
  auto* sel_byte_size = builder.CreateSelect(in_bounds, one_32, one_32);

  // Flags: initially active only if in-bounds.
  auto* active_flag =
      llvm::ConstantInt::get(i8_ty, runtime::kTriggerInitiallyActive);
  auto* sel_flags = builder.CreateSelect(in_bounds, active_flag, zero_8);

  // Store into WaitTriggerRecord fields.
  auto* bit_idx_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 2);
  builder.CreateStore(sel_bit_idx, bit_idx_ptr);
  auto* flags_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 4);
  builder.CreateStore(sel_flags, flags_ptr);
  auto* offset_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 5);
  builder.CreateStore(sel_byte_off, offset_ptr);
  auto* size_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 6);
  builder.CreateStore(sel_byte_size, size_ptr);
}

// Emit IR to compute dynamic byte_offset/bit_index for an unpacked array
// element edge trigger. Reads the SV index from the observed_place's
// BitRangeProjection, applies the affine mapping with element_bit_stride
// computed from DataLayout, and stores results into WaitTriggerRecord fields.
void EmitDynamicUnpackedBitTarget(
    Context& context, llvm::Value* elem_ptr, llvm::Type* trigger_type,
    const mir::WaitTrigger& trigger) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

  // Read the SV index from the observed_place's BitRangeProjection.
  const auto& arena = context.GetMirArena();
  const auto& place = arena[*trigger.observed_place];
  const auto& last_proj = place.projections.back();
  const auto& bit_range = std::get<mir::BitRangeProjection>(last_proj.info);

  auto offset_or_err = LowerOperand(context, bit_range.bit_offset);
  if (!offset_or_err) {
    throw common::InternalError(
        "EmitDynamicUnpackedBitTarget", "failed to lower index operand");
  }
  auto* sv_index = builder.CreateSExtOrTrunc(*offset_or_err, i64_ty, "ua.idx");

  // Compute element_bit_stride from DataLayout.
  const auto& lb = *trigger.late_bound;
  auto* elem_llvm_type = GetLlvmTypeForTypeId(
      llvm_ctx, *lb.element_type, context.GetTypeArena(),
      context.IsForceTwoState());
  const auto& dl = context.GetModule().getDataLayout();
  auto elem_alloc = static_cast<uint32_t>(dl.getTypeAllocSize(elem_llvm_type));
  uint32_t element_bit_stride = elem_alloc * 8;
  uint32_t total_bits = lb.num_elements * element_bit_stride;

  // Apply affine mapping: logical_bit = (sv_index - index_base) * index_step
  // where index_step is direction_sign * element_bit_stride.
  auto* index_base = llvm::ConstantInt::get(i64_ty, lb.mapping.index_base);
  auto* diff = builder.CreateSub(sv_index, index_base, "ua.diff");
  int32_t scaled_step =
      lb.mapping.index_step * static_cast<int32_t>(element_bit_stride);
  auto* step = llvm::ConstantInt::getSigned(i64_ty, scaled_step);
  auto* logical_bit = builder.CreateMul(diff, step, "ua.logbit");

  // Bounds check: 0 <= logical_bit < total_bits.
  auto* total = llvm::ConstantInt::get(i64_ty, total_bits);
  auto* zero_64 = llvm::ConstantInt::get(i64_ty, 0);
  auto* ge_zero = builder.CreateICmpSGE(logical_bit, zero_64, "ua.ge0");
  auto* lt_total = builder.CreateICmpSLT(logical_bit, total, "ua.lt_total");
  auto* in_bounds = builder.CreateAnd(ge_zero, lt_total, "ua.inbounds");

  // Compute byte_offset (bit_index is always 0 for byte-aligned elements).
  auto* eight = llvm::ConstantInt::get(i64_ty, 8);
  auto* byte_off_64 = builder.CreateSDiv(logical_bit, eight, "ua.byteoff");
  auto* byte_off_32 = builder.CreateTrunc(byte_off_64, i32_ty);

  // Select: in-bounds -> computed, OOB -> zero (valid but inactive).
  auto* zero_32 = llvm::ConstantInt::get(i32_ty, 0);
  auto* zero_8 = llvm::ConstantInt::get(i8_ty, 0);
  auto* one_32 = llvm::ConstantInt::get(i32_ty, 1);
  auto* sel_byte_off = builder.CreateSelect(in_bounds, byte_off_32, zero_32);
  auto* sel_byte_size = builder.CreateSelect(in_bounds, one_32, one_32);

  // Flags: initially active only if in-bounds.
  auto* active_flag =
      llvm::ConstantInt::get(i8_ty, runtime::kTriggerInitiallyActive);
  auto* sel_flags = builder.CreateSelect(in_bounds, active_flag, zero_8);

  // Store into WaitTriggerRecord fields.
  auto* bit_idx_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 2);
  builder.CreateStore(zero_8, bit_idx_ptr);
  auto* flags_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 4);
  builder.CreateStore(sel_flags, flags_ptr);
  auto* offset_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 5);
  builder.CreateStore(sel_byte_off, offset_ptr);
  auto* size_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 6);
  builder.CreateStore(sel_byte_size, size_ptr);
}

// Emit IR to compute dynamic byte_offset for a container (dyn array / queue)
// element edge trigger. For local indices: evaluate index, OOB check against
// DynArrayData::size, compute byte_offset = index * elem_stride. For
// design-state indices: byte_offset=0, byte_size=1 (RebindSubscription
// corrects).
void EmitDynamicContainerTarget(
    Context& context, llvm::Value* elem_ptr, llvm::Type* trigger_type,
    const mir::WaitTrigger& trigger) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);

  const auto& lb = *trigger.late_bound;

  // Read the SV index from the observed_place's BitRangeProjection.
  const auto& arena = context.GetMirArena();
  const auto& place = arena[*trigger.observed_place];
  const auto& last_proj = place.projections.back();
  const auto& bit_range = std::get<mir::BitRangeProjection>(last_proj.info);

  auto offset_or_err = LowerOperand(context, bit_range.bit_offset);
  if (!offset_or_err) {
    throw common::InternalError(
        "EmitDynamicContainerTarget", "failed to lower index operand");
  }
  auto* sv_index = builder.CreateSExtOrTrunc(*offset_or_err, i64_ty, "ct.idx");

  auto* elem_llvm_type = GetLlvmTypeForTypeId(
      llvm_ctx, *lb.element_type, context.GetTypeArena(),
      context.IsForceTwoState());
  const auto& dl = context.GetModule().getDataLayout();
  auto elem_stride = static_cast<uint32_t>(dl.getTypeAllocSize(elem_llvm_type));

  // Container elem stride is always stored in the trigger record.
  auto* stride_const = llvm::ConstantInt::get(i32_ty, elem_stride);
  auto* stride_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 7);
  builder.CreateStore(stride_const, stride_ptr);

  if (!lb.dep_slots.empty()) {
    // Design-state index: store element index for initial subscription.
    // RebindSubscription will update it when the index changes.
    // For kContainer, byte_offset carries the element index (not byte offset).
    auto* sv_idx_32 = builder.CreateTrunc(sv_index, i32_ty, "ct.ds.idx");
    auto* zero_8 = llvm::ConstantInt::get(i8_ty, 0);
    auto* zero_32 = llvm::ConstantInt::get(i32_ty, 0);
    auto* active_flag =
        llvm::ConstantInt::get(i8_ty, runtime::kTriggerInitiallyActive);
    auto* bit_idx_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 2);
    builder.CreateStore(zero_8, bit_idx_ptr);
    auto* flags_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 4);
    builder.CreateStore(active_flag, flags_ptr);
    auto* offset_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 5);
    builder.CreateStore(sv_idx_32, offset_ptr);
    auto* size_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 6);
    builder.CreateStore(zero_32, size_ptr);
    return;
  }

  // Local-index path: load container handle from slot.
  auto* slot_ptr = context.GetSignalSlotPointer(trigger.signal);
  auto* handle = builder.CreateLoad(ptr_ty, slot_ptr, "ct.handle");

  // Check null handle.
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(llvm_ctx));
  auto* is_null = builder.CreateICmpEQ(handle, null_ptr, "ct.null");

  // Load DynArrayData::size (offset: magic(8) + epoch(8) + data(8) = 24).
  auto* size_ptr_gep =
      builder.CreateConstInBoundsGEP1_64(i8_ty, handle, 24, "ct.szptr");
  auto* arr_size = builder.CreateLoad(i64_ty, size_ptr_gep, "ct.sz");

  // OOB check: 0 <= sv_index < arr_size.
  auto* zero_64 = llvm::ConstantInt::get(i64_ty, 0);
  auto* ge_zero = builder.CreateICmpSGE(sv_index, zero_64, "ct.ge0");
  auto* lt_size = builder.CreateICmpSLT(sv_index, arr_size, "ct.lt_sz");
  auto* in_bounds = builder.CreateAnd(ge_zero, lt_size, "ct.inbounds");
  auto* not_null_and_in_bounds =
      builder.CreateAnd(builder.CreateNot(is_null), in_bounds, "ct.valid");

  // For kContainer, byte_offset carries the element index (not byte offset).
  auto* sv_idx_32 = builder.CreateTrunc(sv_index, i32_ty, "ct.idx32");

  auto* zero_32 = llvm::ConstantInt::get(i32_ty, 0);
  auto* zero_8 = llvm::ConstantInt::get(i8_ty, 0);

  auto* sel_elem_idx =
      builder.CreateSelect(not_null_and_in_bounds, sv_idx_32, zero_32);

  // Flags: initially active only if in-bounds and not null.
  auto* active_flag =
      llvm::ConstantInt::get(i8_ty, runtime::kTriggerInitiallyActive);
  auto* sel_flags =
      builder.CreateSelect(not_null_and_in_bounds, active_flag, zero_8);

  auto* bit_idx_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 2);
  builder.CreateStore(zero_8, bit_idx_ptr);
  auto* flags_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 4);
  builder.CreateStore(sel_flags, flags_ptr);
  auto* offset_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 5);
  builder.CreateStore(sel_elem_idx, offset_ptr);
  auto* size_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 6);
  builder.CreateStore(zero_32, size_ptr);
}

// Determine the TriggerInstallKind for a given trigger.
auto TriggerKindFromEdge(common::EdgeKind edge) -> runtime::TriggerInstallKind {
  if (edge == common::EdgeKind::kAnyChange) {
    return runtime::TriggerInstallKind::kChange;
  }
  return runtime::TriggerInstallKind::kEdge;
}

// Helper to fill trigger array at a given base pointer
void FillTriggerArray(
    Context& context, llvm::Value* base_ptr, llvm::Type* trigger_type,
    llvm::Type* array_type, const std::vector<mir::WaitTrigger>& triggers) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  for (uint32_t i = 0; i < triggers.size(); ++i) {
    auto* elem_ptr = builder.CreateConstGEP2_32(array_type, base_ptr, 0, i);

    // Write signal_id (field 0).
    // R5: local signals emit body-local LocalSignalId directly (no flat
    // base addition). Global signals emit GlobalSignalId. The domain
    // is recorded in the flags field (kTriggerLocalSignal).
    auto* signal_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 0);
    auto signal_expr = context.EmitSignalCoord(triggers[i].signal);
    bool is_local_signal = signal_expr.IsLocal();
    builder.CreateStore(signal_expr.Emit(builder), signal_ptr);

    // Write edge (field 1)
    auto* edge_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 1);
    builder.CreateStore(
        llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(triggers[i].edge)),
        edge_ptr);

    // Write kind (field 3) -- determined by trigger classification.
    bool is_container =
        triggers[i].late_bound && triggers[i].late_bound->is_container;
    auto install_kind = is_container ? runtime::TriggerInstallKind::kContainer
                                     : TriggerKindFromEdge(triggers[i].edge);
    auto* kind_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 3);
    builder.CreateStore(
        llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(install_kind)),
        kind_ptr);

    if (is_container) {
      // Container element: emit dynamic container target.
      // container_elem_stride and flags are set by EmitDynamicContainerTarget.
      EmitDynamicContainerTarget(context, elem_ptr, trigger_type, triggers[i]);
    } else if (triggers[i].late_bound && triggers[i].late_bound->element_type) {
      // Dynamic unpacked array element.
      // container_elem_stride = 0 (non-container), flags set by Emit*.
      auto* stride_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 7);
      builder.CreateStore(llvm::ConstantInt::get(i32_ty, 0), stride_ptr);
      EmitDynamicUnpackedBitTarget(
          context, elem_ptr, trigger_type, triggers[i]);
    } else if (triggers[i].late_bound) {
      // Dynamic bit-select.
      // container_elem_stride = 0 (non-container), flags set by Emit*.
      auto* stride_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 7);
      builder.CreateStore(llvm::ConstantInt::get(i32_ty, 0), stride_ptr);
      EmitDynamicBitTarget(context, elem_ptr, trigger_type, triggers[i]);
    } else {
      // Static path: resolve observation range at compile time.
      // Always active, container_elem_stride = 0.
      auto range = ResolveObservationRange(context, triggers[i]);
      auto* bit_idx_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 2);
      builder.CreateStore(
          llvm::ConstantInt::get(i8_ty, range.bit_index), bit_idx_ptr);
      auto* flags_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 4);
      uint8_t trigger_flags = runtime::kTriggerInitiallyActive;
      if (is_local_signal) {
        trigger_flags |= runtime::kTriggerLocalSignal;
      }
      builder.CreateStore(
          llvm::ConstantInt::get(i8_ty, trigger_flags), flags_ptr);
      auto* offset_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 5);
      builder.CreateStore(
          llvm::ConstantInt::get(i32_ty, range.byte_offset), offset_ptr);
      auto* size_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 6);
      builder.CreateStore(
          llvm::ConstantInt::get(i32_ty, range.byte_size), size_ptr);
      auto* stride_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 7);
      builder.CreateStore(llvm::ConstantInt::get(i32_ty, 0), stride_ptr);
    }

    // R5: set domain flags and cross-instance identity. Done after the
    // per-kind path because each path writes its own flags value.
    if (is_local_signal) {
      uint8_t local_flags = runtime::kTriggerLocalSignal;
      auto cross_id = signal_expr.GetInstanceIdOverride();
      if (cross_id.has_value()) {
        local_flags |= runtime::kTriggerCrossInstanceLocal;
      }
      auto* flags_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 4);
      auto* current = builder.CreateLoad(i8_ty, flags_ptr, "trigger.flags");
      auto* with_local = builder.CreateOr(
          current, llvm::ConstantInt::get(i8_ty, local_flags),
          "trigger.flags.local");
      builder.CreateStore(with_local, flags_ptr);

      // Cross-instance: write target_instance_id (field 8) and
      // target_local_signal_id (field 9).
      if (cross_id.has_value()) {
        auto* inst_id_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 8);
        builder.CreateStore(
            llvm::ConstantInt::get(i32_ty, cross_id->value), inst_id_ptr);
        auto* local_id_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 9);
        builder.CreateStore(signal_expr.Emit(builder), local_id_ptr);
      }
    }
  }
}

// Emitted data for late-bound triggers: headers, plan_ops pool, dep_slots pool.
struct LateBoundEmitResult {
  llvm::Value* headers_ptr = nullptr;
  uint32_t num_headers = 0;
  llvm::Value* plan_ops_ptr = nullptr;
  uint32_t num_plan_ops = 0;
  llvm::Value* dep_slots_ptr = nullptr;
  uint32_t num_dep_slots = 0;
};

// Emit late-bound data arrays for dynamic-index edge triggers.
auto EmitLateBoundData(
    Context& context, const std::vector<mir::WaitTrigger>& triggers)
    -> LateBoundEmitResult {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i16_ty = llvm::Type::getInt16Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);

  // Collect late-bound triggers with dep_slots (need runtime rebinding).
  struct LbEntry {
    uint32_t trigger_idx;
    const mir::LateBoundIndex* lb;
  };
  std::vector<LbEntry> entries;
  uint32_t total_plan_ops = 0;
  uint32_t total_dep_slots = 0;
  for (uint32_t i = 0; i < triggers.size(); ++i) {
    if (!triggers[i].late_bound) continue;
    const auto& lb = *triggers[i].late_bound;
    // Include entries with dep_slots (need runtime rebinding) or
    // container triggers (need container_elem_stride even without rebinding).
    if (lb.dep_slots.empty() && !lb.is_container) continue;
    entries.push_back({i, &lb});
    total_plan_ops += static_cast<uint32_t>(lb.plan.size());
    total_dep_slots += static_cast<uint32_t>(lb.dep_slots.size());
  }

  if (entries.empty()) return {};

  auto num_headers = static_cast<uint32_t>(entries.size());

  // LateBoundHeader: {i32, i16, i16, i16, i16, i32, i32, i32, i32} = 28 bytes
  static_assert(sizeof(runtime::LateBoundHeader) == 28);
  auto* hdr_type = llvm::StructType::get(
      llvm_ctx,
      {i32_ty, i16_ty, i16_ty, i16_ty, i16_ty, i32_ty, i32_ty, i32_ty, i32_ty});
  auto* hdr_array_type = llvm::ArrayType::get(hdr_type, num_headers);
  auto* hdr_alloca =
      builder.CreateAlloca(hdr_array_type, nullptr, "lb_headers");

  // IndexPlanOp: {i8, i8, i8, i8, i32, i32, i32, i64} = 24 bytes
  static_assert(sizeof(runtime::IndexPlanOp) == 24);
  auto* plan_op_type = llvm::StructType::get(
      llvm_ctx, {i8_ty, i8_ty, i8_ty, i8_ty, i32_ty, i32_ty, i32_ty, i64_ty});

  llvm::Value* plan_ops_alloca = nullptr;
  if (total_plan_ops > 0) {
    auto* plan_array_type = llvm::ArrayType::get(plan_op_type, total_plan_ops);
    plan_ops_alloca =
        builder.CreateAlloca(plan_array_type, nullptr, "lb_plan_ops");
  }

  // DepSignalRecord: {i32 signal_id, i8 flags, [3 x i8] padding,
  //                   i32 target_instance_id, i32 target_local_signal_id}
  auto* dep_record_type = llvm::StructType::get(
      llvm_ctx,
      {i32_ty, i8_ty, llvm::ArrayType::get(i8_ty, 3), i32_ty, i32_ty});
  llvm::Value* dep_slots_alloca = nullptr;
  if (total_dep_slots > 0) {
    auto* dep_array_type =
        llvm::ArrayType::get(dep_record_type, total_dep_slots);
    dep_slots_alloca =
        builder.CreateAlloca(dep_array_type, nullptr, "lb_dep_slots");
  }

  // Fill data.
  uint32_t plan_offset = 0;
  uint32_t dep_offset = 0;
  for (uint32_t h = 0; h < num_headers; ++h) {
    const auto& entry = entries[h];
    const auto& lb = *entry.lb;

    auto plan_count = static_cast<uint16_t>(lb.plan.size());
    auto dep_count = static_cast<uint16_t>(lb.dep_slots.size());

    // Fill header.
    auto* hdr_ptr =
        builder.CreateConstGEP2_32(hdr_array_type, hdr_alloca, 0, h);

    auto* f0 = builder.CreateStructGEP(hdr_type, hdr_ptr, 0);
    builder.CreateStore(llvm::ConstantInt::get(i32_ty, entry.trigger_idx), f0);
    auto* f1 = builder.CreateStructGEP(hdr_type, hdr_ptr, 1);
    builder.CreateStore(llvm::ConstantInt::get(i16_ty, plan_offset), f1);
    auto* f2 = builder.CreateStructGEP(hdr_type, hdr_ptr, 2);
    builder.CreateStore(llvm::ConstantInt::get(i16_ty, plan_count), f2);
    auto* f3 = builder.CreateStructGEP(hdr_type, hdr_ptr, 3);
    builder.CreateStore(llvm::ConstantInt::get(i16_ty, dep_offset), f3);
    auto* f4 = builder.CreateStructGEP(hdr_type, hdr_ptr, 4);
    builder.CreateStore(llvm::ConstantInt::get(i16_ty, dep_count), f4);
    int32_t index_step = lb.mapping.index_step;
    uint32_t total_bits = lb.mapping.total_bits;
    uint32_t container_elem_stride = 0;
    if (lb.is_container && lb.element_type) {
      auto* elem_type = GetLlvmTypeForTypeId(
          llvm_ctx, *lb.element_type, context.GetTypeArena(),
          context.IsForceTwoState());
      const auto& dl = context.GetModule().getDataLayout();
      container_elem_stride =
          static_cast<uint32_t>(dl.getTypeAllocSize(elem_type));
    } else if (lb.element_type && !lb.is_container) {
      auto* elem_type = GetLlvmTypeForTypeId(
          llvm_ctx, *lb.element_type, context.GetTypeArena(),
          context.IsForceTwoState());
      const auto& dl = context.GetModule().getDataLayout();
      auto elem_alloc = static_cast<uint32_t>(dl.getTypeAllocSize(elem_type));
      uint32_t element_bit_stride = elem_alloc * 8;
      index_step =
          lb.mapping.index_step * static_cast<int32_t>(element_bit_stride);
      total_bits = lb.num_elements * element_bit_stride;
    }

    auto* f5 = builder.CreateStructGEP(hdr_type, hdr_ptr, 5);
    builder.CreateStore(
        llvm::ConstantInt::get(
            i32_ty, static_cast<uint32_t>(lb.mapping.index_base)),
        f5);
    auto* f6 = builder.CreateStructGEP(hdr_type, hdr_ptr, 6);
    builder.CreateStore(
        llvm::ConstantInt::get(i32_ty, static_cast<uint32_t>(index_step)), f6);
    auto* f7 = builder.CreateStructGEP(hdr_type, hdr_ptr, 7);
    builder.CreateStore(llvm::ConstantInt::get(i32_ty, total_bits), f7);
    auto* f8 = builder.CreateStructGEP(hdr_type, hdr_ptr, 8);
    builder.CreateStore(
        llvm::ConstantInt::get(i32_ty, container_elem_stride), f8);

    // Fill plan ops. Module-local reads emit body-local signal_id with
    // kIndexPlanLocalSignal flag; runtime resolves via the instance.
    if (plan_ops_alloca != nullptr) {
      auto* plan_array_type =
          llvm::ArrayType::get(plan_op_type, total_plan_ops);
      for (uint32_t p = 0; p < lb.plan.size(); ++p) {
        const auto& scoped_op = lb.plan[p];
        auto op = scoped_op.op;
        auto* op_ptr = builder.CreateConstGEP2_32(
            plan_array_type, plan_ops_alloca, 0, plan_offset + p);

        auto* pf0 = builder.CreateStructGEP(plan_op_type, op_ptr, 0);
        builder.CreateStore(
            llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(op.kind)), pf0);
        auto* pf1 = builder.CreateStructGEP(plan_op_type, op_ptr, 1);
        builder.CreateStore(llvm::ConstantInt::get(i8_ty, op.bit_width), pf1);
        auto* pf2 = builder.CreateStructGEP(plan_op_type, op_ptr, 2);
        builder.CreateStore(llvm::ConstantInt::get(i8_ty, op.is_signed), pf2);
        auto* pf3 = builder.CreateStructGEP(plan_op_type, op_ptr, 3);
        builder.CreateStore(llvm::ConstantInt::get(i8_ty, op.byte_size), pf3);
        // Resolve slot_id: module-local emits body-local signal_id with
        // kIndexPlanLocalSignal flag; design-global emits flat slot_id.
        auto* pf4 = builder.CreateStructGEP(plan_op_type, op_ptr, 4);
        auto* pf6 = builder.CreateStructGEP(plan_op_type, op_ptr, 6);
        if (op.kind == runtime::IndexPlanOp::Kind::kReadSlot &&
            scoped_op.slot_scope == mir::ScopedSlotRef::Scope::kModuleLocal) {
          auto signal_expr = context.EmitSignalCoord(
              mir::SignalRef{
                  .scope = mir::SignalRef::Scope::kModuleLocal,
                  .id = op.slot_id});
          builder.CreateStore(signal_expr.Emit(builder), pf4);
          builder.CreateStore(
              llvm::ConstantInt::get(i32_ty, runtime::kIndexPlanLocalSignal),
              pf6);
        } else {
          builder.CreateStore(llvm::ConstantInt::get(i32_ty, op.slot_id), pf4);
          builder.CreateStore(llvm::ConstantInt::get(i32_ty, 0), pf6);
        }
        auto* pf5 = builder.CreateStructGEP(plan_op_type, op_ptr, 5);
        builder.CreateStore(
            llvm::ConstantInt::get(i32_ty, op.byte_offset), pf5);
        auto* pf7 = builder.CreateStructGEP(plan_op_type, op_ptr, 7);
        builder.CreateStore(
            llvm::ConstantInt::get(
                i64_ty, static_cast<uint64_t>(op.const_value)),
            pf7);
      }
    }

    // Fill dep records. Each dep carries its own domain identity.
    if (dep_slots_alloca != nullptr) {
      auto* dep_array_type =
          llvm::ArrayType::get(dep_record_type, total_dep_slots);
      for (uint32_t d = 0; d < lb.dep_slots.size(); ++d) {
        const auto& ref = lb.dep_slots[d];
        auto mir_scope = (ref.scope == mir::ScopedSlotRef::Scope::kModuleLocal)
                             ? mir::SignalRef::Scope::kModuleLocal
                             : mir::SignalRef::Scope::kDesignGlobal;
        auto signal_expr = context.EmitSignalCoord(
            mir::SignalRef{.scope = mir_scope, .id = ref.id});
        // Use the resolved domain, not the original MIR scope.
        // EmitSignalCoord reclassifies instance-owned design-global
        // signals to local with the correct target instance.
        auto* signal_id = signal_expr.Emit(builder);
        uint8_t dep_flags =
            signal_expr.IsLocal() ? runtime::kDepLocalSignal : 0x00;
        auto cross_id = signal_expr.GetInstanceIdOverride();
        if (cross_id.has_value()) {
          dep_flags |= runtime::kDepCrossInstanceLocal;
        }

        auto* rec_ptr = builder.CreateConstGEP2_32(
            dep_array_type, dep_slots_alloca, 0, dep_offset + d);
        auto* id_ptr = builder.CreateStructGEP(dep_record_type, rec_ptr, 0);
        builder.CreateStore(signal_id, id_ptr);
        auto* flags_ptr = builder.CreateStructGEP(dep_record_type, rec_ptr, 1);
        builder.CreateStore(
            llvm::ConstantInt::get(i8_ty, dep_flags), flags_ptr);
        // Cross-instance fields (fields 3, 4).
        auto* dep_inst_ptr =
            builder.CreateStructGEP(dep_record_type, rec_ptr, 3);
        auto* dep_local_ptr =
            builder.CreateStructGEP(dep_record_type, rec_ptr, 4);
        if (cross_id.has_value()) {
          builder.CreateStore(
              llvm::ConstantInt::get(i32_ty, cross_id->value), dep_inst_ptr);
          builder.CreateStore(signal_id, dep_local_ptr);
        } else {
          builder.CreateStore(llvm::ConstantInt::get(i32_ty, 0), dep_inst_ptr);
          builder.CreateStore(llvm::ConstantInt::get(i32_ty, 0), dep_local_ptr);
        }
      }
    }

    plan_offset += plan_count;
    dep_offset += dep_count;
  }

  return {
      .headers_ptr = hdr_alloca,
      .num_headers = num_headers,
      .plan_ops_ptr = plan_ops_alloca,
      .num_plan_ops = total_plan_ops,
      .dep_slots_ptr = dep_slots_alloca,
      .num_dep_slots = total_dep_slots};
}

auto LowerWait(
    Context& context, const mir::Wait& wait, llvm::BasicBlock* exit_block,
    std::vector<WaitSiteEntry>& wait_sites) -> Result<void> {
  static_assert(sizeof(runtime::WaitTriggerRecord) == 28);

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  auto num_triggers = static_cast<uint32_t>(wait.triggers.size());

  // WaitTriggerRecord layout:
  // {i32 signal_id, i8 edge, i8 bit_index, i8 kind, i8 flags,
  //  i32 byte_offset, i32 byte_size, i32 container_elem_stride,
  //  i32 target_instance_id, i32 target_local_signal_id}
  auto* trigger_type = llvm::StructType::get(
      llvm_ctx, {i32_ty, i8_ty, i8_ty, i8_ty, i8_ty, i32_ty, i32_ty, i32_ty,
                 i32_ty, i32_ty});

  // Emit late-bound data (headers + plan ops pool + dep slots pool).
  auto lb_data = EmitLateBoundData(context, wait.triggers);

  // Allocate wait-site ID and record entry for the caller.
  bool has_late_bound = (lb_data.num_headers > 0);
  bool has_container = std::ranges::any_of(wait.triggers, [](const auto& t) {
    return t.late_bound && t.late_bound->is_container;
  });
  uint32_t wait_site_id = context.NextWaitSiteId();
  wait_sites.push_back(
      WaitSiteEntry{
          .resume_block = wait.resume.value,
          .num_triggers = num_triggers,
          .has_late_bound = has_late_bound,
          .has_container = has_container});
  auto* wait_site_id_val = llvm::ConstantInt::get(i32_ty, wait_site_id);

  // Compile-time branching: different codegen for small vs large trigger counts
  if (num_triggers <= runtime::kInlineTriggerCapacity) {
    // Small path: fixed-size stack allocation (never dynamic alloca)
    auto* fixed_array_type =
        llvm::ArrayType::get(trigger_type, runtime::kInlineTriggerCapacity);
    auto* triggers_alloca =
        builder.CreateAlloca(fixed_array_type, nullptr, "triggers");

    FillTriggerArray(
        context, triggers_alloca, trigger_type, fixed_array_type,
        wait.triggers);

    if (has_late_bound) {
      auto* null_ptr = llvm::ConstantPointerNull::get(
          llvm::PointerType::getUnqual(llvm_ctx));
      builder.CreateCall(
          context.GetLyraSuspendWaitWithLateBound(),
          {context.GetStatePointer(),
           llvm::ConstantInt::get(i32_ty, wait.resume.value), triggers_alloca,
           llvm::ConstantInt::get(i32_ty, num_triggers), lb_data.headers_ptr,
           llvm::ConstantInt::get(i32_ty, lb_data.num_headers),
           lb_data.plan_ops_ptr != nullptr ? lb_data.plan_ops_ptr : null_ptr,
           llvm::ConstantInt::get(i32_ty, lb_data.num_plan_ops),
           lb_data.dep_slots_ptr != nullptr ? lb_data.dep_slots_ptr : null_ptr,
           llvm::ConstantInt::get(i32_ty, lb_data.num_dep_slots),
           wait_site_id_val});
    } else {
      builder.CreateCall(
          context.GetLyraSuspendWait(),
          {context.GetStatePointer(),
           llvm::ConstantInt::get(i32_ty, wait.resume.value), triggers_alloca,
           llvm::ConstantInt::get(i32_ty, num_triggers), wait_site_id_val});
    }
  } else {
    // Large path: runtime allocates scratch buffer, we fill it, runtime copies
    auto* heap_ptr = builder.CreateCall(
        context.GetLyraAllocTriggers(),
        {llvm::ConstantInt::get(i32_ty, num_triggers)}, "heap_triggers");

    auto* array_type = llvm::ArrayType::get(trigger_type, num_triggers);
    FillTriggerArray(
        context, heap_ptr, trigger_type, array_type, wait.triggers);

    if (has_late_bound) {
      auto* null_ptr = llvm::ConstantPointerNull::get(
          llvm::PointerType::getUnqual(llvm_ctx));
      builder.CreateCall(
          context.GetLyraSuspendWaitWithLateBound(),
          {context.GetStatePointer(),
           llvm::ConstantInt::get(i32_ty, wait.resume.value), heap_ptr,
           llvm::ConstantInt::get(i32_ty, num_triggers), lb_data.headers_ptr,
           llvm::ConstantInt::get(i32_ty, lb_data.num_headers),
           lb_data.plan_ops_ptr != nullptr ? lb_data.plan_ops_ptr : null_ptr,
           llvm::ConstantInt::get(i32_ty, lb_data.num_plan_ops),
           lb_data.dep_slots_ptr != nullptr ? lb_data.dep_slots_ptr : null_ptr,
           llvm::ConstantInt::get(i32_ty, lb_data.num_dep_slots),
           wait_site_id_val});
    } else {
      builder.CreateCall(
          context.GetLyraSuspendWait(),
          {context.GetStatePointer(),
           llvm::ConstantInt::get(i32_ty, wait.resume.value), heap_ptr,
           llvm::ConstantInt::get(i32_ty, num_triggers), wait_site_id_val});
    }

    // Free scratch buffer (runtime already copied to SuspendRecord storage)
    builder.CreateCall(context.GetLyraFreeTriggers(), {heap_ptr});
  }

  builder.CreateBr(exit_block);
  return {};
}

void LowerRepeat(Context& context, llvm::BasicBlock* exit_block) {
  auto& builder = context.GetBuilder();

  builder.CreateCall(
      context.GetLyraSuspendRepeat(), {context.GetStatePointer()});

  builder.CreateBr(exit_block);
}

auto LowerTerminator(
    Context& context, SlotAccessResolver& resolver, const mir::Terminator& term,
    const std::vector<llvm::BasicBlock*>& blocks, llvm::BasicBlock* exit_block,
    PhiWiringState& phi_state, std::vector<WaitSiteEntry>& wait_sites)
    -> Result<void> {
  // Set origin for error reporting.
  // OriginScope preserves outer (function/process) origin if term.origin is
  // Invalid.
  OriginScope origin_scope(context, term.origin);

  return std::visit(
      common::Overloaded{
          [&](const mir::Jump& t) -> Result<void> {
            return LowerJump(context, resolver, t, blocks, phi_state);
          },
          [&](const mir::Branch& t) -> Result<void> {
            return LowerBranch(context, resolver, t, blocks, phi_state);
          },
          [&](const mir::Return&) -> Result<void> {
            LowerReturn(context, exit_block);
            return {};
          },
          [&](const mir::Finish& f) -> Result<void> {
            return LowerFinish(context, f, exit_block);
          },
          [&](const mir::Delay& d) -> Result<void> {
            LowerDelay(context, d, exit_block);
            return {};
          },
          [&](const mir::Wait& w) -> Result<void> {
            return LowerWait(context, w, exit_block, wait_sites);
          },
          [&](const mir::Repeat&) -> Result<void> {
            LowerRepeat(context, exit_block);
            return {};
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

auto LowerTerminator(
    Context& context, const mir::Terminator& term,
    const std::vector<llvm::BasicBlock*>& blocks, llvm::BasicBlock* exit_block,
    PhiWiringState& phi_state, std::vector<WaitSiteEntry>& wait_sites)
    -> Result<void> {
  CanonicalSlotAccess canonical(context);
  return LowerTerminator(
      context, canonical, term, blocks, exit_block, phi_state, wait_sites);
}

}  // namespace

// Materialize local/temp storage as allocas for suspension-free processes.
// Called after EmitProcessStateSetup so that frame_ptr_ is set (but unused).
// Sets up function-scope alloca management and creates initialized storage
// for each root in alloca_roots. Returns error if any storage creation fails.
//
// Phase 1: conservative whole-process classification. A process is
// suspension-free iff it has no Delay/Wait terminators. Future phases may
// refine to per-root classification for mixed processes.
static auto MaterializeAllocaStorage(
    Context& context, llvm::Function& func, const ProcessLayout& proc_layout)
    -> Result<void> {
  if (proc_layout.has_suspension) return {};

  context.BeginFunction(func);
  for (const auto& root : proc_layout.alloca_roots) {
    mir::PlaceRoot mir_root{
        .kind = root.key.kind, .id = root.key.id, .type = root.type};
    auto alloca_result = context.GetOrCreatePlaceStorage(mir_root);
    if (!alloca_result) return std::unexpected(alloca_result.error());
    context.InitializePlaceStorage(*alloca_result, root.type);
  }
  return {};
}

auto GenerateProcessFunction(
    Context& context, const mir::Process& process, const std::string& name,
    ProcessExecutionKind execution_kind) -> Result<ProcessCodegenResult> {
  // Process-level origin scope for errors during code generation.
  // If process.origin is Invalid, this is a no-op.
  OriginScope proc_scope(context, process.origin);
  std::vector<WaitSiteEntry> wait_sites;

  const bool is_simulation =
      execution_kind == ProcessExecutionKind::kSimulation;

  // RAII scope for execution-contract state. Saves and restores all
  // per-function state (design_store_mode, engine_ptr, etc.) on exit.
  ExecutionContractScope contract_scope(
      context, is_simulation ? DesignStoreMode::kNotifySimulation
                             : DesignStoreMode::kDirectInit);

  // Identify provably bounded back-edges to skip iteration-limit guards.
  auto bounded_latches = BuildBoundedLatchSet(
      mir::passes::FindBoundedBackEdges(
          process.blocks, context.GetMirArena(), context.GetTypeArena()));

  // Identify qualifying canonical loops for deferred notification.
  // Only applies to simulation processes (init uses kDirectInit which
  // already skips notification entirely).
  auto deferred_loops =
      is_simulation
          ? mir::passes::FindDeferredNotificationLoops(
                process.blocks, context.GetMirArena(), context.GetTypeArena())
          : std::vector<mir::passes::DeferredNotificationLoop>{};
  auto deferred_blocks = BuildDeferredPolicyBlocks(deferred_loops);

  auto& llvm_ctx = context.GetLlvmContext();
  auto& module = context.GetModule();

  // Create function type: void(ptr %state, i32 %resume_block, ptr %out)
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* void_ty = llvm::Type::getVoidTy(llvm_ctx);
  auto* fn_type =
      llvm::FunctionType::get(void_ty, {ptr_ty, i32_ty, ptr_ty}, false);

  auto* func = llvm::Function::Create(
      fn_type, llvm::Function::InternalLinkage, name, &module);

  // Name the arguments
  auto* state_arg = func->getArg(0);
  auto* resume_block_arg = func->getArg(1);
  auto* out_arg = func->getArg(2);
  state_arg->setName("state");
  resume_block_arg->setName("resume_block");
  out_arg->setName("out");

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

  if (!is_simulation) {
    ValidateInitProcessContract(process);
  }

  // Entry block: set up cached pointers and dispatch
  builder.SetInsertPoint(entry_block);

  if (is_simulation) {
    // Simulation contract: load all state pointers including engine.
    context.EmitProcessStateSetup(state_arg);
  } else {
    // Init contract: load design_ptr and frame_ptr only. No engine.
    context.SetStatePointer(state_arg);
    context.SetDesignPointer(context.EmitLoadDesignPtr(state_arg));
    auto* frame_ptr = builder.CreateStructGEP(
        context.GetProcessStateType(), state_arg, 1, "frame_ptr");
    context.SetFramePointer(frame_ptr);
  }

  const auto& proc_layout =
      context.GetLayout().processes[context.GetCurrentProcessIndex()];
  auto alloca_result = MaterializeAllocaStorage(context, *func, proc_layout);
  if (!alloca_result) return std::unexpected(alloca_result.error());

  // Hoist TLS iteration counter pointer to process entry (once per
  // activation instead of once per back-edge).
  auto* limit_ptr =
      builder.CreateCall(context.GetLyraIterationLimitPtr(), {}, "limit_ptr");

  if (is_simulation) {
    // Collect actual resume targets: blocks jumped to after Delay/Wait.
    std::vector<size_t> resume_targets;
    for (const auto& block : process.blocks) {
      std::visit(
          [&](const auto& term) {
            using T = std::decay_t<decltype(term)>;
            if constexpr (std::is_same_v<T, mir::Delay>) {
              resume_targets.push_back(term.resume.value);
            } else if constexpr (std::is_same_v<T, mir::Wait>) {
              resume_targets.push_back(term.resume.value);
            }
          },
          block.terminator.data);
    }
    std::ranges::sort(resume_targets);
    auto [first, last] = std::ranges::unique(resume_targets);
    resume_targets.erase(first, last);

    // Assert: resume target blocks (and bb0) must NOT have block params.
    if (!process.blocks[0].params.empty()) {
      throw common::InternalError(
          "GenerateProcessFunction", std::format(
                                         "entry block 0 has {} params; entry "
                                         "block must have no block params",
                                         process.blocks[0].params.size()));
    }
    for (size_t target : resume_targets) {
      if (!process.blocks[target].params.empty()) {
        throw common::InternalError(
            "GenerateProcessFunction",
            std::format(
                "resume target block {} has {} params; resume targets must "
                "have no block params",
                target, process.blocks[target].params.size()));
      }
    }

    // Resume dispatch switch with bb0 as default (initial execution).
    auto* sw = builder.CreateSwitch(
        resume_block_arg, llvm_blocks[0],
        static_cast<unsigned>(resume_targets.size()));
    for (size_t target : resume_targets) {
      sw->addCase(
          llvm::ConstantInt::get(i32_ty, static_cast<uint64_t>(target)),
          llvm_blocks[target]);
    }
  } else {
    // Init contract: always enters at bb0, no resume dispatch.
    builder.CreateBr(llvm_blocks[0]);
  }

  // Clear stale temp bindings and setup PHI nodes for block params
  context.ClearTemps();
  auto phi_state_or_err =
      SetupBlockParamPhis(context, process.blocks, llvm_blocks);
  if (!phi_state_or_err) return std::unexpected(phi_state_or_err.error());
  auto& phi_state = *phi_state_or_err;

  // Lower each MIR block
  for (size_t i = 0; i < process.blocks.size(); ++i) {
    const auto& block = process.blocks[i];
    builder.SetInsertPoint(llvm_blocks[i]);

    // Scoped notification policy: kDeferred for blocks inside a
    // qualifying non-yielding loop, kImmediate otherwise. RAII guard
    // restores the policy on scope exit (including early returns).
    auto policy =
        std::ranges::binary_search(deferred_blocks, static_cast<uint32_t>(i))
            ? NotificationPolicy::kDeferred
            : NotificationPolicy::kImmediate;
    NotificationPolicyScope policy_scope(context, policy);

    // Loop-exit edge effect: emit deferred dirty-marks for qualifying
    // loops whose exit edge targets this block. These marks model the
    // edge leaving the loop, not block-entry logic. They must appear
    // before any non-PHI work in this block.
    for (const auto& loop : deferred_loops) {
      if (loop.exit_block == static_cast<uint32_t>(i)) {
        EmitLoopExitDeferredMarks(context, loop.notified_roots);
      }
    }

    for (const auto& instruction : block.statements) {
      auto result = LowerStatement(context, instruction);
      if (!result) return std::unexpected(result.error());
    }

    // Emit back-edge guard before loop back-edge terminators.
    // Skip for provably bounded canonical for-loops.
    if (HasBackEdge(block.terminator, i) &&
        !std::ranges::binary_search(
            bounded_latches, static_cast<uint32_t>(i))) {
      EmitBackEdgeGuard(
          context, block.terminator.origin, func, out_arg, limit_ptr);
    }

    auto term_result = LowerTerminator(
        context, block.terminator, llvm_blocks, exit_block, phi_state,
        wait_sites);
    if (!term_result) return std::unexpected(term_result.error());
  }

  // Wire PHI incoming edges and validate
  phi_state.WireIncomingEdges(context.GetBuilder());
  phi_state.ValidatePhiWiring(llvm_blocks, func->getName().str());

  // Exit block: store kOk outcome and return void
  builder.SetInsertPoint(exit_block);
  EmitStoreOkOutcome(builder, llvm_ctx, out_arg);
  builder.CreateRetVoid();

  // Clean up suspension-free alloca state before clearing pointers
  if (!proc_layout.has_suspension) {
    context.EndFunction();
  }

  // contract_scope destructor restores all execution-contract state.
  VerifyLlvmFunction(func, "GenerateProcessFunction");
  return ProcessCodegenResult{
      .function = func,
      .wait_sites = std::move(wait_sites),
      .process_trigger = ExtractProcessTriggerEntry(process)};
}

auto GenerateSharedProcessFunction(
    Context& context, const mir::Process& process, const std::string& name)
    -> Result<ProcessCodegenResult> {
  OriginScope proc_scope(context, process.origin);
  std::vector<WaitSiteEntry> wait_sites;

  // Shared module processes are simulation-only by architecture.
  ExecutionContractScope contract_scope(
      context, DesignStoreMode::kNotifySimulation);

  auto bounded_latches = BuildBoundedLatchSet(
      mir::passes::FindBoundedBackEdges(
          process.blocks, context.GetMirArena(), context.GetTypeArena()));

  // Shared module processes are always simulation-only.
  auto deferred_loops = mir::passes::FindDeferredNotificationLoops(
      process.blocks, context.GetMirArena(), context.GetTypeArena());
  auto deferred_blocks = BuildDeferredPolicyBlocks(deferred_loops);

  auto activation_plan = BuildProcessActivationPlan(
      process, context.GetMirArena(), context.GetTypeArena());

  auto& llvm_ctx = context.GetLlvmContext();
  auto& module = context.GetModule();

  // Shared body call contract: void(ptr frame, i32 resume).
  // Instance binding is loaded from the frame header at entry.
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* void_ty = llvm::Type::getVoidTy(llvm_ctx);
  auto* fn_type = llvm::FunctionType::get(void_ty, {ptr_ty, i32_ty}, false);

  auto* func = llvm::Function::Create(
      fn_type, llvm::Function::InternalLinkage, name, &module);

  func->getArg(0)->setName("frame");
  func->getArg(1)->setName("resume_block");

  auto* entry_block = llvm::BasicBlock::Create(llvm_ctx, "entry", func);
  auto* exit_block = llvm::BasicBlock::Create(llvm_ctx, "exit", func);

  std::vector<llvm::BasicBlock*> llvm_blocks;
  llvm_blocks.reserve(process.blocks.size());
  for (size_t i = 0; i < process.blocks.size(); ++i) {
    llvm_blocks.push_back(
        llvm::BasicBlock::Create(llvm_ctx, std::format("bb{}", i), func));
  }

  auto& builder = context.GetBuilder();
  builder.SetInsertPoint(entry_block);

  context.EmitProcessStateSetup(func->getArg(0));

  const auto& shared_proc_layout =
      context.GetLayout().processes[context.GetCurrentProcessIndex()];
  auto shared_alloca_result =
      MaterializeAllocaStorage(context, *func, shared_proc_layout);
  if (!shared_alloca_result) {
    return std::unexpected(shared_alloca_result.error());
  }

  // Load realized instance binding from the frame header.
  // Module slots accessed via this_ptr + spec_slot_layout (stable offsets
  // are compile-time constants; unstable offsets loaded from header).
  // Signal coordination via typed identity (local signals are body-local).
  context.SetSlotAddressingMode(SlotAddressingMode::kSpecializationLocal);
  context.EmitSharedBodyBindingSetup(func->getArg(0));

  // Create activation-local managed slot storage (shadow allocas).
  // Must be after slot addressing setup (this_ptr, instance binding).
  auto managed_storage = CreateManagedSlotStorage(activation_plan, context);

  // Resume dispatch (same logic as GenerateProcessFunction)
  std::vector<size_t> resume_targets;
  for (const auto& block : process.blocks) {
    std::visit(
        [&](const auto& term) {
          using T = std::decay_t<decltype(term)>;
          if constexpr (std::is_same_v<T, mir::Delay>) {
            resume_targets.push_back(term.resume.value);
          } else if constexpr (std::is_same_v<T, mir::Wait>) {
            resume_targets.push_back(term.resume.value);
          }
        },
        block.terminator.data);
  }
  // Deduplicate: G9 lowering may produce multiple Wait terminators with the
  // same resume target (see GenerateProcessFunction comment).
  std::ranges::sort(resume_targets);
  auto [sfirst, slast] = std::ranges::unique(resume_targets);
  resume_targets.erase(sfirst, slast);

  auto* limit_ptr =
      builder.CreateCall(context.GetLyraIterationLimitPtr(), {}, "limit_ptr");

  auto* sw = builder.CreateSwitch(
      func->getArg(1), llvm_blocks[0],
      static_cast<unsigned>(resume_targets.size()));
  for (size_t target : resume_targets) {
    sw->addCase(
        llvm::ConstantInt::get(i32_ty, static_cast<uint64_t>(target)),
        llvm_blocks[target]);
  }

  context.ClearTemps();
  auto phi_state_or_err =
      SetupBlockParamPhis(context, process.blocks, llvm_blocks);
  if (!phi_state_or_err) return std::unexpected(phi_state_or_err.error());
  auto& phi_state = *phi_state_or_err;

  ContractExecutor executor(
      context, activation_plan, std::move(managed_storage));

  for (size_t i = 0; i < process.blocks.size(); ++i) {
    const auto& block = process.blocks[i];
    auto bi = static_cast<uint32_t>(i);
    builder.SetInsertPoint(llvm_blocks[i]);

    auto policy = std::ranges::binary_search(deferred_blocks, bi)
                      ? NotificationPolicy::kDeferred
                      : NotificationPolicy::kImmediate;
    NotificationPolicyScope policy_scope(context, policy);

    for (const auto& loop : deferred_loops) {
      if (loop.exit_block == bi) {
        EmitLoopExitDeferredMarks(context, loop.notified_roots);
      }
    }

    auto& resolver = executor.GetResolver(bi);
    executor.ExecuteBlockEntry(bi);

    for (uint32_t si = 0; si < block.statements.size(); ++si) {
      executor.ExecutePreStatement(bi, si);
      auto result = LowerStatement(context, resolver, block.statements[si]);
      if (!result) return std::unexpected(result.error());
      executor.ExecutePostStatement(bi, si);
    }

    executor.ExecuteBlockExit(bi);

    if (HasBackEdge(block.terminator, i) &&
        !std::ranges::binary_search(
            bounded_latches, static_cast<uint32_t>(i))) {
      auto* outcome_ptr = context.EmitOutcomePtr(func->getArg(0));
      EmitBackEdgeGuard(
          context, block.terminator.origin, func, outcome_ptr, limit_ptr);
    }

    auto term_result = LowerTerminator(
        context, resolver, block.terminator, llvm_blocks, exit_block, phi_state,
        wait_sites);
    if (!term_result) return std::unexpected(term_result.error());
  }

  phi_state.WireIncomingEdges(context.GetBuilder());
  phi_state.ValidatePhiWiring(llvm_blocks, func->getName().str());

  // Exit block: store kOk outcome to header and return void
  builder.SetInsertPoint(exit_block);
  auto* exit_outcome_ptr = context.EmitOutcomePtr(func->getArg(0));
  EmitStoreOkOutcome(builder, llvm_ctx, exit_outcome_ptr);
  builder.CreateRetVoid();

  if (!shared_proc_layout.has_suspension) {
    context.EndFunction();
  }

  // contract_scope destructor restores all execution-contract state
  // including shared-body fields (slot_addressing, this_ptr, etc.).
  VerifyLlvmFunction(func, "GenerateSharedProcessFunction");
  return ProcessCodegenResult{
      .function = func,
      .wait_sites = std::move(wait_sites),
      .process_trigger = ExtractProcessTriggerEntry(process)};
}

auto DeclareMirFunction(
    Context& context, mir::FunctionId func_id, const std::string& name)
    -> Result<llvm::Function*> {
  auto& module = context.GetModule();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& arena = context.GetMirArena();
  const auto& func = arena[func_id];

  llvm::FunctionType* fn_type = nullptr;

  // Determine LLVM function type based on function kind.
  // Observer programs unconditionally use the observer ABI with
  // ObserverContext*, regardless of module-scoped vs design-global.
  // Regular functions use signature-derived types.
  bool is_module_scoped = context.IsModuleScopedFunction(func_id);
  bool is_observer = mir::IsObserverProgram(func.runtime_kind);

  if (is_observer) {
    auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
    auto* void_ty = llvm::Type::getVoidTy(llvm_ctx);
    if (func.runtime_kind == mir::RuntimeProgramKind::kMonitorCheck) {
      // Monitor check: (design*, engine*, observer_ctx*, prev_buf*)
      fn_type = llvm::FunctionType::get(
          void_ty, {ptr_ty, ptr_ty, ptr_ty, ptr_ty}, false);
    } else {
      // Strobe / monitor setup: (design*, engine*, observer_ctx*)
      fn_type =
          llvm::FunctionType::get(void_ty, {ptr_ty, ptr_ty, ptr_ty}, false);
    }
  } else {
    auto fn_type_or_err =
        context.BuildUserFunctionType(func.signature, is_module_scoped);
    if (!fn_type_or_err) return std::unexpected(fn_type_or_err.error());
    fn_type = *fn_type_or_err;
  }

  // Create function declaration
  auto* llvm_func = llvm::Function::Create(
      fn_type, llvm::Function::InternalLinkage, name, &module);

  // Note: We use out-param calling convention for managed returns, but do NOT
  // add LLVM's sret attribute. The sret attribute is for aggregate types, not
  // pointer handles. Our "sret" is just a regular pointer parameter.

  // Register in context for call resolution
  context.RegisterUserFunction(func_id, llvm_func);

  return llvm_func;
}

namespace {

// Common entry setup for all observer programs (strobe, monitor-setup,
// monitor-check). Sets design/engine pointers, names the observer_ctx arg,
// and enters specialization-local mode when the function is module-scoped.
// Returns the observer_ctx argument value.
//
// arg_base: LLVM arg index where (design, engine, observer_ctx) starts.
//   0 for monitor-check (no sret), arg_offset for strobe/setup (may have sret).
auto SetupObserverProgramEntry(
    Context& context, mir::FunctionId func_id, llvm::Function* llvm_func,
    unsigned arg_base) -> llvm::Value* {
  auto* design_arg = llvm_func->getArg(arg_base);
  design_arg->setName("design");
  context.SetDesignPointer(design_arg);

  auto* engine_arg = llvm_func->getArg(arg_base + 1);
  engine_arg->setName("engine");
  context.SetEnginePointer(engine_arg);

  auto* observer_ctx_arg = llvm_func->getArg(arg_base + 2);
  observer_ctx_arg->setName("observer_ctx");

  if (context.IsModuleScopedFunction(func_id)) {
    EnterObserverSpecializationLocalContext(context, func_id, observer_ctx_arg);
  }

  return observer_ctx_arg;
}

// Collect all unique place roots (Local or Temp) from a function.
// Storage is per-root, NOT per-PlaceId. Multiple PlaceIds with the same root
// (but different projections) share the same storage.
struct PlaceCollector {
  // Collect PlaceRoot directly, keyed by root identity.
  // No arena scanning needed - we just extract the root from any place.
  std::unordered_map<PlaceRootKey, mir::PlaceRoot, PlaceRootKeyHash> roots;

  void CollectFromPlace(mir::PlaceId place_id, const mir::Arena& arena) {
    const auto& place = arena[place_id];
    if (mir::IsProcessLocalRoot(place.root.kind)) {
      PlaceRootKey key{.kind = place.root.kind, .id = place.root.id};
      roots.try_emplace(key, place.root);
    }
  }

  void CollectFromOperand(const mir::Operand& op, const mir::Arena& arena) {
    if (op.kind == mir::Operand::Kind::kUse) {
      CollectFromPlace(std::get<mir::PlaceId>(op.payload), arena);
    }
  }

  // Collect places from an Rvalue (operands + RvalueInfo).
  // Uses exhaustive std::visit to ensure new RvalueInfo types with embedded
  // PlaceIds cause compilation errors until handled.
  void CollectFromRvalue(const mir::Rvalue& rvalue, const mir::Arena& arena) {
    for (const auto& op : rvalue.operands) {
      CollectFromOperand(op, arena);
    }

    std::visit(
        [&](const auto& info) {
          using T = std::decay_t<decltype(info)>;

          if constexpr (std::is_same_v<T, mir::GuardedUseRvalueInfo>) {
            CollectFromPlace(info.place, arena);
          } else if constexpr (std::is_same_v<T, mir::BuiltinCallRvalueInfo>) {
            if (info.receiver) {
              CollectFromPlace(*info.receiver, arena);
            }
          } else if constexpr (std::is_same_v<T, mir::TestPlusargsRvalueInfo>) {
            // Test plusargs uses info.query, not Rvalue::operands
            CollectFromOperand(info.query.operand, arena);
          } else if constexpr (std::is_same_v<T, mir::FopenRvalueInfo>) {
            // Fopen uses info.filename/mode, not Rvalue::operands
            CollectFromOperand(info.filename.operand, arena);
            if (info.mode) {
              CollectFromOperand(info.mode->operand, arena);
            }
          } else if constexpr (std::is_same_v<T, mir::SystemCmdRvalueInfo>) {
            // SystemCmd uses info.command, not Rvalue::operands
            if (info.command) {
              CollectFromOperand(info.command->operand, arena);
            }
          } else if constexpr (std::is_same_v<T, mir::SFormatRvalueInfo>) {
            for (const auto& fop : info.ops) {
              if (fop.value) {
                CollectFromOperand(*fop.value, arena);
              }
            }
          } else if constexpr (
              std::is_same_v<T, mir::UnaryRvalueInfo> ||
              std::is_same_v<T, mir::BinaryRvalueInfo> ||
              std::is_same_v<T, mir::CastRvalueInfo> ||
              std::is_same_v<T, mir::BitCastRvalueInfo> ||
              std::is_same_v<T, mir::AggregateRvalueInfo> ||
              std::is_same_v<T, mir::IsKnownRvalueInfo> ||
              std::is_same_v<T, mir::IndexInRangeRvalueInfo> ||
              std::is_same_v<T, mir::ConcatRvalueInfo> ||
              std::is_same_v<T, mir::ReplicateRvalueInfo> ||
              std::is_same_v<T, mir::RuntimeQueryRvalueInfo> ||
              std::is_same_v<T, mir::MathCallRvalueInfo> ||
              std::is_same_v<T, mir::SystemTfRvalueInfo> ||
              std::is_same_v<T, mir::ArrayQueryRvalueInfo> ||
              std::is_same_v<T, mir::SelectRvalueInfo>) {
            // These RvalueInfo types have no embedded PlaceIds or Operands
            // beyond what's in Rvalue::operands (already collected above)
          } else {
            static_assert(
                !sizeof(T),
                "Unhandled RvalueInfo type in PlaceCollector - if this type "
                "has embedded PlaceIds or Operands, add handling above");
          }
        },
        rvalue.info);
  }

  void CollectFromRhs(const mir::RightHandSide& rhs, const mir::Arena& arena) {
    std::visit(
        common::Overloaded{
            [&](const mir::Operand& op) { CollectFromOperand(op, arena); },
            [&](const mir::Rvalue& rv) { CollectFromRvalue(rv, arena); },
        },
        rhs);
  }

  // Collect places from an effect operation.
  // Each effect type has different operand/place fields - this centralizes
  // the handling to make coverage auditable.
  void CollectFromEffect(
      const mir::EffectOp& effect_op, const mir::Arena& arena) {
    std::visit(
        [&](const auto& eff) {
          using E = std::decay_t<decltype(eff)>;

          if constexpr (std::is_same_v<E, mir::DisplayEffect>) {
            for (const auto& fop : eff.ops) {
              if (fop.value.has_value()) {
                CollectFromOperand(*fop.value, arena);
              }
            }
            if (eff.descriptor.has_value()) {
              CollectFromOperand(*eff.descriptor, arena);
            }
          } else if constexpr (std::is_same_v<E, mir::SeverityEffect>) {
            for (const auto& fop : eff.ops) {
              if (fop.value.has_value()) {
                CollectFromOperand(*fop.value, arena);
              }
            }
          } else if constexpr (std::is_same_v<E, mir::MonitorEffect>) {
            for (const auto& fop : eff.format_ops) {
              if (fop.value.has_value()) {
                CollectFromOperand(*fop.value, arena);
              }
            }
          } else if constexpr (std::is_same_v<E, mir::MemIOEffect>) {
            CollectFromPlace(eff.target, arena);
            CollectFromOperand(eff.filename.operand, arena);
            if (eff.start_addr.has_value()) {
              CollectFromOperand(*eff.start_addr, arena);
            }
            if (eff.end_addr.has_value()) {
              CollectFromOperand(*eff.end_addr, arena);
            }
          } else if constexpr (std::is_same_v<E, mir::SystemTfEffect>) {
            for (const auto& op : eff.args) {
              CollectFromOperand(op, arena);
            }
          } else if constexpr (std::is_same_v<
                                   E, mir::RecordDecisionObservationDynamic>) {
            CollectFromOperand(eff.match_class, arena);
            CollectFromOperand(eff.selected_kind, arena);
            CollectFromOperand(eff.selected_arm, arena);
          }
          // StrobeEffect, TimeFormatEffect, MonitorControlEffect,
          // RecordDecisionObservation: no operands
        },
        effect_op);
  }

  void CollectFromFunction(const mir::Function& func, const mir::Arena& arena) {
    for (const auto& block : func.blocks) {
      for (const auto& inst : block.statements) {
        std::visit(
            [&](const auto& data) {
              using T = std::decay_t<decltype(data)>;
              if constexpr (std::is_same_v<T, mir::Assign>) {
                CollectFromPlace(data.dest, arena);
                CollectFromRhs(data.rhs, arena);
              } else if constexpr (std::is_same_v<T, mir::GuardedAssign>) {
                CollectFromPlace(data.dest, arena);
                CollectFromRhs(data.rhs, arena);
                CollectFromOperand(data.guard, arena);
              } else if constexpr (std::is_same_v<T, mir::Effect>) {
                CollectFromEffect(data.op, arena);
              } else if constexpr (std::is_same_v<T, mir::DeferredAssign>) {
                CollectFromPlace(data.dest, arena);
                CollectFromRhs(data.rhs, arena);
              } else if constexpr (std::is_same_v<T, mir::Call>) {
                // Scan input arguments
                for (const auto& arg : data.in_args) {
                  CollectFromOperand(arg, arena);
                }
                // Scan return tmp ONLY (not dest - dest may be design slot)
                if (data.ret) {
                  CollectFromPlace(data.ret->tmp, arena);
                  // DO NOT: CollectFromPlace(data.ret->dest, arena);
                }
                // Scan writeback tmps ONLY (not dests).
                // kDirectToDest has no tmp (nullopt).
                for (const auto& wb : data.writebacks) {
                  if (wb.tmp.has_value()) {
                    CollectFromPlace(*wb.tmp, arena);
                  }
                  // DO NOT: CollectFromPlace(wb.dest, arena);
                }
              } else if constexpr (std::is_same_v<T, mir::DpiCall>) {
                for (const auto& binding : data.args) {
                  if (binding.input_value) {
                    CollectFromOperand(*binding.input_value, arena);
                  }
                  if (binding.writeback_dest) {
                    CollectFromPlace(*binding.writeback_dest, arena);
                  }
                }
                if (data.ret) {
                  CollectFromPlace(data.ret->tmp, arena);
                }
              } else if constexpr (std::is_same_v<T, mir::BuiltinCall>) {
                if (data.dest) {
                  CollectFromPlace(*data.dest, arena);
                }
                CollectFromPlace(data.receiver, arena);
                for (const auto& arg : data.args) {
                  CollectFromOperand(arg, arena);
                }
              } else if constexpr (std::is_same_v<T, mir::DefineTemp>) {
                // DefineTemp doesn't create place storage; only visit RHS
                // operands
                CollectFromRhs(data.rhs, arena);
              }
            },
            inst.data);
      }
      // Collect from terminator operands
      std::visit(
          [&](const auto& term) {
            using T = std::decay_t<decltype(term)>;
            if constexpr (std::is_same_v<T, mir::Jump>) {
              // Jump::args are edge arguments for block params
              for (const auto& arg : term.args) {
                CollectFromOperand(arg, arena);
              }
            } else if constexpr (std::is_same_v<T, mir::Branch>) {
              // Branch::condition is Operand
              CollectFromOperand(term.condition, arena);
              // Branch edge args for block params
              for (const auto& arg : term.then_args) {
                CollectFromOperand(arg, arena);
              }
              for (const auto& arg : term.else_args) {
                CollectFromOperand(arg, arena);
              }
            } else if constexpr (std::is_same_v<T, mir::Return>) {
              // Return::value is optional<Operand>
              if (term.value.has_value()) {
                CollectFromOperand(*term.value, arena);
              }
            } else if constexpr (std::is_same_v<T, mir::Switch>) {
              // Switch::selector is Operand
              CollectFromOperand(term.selector, arena);
            }
          },
          block.terminator.data);
    }
  }
};

}  // namespace

// Special lowering for monitor check observer programs with comparison logic.
// Evaluates expressions, compares against prev_buffer, and only prints if
// values changed.
auto DefineMonitorCheckProgram(
    Context& context, mir::FunctionId func_id, llvm::Function* llvm_func,
    const Context::MonitorLayout& layout) -> Result<void> {
  // Observer programs are simulation-only (engine passed as explicit param).
  ExecutionContractScope contract_scope(
      context, DesignStoreMode::kNotifySimulation);

  auto& llvm_ctx = context.GetLlvmContext();
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& func = arena[func_id];

  // Create blocks: entry -> eval -> [print -> update] -> exit
  auto* entry_block = llvm::BasicBlock::Create(llvm_ctx, "entry", llvm_func);
  auto* print_block = llvm::BasicBlock::Create(llvm_ctx, "print", llvm_func);
  auto* exit_block = llvm::BasicBlock::Create(llvm_ctx, "exit", llvm_func);

  // Create MIR basic blocks for body lowering
  std::vector<llvm::BasicBlock*> llvm_blocks;
  llvm_blocks.reserve(func.blocks.size());
  for (size_t i = 0; i < func.blocks.size(); ++i) {
    auto* bb =
        llvm::BasicBlock::Create(llvm_ctx, std::format("bb{}", i), llvm_func);
    llvm_blocks.push_back(bb);
  }

  builder.SetInsertPoint(entry_block);
  context.BeginFunction(*llvm_func);

  // Observer program entry: design, engine, observer_ctx, prev_buf.
  // SetupObserverProgramEntry handles the common (design, engine, observer_ctx)
  // triple and conditionally enters specialization-local mode.
  SetupObserverProgramEntry(context, func_id, llvm_func, 0);

  auto* prev_buf_arg = llvm_func->getArg(3);
  prev_buf_arg->setName("prev_buf");

  // Collect local/temp places for storage allocation
  PlaceCollector collector;
  collector.CollectFromFunction(func, arena);

  // Allocate and initialize all collected places at function entry.
  // INVARIANT: Observer programs must follow the same allocate-all + init-all
  // policy as regular functions; no lazy storage creation is allowed.
  // ComputePlacePointer
  // will throw InternalError if storage is missing.
  for (const auto& [key, root] : collector.roots) {
    auto alloca_or_err = context.GetOrCreatePlaceStorage(root);
    if (!alloca_or_err) return std::unexpected(alloca_or_err.error());
    context.InitializePlaceStorage(*alloca_or_err, root.type);
  }

  // Find the DisplayEffect from check program's MIR body (correct operand
  // context). layout only provides offsets/byte_sizes; format_ops come from the
  // program itself.
  const mir::DisplayEffect* display_effect = nullptr;
  for (const auto& block : func.blocks) {
    for (const auto& inst : block.statements) {
      if (const auto* eff = std::get_if<mir::Effect>(&inst.data)) {
        if (const auto* disp = std::get_if<mir::DisplayEffect>(&eff->op)) {
          display_effect = disp;
          break;
        }
      }
    }
    if (display_effect != nullptr) break;
  }
  if (display_effect == nullptr) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        func.origin, "monitor check program missing DisplayEffect",
        UnsupportedCategory::kFeature));
  }

  // Allocate temp buffer for current values (for comparison)
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  llvm::Value* temp_buf = nullptr;
  if (layout.total_size > 0) {
    auto* buf_ty = llvm::ArrayType::get(i8_ty, layout.total_size);
    auto* alloca_inst = builder.CreateAlloca(buf_ty, nullptr, "temp_buf");
    // Align to 8 bytes to match layout computation (offsets are 8-byte aligned)
    alloca_inst->setAlignment(llvm::Align(8));
    temp_buf = alloca_inst;

    // Zero-initialize to avoid nondeterministic comparison from padding bytes
    auto* memset_fn = llvm::Intrinsic::getDeclaration(
        &context.GetModule(), llvm::Intrinsic::memset,
        {ptr_ty, llvm::Type::getInt32Ty(llvm_ctx)});
    builder.CreateCall(
        memset_fn,
        {temp_buf, llvm::ConstantInt::get(i8_ty, 0),
         llvm::ConstantInt::get(
             llvm::Type::getInt32Ty(llvm_ctx), layout.total_size),
         llvm::ConstantInt::get(llvm::Type::getInt1Ty(llvm_ctx), 0)});
  }

  // Clear stale temp bindings and setup PHI nodes for block params
  context.ClearTemps();
  auto phi_state_or_err =
      SetupBlockParamPhis(context, func.blocks, llvm_blocks);
  if (!phi_state_or_err) return std::unexpected(phi_state_or_err.error());
  auto& phi_state = *phi_state_or_err;

  // Jump to first MIR block to evaluate expressions
  builder.SetInsertPoint(entry_block);
  builder.CreateBr(llvm_blocks[func.entry.value]);

  // Lower MIR blocks, skip DisplayEffect (we'll handle it at
  // Return/print_block).
  for (size_t i = 0; i < func.blocks.size(); ++i) {
    const auto& block = func.blocks[i];
    builder.SetInsertPoint(llvm_blocks[i]);

    // Lower all instructions except DisplayEffect
    for (const auto& instruction : block.statements) {
      if (const auto* effect = std::get_if<mir::Effect>(&instruction.data)) {
        if (std::holds_alternative<mir::DisplayEffect>(effect->op)) {
          // Skip DisplayEffect - we'll handle it in print_block
          continue;
        }
      }
      auto result = LowerStatement(context, instruction);
      if (!result) return std::unexpected(result.error());
    }

    // Lower terminator - redirect to comparison instead of exit
    OriginScope term_scope(context, block.terminator.origin);
    auto term_result = std::visit(
        common::Overloaded{
            [&](const mir::Jump& t) -> Result<void> {
              return LowerJump(context, t, llvm_blocks, phi_state);
            },
            [&](const mir::Branch& t) -> Result<void> {
              return LowerBranch(context, t, llvm_blocks, phi_state);
            },
            [&](const mir::Return&) -> Result<void> {
              // At Return: evaluate operands from check program's DisplayEffect
              // (correct MIR operand context), store to temp buffer, compare.
              if (temp_buf != nullptr) {
                auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

                // Evaluate each operand from the check program's DisplayEffect
                for (size_t j = 0; j < display_effect->ops.size(); ++j) {
                  const auto& op = display_effect->ops[j];
                  if (!op.value.has_value()) continue;
                  if (j >= layout.byte_sizes.size()) break;

                  uint32_t byte_size = layout.byte_sizes[j];
                  if (byte_size == 0) continue;  // Skip literals
                  uint32_t offset = layout.offsets[j];

                  // Evaluate the operand
                  auto val_result = LowerOperand(context, *op.value);
                  if (!val_result) {
                    return std::unexpected(val_result.error());
                  }
                  llvm::Value* val = *val_result;

                  auto* slot_ptr = builder.CreateGEP(
                      i8_ty, temp_buf, {llvm::ConstantInt::get(i64_ty, offset)},
                      "temp_slot");

                  // Use exact-width integer type (i8, i16, i24, i32, i40, etc.)
                  // to avoid clobbering adjacent slots for non-power-of-2
                  // sizes.
                  uint32_t bit_width = byte_size * 8;
                  llvm::Type* store_ty =
                      llvm::Type::getIntNTy(llvm_ctx, bit_width);

                  llvm::Value* store_val = val;
                  if (val->getType()->isIntegerTy() &&
                      store_ty->isIntegerTy()) {
                    unsigned cur_bits = val->getType()->getIntegerBitWidth();
                    unsigned store_bits = store_ty->getIntegerBitWidth();
                    if (cur_bits < store_bits) {
                      store_val = builder.CreateZExt(val, store_ty);
                    } else if (cur_bits > store_bits) {
                      store_val = builder.CreateTrunc(val, store_ty);
                    }
                  } else if (val->getType()->isFloatingPointTy()) {
                    if (val->getType()->isDoubleTy()) {
                      store_val = builder.CreateBitCast(
                          val, llvm::Type::getInt64Ty(llvm_ctx));
                      store_ty = llvm::Type::getInt64Ty(llvm_ctx);
                    } else {
                      store_val = builder.CreateBitCast(
                          val, llvm::Type::getInt32Ty(llvm_ctx));
                      store_ty = llvm::Type::getInt32Ty(llvm_ctx);
                    }
                  }
                  // Use explicit alignment=1 (safe for any byte size, perf not
                  // critical)
                  builder.CreateAlignedStore(
                      store_val, slot_ptr, llvm::Align(1));
                }

                // Compare temp_buf with prev_buf
                auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
                llvm::Value* any_changed = llvm::ConstantInt::get(i1_ty, 0);

                for (size_t j = 0; j < layout.byte_sizes.size(); ++j) {
                  uint32_t byte_size = layout.byte_sizes[j];
                  if (byte_size == 0) continue;  // Skip literals

                  uint32_t offset = layout.offsets[j];

                  // Use exact-width integer type for comparison (matches
                  // storage).
                  uint32_t cmp_bits = byte_size * 8;
                  llvm::Type* cmp_ty =
                      llvm::Type::getIntNTy(llvm_ctx, cmp_bits);

                  auto* temp_ptr = builder.CreateGEP(
                      i8_ty, temp_buf, {llvm::ConstantInt::get(i64_ty, offset)},
                      "t_ptr");
                  auto* prev_ptr = builder.CreateGEP(
                      i8_ty, prev_buf_arg,
                      {llvm::ConstantInt::get(i64_ty, offset)}, "p_ptr");

                  // Use explicit alignment=1 (safe for any byte size)
                  llvm::Value* temp_val = builder.CreateAlignedLoad(
                      cmp_ty, temp_ptr, llvm::Align(1), "t_val");
                  llvm::Value* prev_val = builder.CreateAlignedLoad(
                      cmp_ty, prev_ptr, llvm::Align(1), "p_val");

                  llvm::Value* cmp =
                      builder.CreateICmpNE(temp_val, prev_val, "cmp");
                  any_changed = builder.CreateOr(any_changed, cmp, "changed");
                }

                builder.CreateCondBr(any_changed, print_block, exit_block);
              } else {
                // No values to compare - never print after initial
                builder.CreateBr(exit_block);
              }
              return {};
            },
            [&](const mir::Finish&) -> Result<void> {
              builder.CreateBr(exit_block);
              return {};
            },
            [&](const auto&) -> Result<void> {
              return std::unexpected(
                  context.GetDiagnosticContext().MakeUnsupported(
                      context.GetCurrentOrigin(),
                      "terminator not yet supported in monitor check program",
                      UnsupportedCategory::kFeature));
            },
        },
        block.terminator.data);
    if (!term_result) return std::unexpected(term_result.error());
  }

  // Wire PHI incoming edges and validate
  phi_state.WireIncomingEdges(context.GetBuilder());
  phi_state.ValidatePhiWiring(llvm_blocks, llvm_func->getName().str());

  // Print block: lower DisplayEffect and update prev_buffer
  builder.SetInsertPoint(print_block);

  // Find and lower the DisplayEffect
  for (const auto& block : func.blocks) {
    for (const auto& instruction : block.statements) {
      if (const auto* effect = std::get_if<mir::Effect>(&instruction.data)) {
        if (const auto* display =
                std::get_if<mir::DisplayEffect>(&effect->op)) {
          auto result = LowerDisplayEffect(context, *display);
          if (!result) return std::unexpected(result.error());
          break;
        }
      }
    }
  }

  // Update prev_buffer with temp_buf values
  if (temp_buf != nullptr && layout.total_size > 0) {
    auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
    auto* memcpy_fn = llvm::Intrinsic::getDeclaration(
        &context.GetModule(), llvm::Intrinsic::memcpy,
        {ptr_ty, ptr_ty, llvm::Type::getInt32Ty(llvm_ctx)});
    auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
    builder.CreateCall(
        memcpy_fn, {prev_buf_arg, temp_buf,
                    llvm::ConstantInt::get(
                        llvm::Type::getInt32Ty(llvm_ctx), layout.total_size),
                    llvm::ConstantInt::get(i1_ty, 0)});
  }
  builder.CreateBr(exit_block);

  // Exit block
  builder.SetInsertPoint(exit_block);
  builder.CreateRetVoid();

  VerifyLlvmFunction(llvm_func, "DefineMonitorCheckProgram");

  context.EndFunction();
  // contract_scope destructor restores execution-contract fields.
  return {};
}

// Emit setup program epilogue: serialize current values + call
// LyraMonitorRegister. Called at end of setup program's exit block, before
// return.
auto EmitMonitorSetupEpilogue(
    Context& context, mir::FunctionId setup_program_id,
    const Context::MonitorSetupInfo& info, llvm::Value* design_ptr,
    llvm::Value* engine_ptr) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& arena = context.GetMirArena();

  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  // Find the DisplayEffect from setup program's MIR body (correct operand
  // context).
  const mir::Function& setup_func = arena[setup_program_id];
  const mir::DisplayEffect* display_effect = nullptr;
  for (const auto& block : setup_func.blocks) {
    for (const auto& inst : block.statements) {
      if (const auto* eff = std::get_if<mir::Effect>(&inst.data)) {
        if (const auto* disp = std::get_if<mir::DisplayEffect>(&eff->op)) {
          display_effect = disp;
          break;
        }
      }
    }
    if (display_effect != nullptr) break;
  }
  if (display_effect == nullptr) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "$monitor setup program missing DisplayEffect",
        UnsupportedCategory::kFeature));
  }

  // Get check program function pointer for registration
  llvm::Function* check_fn = context.GetUserFunction(info.check_program);
  if (check_fn == nullptr) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "$monitor check program not found for setup epilogue",
        UnsupportedCategory::kFeature));
  }

  // Look up layout via check_program (single source of truth)
  const auto* layout = context.GetMonitorLayout(info.check_program);
  if (layout == nullptr) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "$monitor layout not found for setup epilogue",
        UnsupportedCategory::kFeature));
  }

  llvm::Value* init_buf = nullptr;
  if (layout->total_size > 0) {
    // Allocate buffer on stack with 8-byte alignment (matches layout
    // computation)
    auto* buf_ty = llvm::ArrayType::get(i8_ty, layout->total_size);
    auto* alloca_inst = builder.CreateAlloca(buf_ty, nullptr, "init_buf");
    alloca_inst->setAlignment(llvm::Align(8));
    init_buf = alloca_inst;

    // Zero-initialize the buffer
    auto* memset_fn = llvm::Intrinsic::getDeclaration(
        &context.GetModule(), llvm::Intrinsic::memset,
        {ptr_ty, llvm::Type::getInt32Ty(llvm_ctx)});
    builder.CreateCall(
        memset_fn,
        {init_buf, llvm::ConstantInt::get(i8_ty, 0),
         llvm::ConstantInt::get(i32_ty, layout->total_size),
         llvm::ConstantInt::get(llvm::Type::getInt1Ty(llvm_ctx), 0)});

    // Serialize each value operand to the buffer.
    // Use display_effect->ops (correct MIR operand context for setup program).
    // The offsets/byte_sizes arrays are indexed by ops position.
    for (size_t i = 0; i < display_effect->ops.size(); ++i) {
      const auto& op = display_effect->ops[i];
      if (!op.value.has_value()) continue;
      if (i >= layout->byte_sizes.size()) break;

      uint32_t byte_size = layout->byte_sizes[i];
      if (byte_size == 0) continue;

      uint32_t offset = layout->offsets[i];

      // Lower the operand to get current value
      auto val_result = LowerOperandRaw(context, *op.value);
      if (!val_result) return std::unexpected(val_result.error());
      llvm::Value* value = *val_result;

      // Get pointer to buffer location
      auto* buf_ptr = builder.CreateGEP(
          i8_ty, init_buf, llvm::ConstantInt::get(i32_ty, offset));

      // Use exact-width integer type (i8, i16, i24, i32, i40, etc.)
      // to avoid clobbering adjacent slots for non-power-of-2 sizes.
      // Must match check program's serialization.
      uint32_t bit_width = byte_size * 8;
      llvm::Type* store_ty = llvm::Type::getIntNTy(llvm_ctx, bit_width);

      llvm::Type* val_type = value->getType();
      llvm::Value* store_val = value;
      if (val_type->isIntegerTy() && store_ty->isIntegerTy()) {
        unsigned cur_bits = val_type->getIntegerBitWidth();
        unsigned store_bits = store_ty->getIntegerBitWidth();
        if (cur_bits < store_bits) {
          store_val = builder.CreateZExt(value, store_ty);
        } else if (cur_bits > store_bits) {
          store_val = builder.CreateTrunc(value, store_ty);
        }
      } else if (val_type->isDoubleTy()) {
        store_val =
            builder.CreateBitCast(value, llvm::Type::getInt64Ty(llvm_ctx));
      } else if (val_type->isFloatTy()) {
        store_val = builder.CreateBitCast(value, i32_ty);
      }
      // Use explicit alignment=1 (safe for any byte size, perf not critical)
      builder.CreateAlignedStore(store_val, buf_ptr, llvm::Align(1));
      // Other types (4-state, etc.) would need special handling
    }
  } else {
    // No values to track - create null buffer
    init_buf = llvm::ConstantPointerNull::get(ptr_ty);
  }

  // Pass ObserverContext fields to LyraMonitorRegister for capture.
  // If the setup program is module-scoped, these are loaded from the incoming
  // ObserverContext. If design-global, they are null/zero.
  auto obs_fields = GetObserverContextFieldValues(context);

  // Call LyraMonitorRegister(engine, check_fn, design,
  //                          this_ptr, instance_id, init_buf, size)
  builder.CreateCall(
      context.GetLyraMonitorRegister(),
      {engine_ptr, check_fn, design_ptr, obs_fields.this_ptr,
       obs_fields.instance_id, init_buf,
       llvm::ConstantInt::get(i32_ty, layout->total_size)});

  return {};
}

auto DefineMirFunction(
    Context& context, mir::FunctionId func_id, llvm::Function* llvm_func)
    -> Result<void> {
  const auto& arena = context.GetMirArena();
  const auto& func = arena[func_id];

  // Monitor check observer programs have special lowering with comparison
  // logic. Use runtime_kind from MIR (source of truth), layout from side table
  // (codegen artifact).
  if (func.runtime_kind == mir::RuntimeProgramKind::kMonitorCheck) {
    const auto* layout = context.GetMonitorLayout(func_id);
    if (layout == nullptr) {
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          func.origin, "monitor check observer program missing layout",
          UnsupportedCategory::kFeature));
    }
    return DefineMonitorCheckProgram(context, func_id, llvm_func, *layout);
  }

  auto& llvm_ctx = context.GetLlvmContext();
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  // Select execution contract based on callable category:
  // - Observer programs (strobe/setup): simulation-only, engine always non-null
  // - User-defined functions/tasks: cross-context, engine may be null
  bool is_observer_program = mir::IsObserverProgram(func.runtime_kind);
  auto store_mode = is_observer_program ? DesignStoreMode::kNotifySimulation
                                        : DesignStoreMode::kNotifyCrossContext;
  ExecutionContractScope contract_scope(context, store_mode);

  // Function-level origin scope for prologue errors.
  // If func.origin is Invalid, this is a no-op.
  OriginScope func_scope(context, func.origin);

  // Use MIR return policy (frozen at HIR->MIR lowering)
  bool uses_sret =
      func.signature.return_policy == mir::ReturnPolicy::kSretOutParam;

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

  // Argument offset: sret pointer comes first if present
  unsigned arg_offset = uses_sret ? 1 : 0;

  // Extract sret pointer if present (for use in exit block)
  llvm::Value* sret_ptr = nullptr;
  if (uses_sret) {
    sret_ptr = llvm_func->getArg(0);
    sret_ptr->setName("sret");
  }

  // DesignState* and Engine* are always at arg_offset, arg_offset+1.
  auto* design_arg = llvm_func->getArg(arg_offset);
  auto* engine_arg = llvm_func->getArg(arg_offset + 1);

  // Two orthogonal axes determine function entry:
  //   1. ABI: observer programs always receive ObserverContext* (from
  //   runtime_kind)
  //   2. Entry: module-scoped functions enter specialization-local mode
  // These are independent: a design-global observer receives ObserverContext*
  // but does not enter specialization-local mode (context fields are
  // zero/null).
  bool is_module_scoped = context.IsModuleScopedFunction(func_id);
  bool is_observer = mir::IsObserverProgram(func.runtime_kind);
  unsigned context_arg_count = 0;
  if (is_observer) {
    // SetupObserverProgramEntry handles design/engine/observer_ctx setup
    // and conditionally enters specialization-local mode.
    SetupObserverProgramEntry(context, func_id, llvm_func, arg_offset);
    context_arg_count = 0;
  } else {
    // Non-observer: set design/engine on context (observer path does this
    // inside SetupObserverProgramEntry).
    design_arg->setName("design");
    context.SetDesignPointer(design_arg);
    engine_arg->setName("engine");
    context.SetEnginePointer(engine_arg);
  }
  if (!is_observer && is_module_scoped) {
    const auto& lowering = context.GetModuleFunctionLowering(func_id);
    context.SetSpecSlotInfo(lowering.spec_slot_info);

    auto* this_arg = llvm_func->getArg(arg_offset + 2);
    this_arg->setName("this_ptr");
    context.SetThisPointer(this_arg);

    auto* instance_arg = llvm_func->getArg(arg_offset + 3);
    instance_arg->setName("instance_ptr");
    context.SetInstancePointer(instance_arg);

    auto* instance_id_arg = llvm_func->getArg(arg_offset + 4);
    instance_id_arg->setName("instance_id");
    context.SetDynamicInstanceId(instance_id_arg);

    context.SetSlotAddressingMode(SlotAddressingMode::kSpecializationLocal);
    context_arg_count = 3;
  }

  // Collect all local/temp places referenced in the function
  PlaceCollector collector;
  collector.CollectFromFunction(func, arena);

  // Variable to hold return value (for non-void, non-sret functions)
  // For sret: we write directly to sret_ptr at each Return terminator,
  // eliminating the staging slot and aggregate SSA materialization.
  llvm::Value* return_value_ptr = nullptr;
  const Type& ret_type = types[func.signature.return_type];
  if (ret_type.Kind() != TypeKind::kVoid && !uses_sret) {
    return_value_ptr =
        builder.CreateAlloca(llvm_func->getReturnType(), nullptr, "retval");

    // Default-initialize the return slot (part of the storage invariant).
    EmitSVDefaultInit(context, return_value_ptr, func.signature.return_type);
  }

  // PROLOGUE: Allocate and default-initialize ALL locals/temps at function
  // entry. This matches SystemVerilog default initialization semantics and
  // ensures deterministic behavior. Order: 1) allocate all, 2) initialize all,
  // 3) store parameters
  //
  // CONTRACT: All function-local storage is default-initialized at function
  // entry, including: kLocal, kTemp, and return slots.
  // - Managed handles become nullptr (Destroy is a no-op on empty).
  // - 4-state values become X per SV default init rules.
  //
  // Therefore: commit/assign paths may safely destroy old values even on first
  // write, because Destroy(nullptr) is a no-op for managed types.
  std::vector<std::pair<llvm::AllocaInst*, TypeId>> all_allocas;
  for (const auto& [key, root] : collector.roots) {
    auto alloca_or_err = context.GetOrCreatePlaceStorage(root);
    if (!alloca_or_err) return std::unexpected(alloca_or_err.error());
    all_allocas.emplace_back(*alloca_or_err, root.type);
  }

  // Initialize all locals/temps with default values per SystemVerilog semantics
  for (const auto& [alloca, type_id] : all_allocas) {
    context.InitializePlaceStorage(alloca, type_id);
  }

  // Store argument values into parameter locals (overwriting default init).
  // Use explicit param_local_slots mapping - do NOT assume param i = local i.
  // Arguments start after: [sret], design, engine, [this, sig_offset, inst_id].
  unsigned user_arg_base = arg_offset + 2 + context_arg_count;
  //
  // For output/inout params, the argument is a pointer to caller's storage.
  // We track these pointers for writing back at function exit.
  std::vector<std::tuple<llvm::Value*, llvm::AllocaInst*, TypeId>>
      output_param_ptrs;  // (caller_ptr, local_alloca, type)

  for (size_t i = 0; i < func.signature.params.size(); ++i) {
    common::OriginId param_origin = (i < func.param_origins.size())
                                        ? func.param_origins[i]
                                        : common::OriginId::Invalid();
    OriginScope param_scope(context, param_origin);

    const auto& param = func.signature.params[i];
    uint32_t local_slot = func.param_local_slots[i];
    PlaceRootKey key{
        .kind = mir::PlaceRoot::Kind::kLocal,
        .id = static_cast<int>(local_slot)};
    auto it = collector.roots.find(key);
    if (it != collector.roots.end()) {
      auto alloca_or_err = context.GetOrCreatePlaceStorage(it->second);
      if (!alloca_or_err) return std::unexpected(alloca_or_err.error());
      llvm::AllocaInst* alloca = *alloca_or_err;

      llvm::Value* arg_val =
          llvm_func->getArg(static_cast<unsigned>(i) + user_arg_base);
      arg_val->setName(std::format("arg{}", i));

      switch (param.kind) {
        case mir::PassingKind::kValue:
          // Input parameter: store value into local
          builder.CreateStore(arg_val, alloca);
          break;

        case mir::PassingKind::kInOut: {
          // Inout parameter: arg_val is pointer to caller's storage.
          // For managed types (queue/dynarray/string): use alias semantics.
          // Operations on the local directly modify caller's storage.
          // For other types: use copy-in/copy-out semantics.
          const Type& param_type = types[param.type];
          bool is_managed = param_type.Kind() == TypeKind::kQueue ||
                            param_type.Kind() == TypeKind::kDynamicArray ||
                            param_type.Kind() == TypeKind::kString;

          if (is_managed) {
            // Alias the local to caller's storage - no copy, no writeback.
            // Store the aliased pointer so GetPlacePointer returns arg_val.
            context.SetPlaceAlias(it->second, arg_val);
          } else {
            // Copy-in at entry, writeback at exit
            auto local_type = GetLlvmTypeForType(context, param.type);
            if (!local_type) return std::unexpected(local_type.error());
            llvm::Value* initial_val =
                builder.CreateLoad(*local_type, arg_val, "inout_init");
            builder.CreateStore(initial_val, alloca);
            // Track for writeback at exit
            output_param_ptrs.emplace_back(arg_val, alloca, param.type);
          }
          break;
        }

        case mir::PassingKind::kOut: {
          // Output parameter: arg_val is pointer to caller's storage.
          // For managed types: alias the local to caller's storage.
          // For aggregates: use staging + writeback.
          const Type& param_type = types[param.type];
          bool is_managed = param_type.Kind() == TypeKind::kQueue ||
                            param_type.Kind() == TypeKind::kDynamicArray ||
                            param_type.Kind() == TypeKind::kString;

          if (is_managed) {
            // Alias the local to caller's storage - writes go directly there.
            // Caller's slot may contain old handle; callee must Destroy before
            // overwriting. Since we alias, CommitValue's Destroy will handle
            // it.
            context.SetPlaceAlias(it->second, arg_val);
          } else {
            // Non-managed: local is default-initialized, writeback at exit
            output_param_ptrs.emplace_back(arg_val, alloca, param.type);
          }
          break;
        }
      }
    }
  }

  // Clear stale temp bindings and setup PHI nodes for block params
  context.ClearTemps();
  auto phi_state_or_err =
      SetupBlockParamPhis(context, func.blocks, llvm_blocks);
  if (!phi_state_or_err) return std::unexpected(phi_state_or_err.error());
  auto& phi_state = *phi_state_or_err;

  // Jump to first MIR block
  builder.SetInsertPoint(entry_block);
  builder.CreateBr(llvm_blocks[func.entry.value]);

  // Lower each MIR block
  for (size_t i = 0; i < func.blocks.size(); ++i) {
    const auto& block = func.blocks[i];
    builder.SetInsertPoint(llvm_blocks[i]);

    // Lower all instructions
    for (const auto& instruction : block.statements) {
      auto result = LowerStatement(context, instruction);
      if (!result) return std::unexpected(result.error());
    }

    // Lower terminator.
    // OriginScope preserves func.origin if terminator.origin is Invalid.
    OriginScope term_scope(context, block.terminator.origin);
    auto term_result = std::visit(
        common::Overloaded{
            [&](const mir::Jump& t) -> Result<void> {
              return LowerJump(context, t, llvm_blocks, phi_state);
            },
            [&](const mir::Branch& t) -> Result<void> {
              return LowerBranch(context, t, llvm_blocks, phi_state);
            },
            [&](const mir::Return& t) -> Result<void> {
              // Handle return value based on return policy
              if (t.value.has_value()) {
                if (uses_sret) {
                  // sret: use Place-based lowering (no aggregate SSA)
                  // CONTRACT: return operand must be a Place for sret types
                  const auto* place_id =
                      std::get_if<mir::PlaceId>(&t.value->payload);
                  if (place_id == nullptr) {
                    throw common::InternalError(
                        "DefineMirFunction",
                        "sret return operand must be a Place, not a constant");
                  }
                  auto src_ptr_result = context.GetPlacePointer(*place_id);
                  if (!src_ptr_result) {
                    return std::unexpected(src_ptr_result.error());
                  }
                  // MoveInit directly from source Place to sret out-param
                  MoveInit(
                      context, sret_ptr, *src_ptr_result,
                      func.signature.return_type);
                } else if (return_value_ptr != nullptr) {
                  // Direct return: load value and store to return slot
                  auto val_result = LowerOperandRaw(context, *t.value);
                  if (!val_result) return std::unexpected(val_result.error());
                  builder.CreateStore(*val_result, return_value_ptr);
                }
              }
              builder.CreateBr(exit_block);
              return {};
            },
            [&](const mir::Finish&) -> Result<void> {
              // $finish in a function - just return (shouldn't happen normally)
              builder.CreateBr(exit_block);
              return {};
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

  // Wire PHI incoming edges and validate
  phi_state.WireIncomingEdges(context.GetBuilder());
  phi_state.ValidatePhiWiring(llvm_blocks, llvm_func->getName().str());

  // Exit block: return the value
  builder.SetInsertPoint(exit_block);

  // Setup programs need serialization + registration before returning.
  // Use runtime_kind from MIR (source of truth), setup_info from side table
  // (codegen artifact).
  if (func.runtime_kind == mir::RuntimeProgramKind::kMonitorSetup) {
    const auto* setup_info = context.GetMonitorSetupInfo(func_id);
    if (setup_info == nullptr) {
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          func.origin, "monitor setup program missing info",
          UnsupportedCategory::kFeature));
    }
    auto result = EmitMonitorSetupEpilogue(
        context, func_id, *setup_info, design_arg, engine_arg);
    if (!result) return std::unexpected(result.error());
  }

  // Write back output/inout parameters to caller's storage
  // For managed types: use MoveAssign (destroy old in caller, move new)
  // For aggregates: use CopyAssign (field-by-field copy)
  for (const auto& [caller_ptr, local_alloca, type_id] : output_param_ptrs) {
    // Load from local and assign to caller's storage
    // MoveAssign handles destroying old value in caller's storage
    MoveAssign(context, caller_ptr, local_alloca, type_id);
  }

  if (ret_type.Kind() == TypeKind::kVoid || uses_sret) {
    // Void functions and sret functions both return void at the LLVM level.
    // For sret, the MoveInit to the out-param happens at each Return terminator
    // (Place-based lowering), so no work needed here.
    builder.CreateRetVoid();
  } else {
    llvm::Value* ret_val =
        builder.CreateLoad(llvm_func->getReturnType(), return_value_ptr, "ret");
    builder.CreateRet(ret_val);
  }

  VerifyLlvmFunction(llvm_func, "DefineMirFunction");

  // Clean up function scope. Execution-contract fields are restored by
  // contract_scope destructor; only module-scoped fields need manual cleanup.
  context.EndFunction();
  // contract_scope destructor restores all execution-contract state.
  return {};
}

auto ExtractProcessTriggerEntry(const mir::Process& process)
    -> std::optional<ProcessTriggerEntry> {
  ProcessTriggerEntry entry;
  bool found_wait = false;
  bool has_dynamic = false;
  bool has_rebindable = false;

  for (const auto& block : process.blocks) {
    const auto* wait = std::get_if<mir::Wait>(&block.terminator.data);
    if (wait == nullptr) continue;

    found_wait = true;
    for (const auto& t : wait->triggers) {
      if (t.late_bound) {
        if (t.late_bound->is_container) {
          has_dynamic = true;
        } else {
          has_rebindable = true;
        }
      }
      entry.triggers.push_back({
          .signal = t.signal,
          .edge = t.edge,
          .has_observed_place = t.observed_place.has_value(),
      });
    }
  }

  if (!found_wait) return std::nullopt;

  if (has_dynamic) {
    entry.shape = runtime::WaitShapeKind::kDynamic;
  } else if (has_rebindable) {
    entry.shape = runtime::WaitShapeKind::kRebindable;
  }

  return entry;
}

}  // namespace lyra::lowering::mir_to_llvm
