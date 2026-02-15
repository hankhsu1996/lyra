#include "lyra/llvm_backend/process.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/BasicBlock.h>
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
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/compute/rvalue.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction/display.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/statement.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

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
  // Map from (block_index, param_index) to the PHI node
  std::unordered_map<uint64_t, llvm::PHINode*> phis;

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
  // For each block with params, verify each PHI has incoming values from
  // all LLVM predecessors. Call this AFTER wiring but BEFORE LLVM verify.
  void ValidatePhiWiring(
      const std::vector<llvm::BasicBlock*>& llvm_blocks,
      std::string_view func_name) const {
    for (const auto& [key, phi] : phis) {
      size_t block_idx = key >> 32;
      size_t param_idx = key & 0xFFFFFFFF;
      llvm::BasicBlock* bb = llvm_blocks[block_idx];

      // Count LLVM predecessors
      size_t pred_count = 0;
      for (auto it = llvm::pred_begin(bb); it != llvm::pred_end(bb); ++it) {
        ++pred_count;
      }

      size_t incoming_count = phi->getNumIncomingValues();
      if (incoming_count != pred_count) {
        // Build list of predecessors for error message
        std::string preds;
        for (auto it = llvm::pred_begin(bb); it != llvm::pred_end(bb); ++it) {
          if (!preds.empty()) preds += ", ";
          preds += (*it)->getName().str();
        }

        std::string incoming_preds;
        for (unsigned i = 0; i < phi->getNumIncomingValues(); ++i) {
          if (!incoming_preds.empty()) incoming_preds += ", ";
          incoming_preds += phi->getIncomingBlock(i)->getName().str();
        }

        throw common::InternalError(
            "PHI wiring",
            std::format(
                "function '{}' block {} param {}: PHI has {} incoming values "
                "but block has {} predecessors. LLVM preds: [{}], PHI preds: "
                "[{}]",
                func_name, block_idx, param_idx, incoming_count, pred_count,
                preds, incoming_preds));
      }
    }
  }

  // Wire up PHI incoming values from pending edges.
  // Call this after all blocks have been lowered.
  void WireIncomingEdges() {
    for (const auto& edge : pending_edges) {
      for (size_t j = 0; j < edge.args.size(); ++j) {
        auto* phi = phis[MakeKey(edge.succ_idx, j)];
        if (phi->getType() != edge.args[j]->getType()) {
          std::string phi_type_str;
          llvm::raw_string_ostream phi_os(phi_type_str);
          phi->getType()->print(phi_os);
          std::string arg_type_str;
          llvm::raw_string_ostream arg_os(arg_type_str);
          edge.args[j]->getType()->print(arg_os);
          throw common::InternalError(
              "PHI wiring",
              std::format(
                  "block {} param {}: PHI type ({}) != incoming value type "
                  "({}) from pred '{}'",
                  edge.succ_idx, j, phi_type_str, arg_type_str,
                  edge.pred->getName().str()));
        }
        phi->addIncoming(edge.args[j], edge.pred);
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
      // Insert PHIs at the start of the block
      builder.SetInsertPoint(llvm_blocks[i], llvm_blocks[i]->begin());
      for (size_t j = 0; j < block.params.size(); ++j) {
        const auto& param = block.params[j];
        auto llvm_ty_or_err = GetLlvmTypeForType(context, param.type);
        if (!llvm_ty_or_err) return std::unexpected(llvm_ty_or_err.error());
        auto* phi =
            builder.CreatePHI(*llvm_ty_or_err, 2, std::format("phi{}", j));
        phi_state.phis[PhiWiringState::MakeKey(i, j)] = phi;
        // Bind the temp_id to this PHI value with its MIR type
        context.BindTemp(param.temp_id, phi, param.type);
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

auto LoadConditionAsI1(Context& context, const mir::Operand& operand)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();

  // Lower the operand to get the condition value
  auto cond_val_or_err = LowerOperandRaw(context, operand);
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

// Lower edge args to LLVM values with full type representation.
// Uses LowerOperandRaw (not LowerOperand) to preserve 4-state structure.
// PHI nodes expect the full type representation, not just the "known-true"
// value.
auto LowerEdgeArgs(Context& context, const std::vector<mir::Operand>& args)
    -> Result<std::vector<llvm::Value*>> {
  std::vector<llvm::Value*> llvm_args;
  llvm_args.reserve(args.size());
  for (const auto& arg : args) {
    auto val_or_err = LowerOperandRaw(context, arg);
    if (!val_or_err) return std::unexpected(val_or_err.error());
    llvm_args.push_back(*val_or_err);
  }
  return llvm_args;
}

auto LowerJump(
    Context& context, const mir::Jump& jump,
    const std::vector<llvm::BasicBlock*>& blocks, PhiWiringState& phi_state)
    -> Result<void> {
  auto* current_bb = context.GetBuilder().GetInsertBlock();

  // Lower edge args BEFORE creating the terminator (they may emit loads)
  std::vector<llvm::Value*> arg_values;
  if (!jump.args.empty()) {
    auto args_or_err = LowerEdgeArgs(context, jump.args);
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

auto LowerBranch(
    Context& context, const mir::Branch& branch,
    const std::vector<llvm::BasicBlock*>& blocks, PhiWiringState& phi_state)
    -> Result<void> {
  auto* current_bb = context.GetBuilder().GetInsertBlock();
  auto cond_or_err = LoadConditionAsI1(context, branch.condition);
  if (!cond_or_err) return std::unexpected(cond_or_err.error());

  // Lower edge args BEFORE creating the branch (while still in predecessor)
  std::vector<llvm::Value*> then_llvm_args;
  std::vector<llvm::Value*> else_llvm_args;
  if (!branch.then_args.empty()) {
    auto args_or_err = LowerEdgeArgs(context, branch.then_args);
    if (!args_or_err) return std::unexpected(args_or_err.error());
    then_llvm_args = std::move(*args_or_err);
  }
  if (!branch.else_args.empty()) {
    auto args_or_err = LowerEdgeArgs(context, branch.else_args);
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

  const auto& arena = context.GetMirArena();
  const auto& place = arena[*trigger.observed_place];
  TypeId root_type = context.GetMirDesign().slot_table[trigger.signal.value];

  auto resolver =
      [&context](const mir::Operand& op) -> std::optional<uint64_t> {
    if (op.kind != mir::Operand::Kind::kUseTemp) return std::nullopt;
    auto temp_id = std::get<mir::TempId>(op.payload);
    if (!context.HasTemp(temp_id.value)) return std::nullopt;
    auto* val = context.ReadTemp(temp_id.value);
    if (const auto* ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
      return ci->getZExtValue();
    }
    return std::nullopt;
  };
  auto range = ResolveByteRange(
      context.GetLlvmContext(), context.GetModule().getDataLayout(),
      context.GetTypeArena(), place, root_type, resolver);
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

  // Select: in-bounds -> computed values, OOB -> UINT32_MAX sentinel.
  // byte_size=0 already means "full slot" in the ABI, so we use UINT32_MAX
  // as a distinct OOB sentinel that the runtime recognizes.
  auto* zero_32 = llvm::ConstantInt::get(i32_ty, 0);
  auto* zero_8 = llvm::ConstantInt::get(i8_ty, 0);
  auto* one_32 = llvm::ConstantInt::get(i32_ty, 1);
  auto* oob_sentinel = llvm::ConstantInt::get(i32_ty, UINT32_MAX);
  auto* sel_byte_off = builder.CreateSelect(in_bounds, byte_off_32, zero_32);
  auto* sel_bit_idx = builder.CreateSelect(in_bounds, bit_idx_8, zero_8);
  auto* sel_byte_size = builder.CreateSelect(in_bounds, one_32, oob_sentinel);

  // Store into WaitTriggerRecord fields.
  auto* bit_idx_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 2);
  builder.CreateStore(sel_bit_idx, bit_idx_ptr);
  auto* offset_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 4);
  builder.CreateStore(sel_byte_off, offset_ptr);
  auto* size_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 5);
  builder.CreateStore(sel_byte_size, size_ptr);
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

    // Write signal_id (field 0)
    auto* signal_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 0);
    builder.CreateStore(
        llvm::ConstantInt::get(i32_ty, triggers[i].signal.value), signal_ptr);

    // Write edge (field 1)
    auto* edge_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 1);
    builder.CreateStore(
        llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(triggers[i].edge)),
        edge_ptr);

    if (triggers[i].late_bound) {
      // Dynamic bit-select: emit IR to compute target at suspend time.
      EmitDynamicBitTarget(context, elem_ptr, trigger_type, triggers[i]);
    } else {
      // Static path: resolve observation range at compile time.
      auto range = ResolveObservationRange(context, triggers[i]);
      auto* bit_idx_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 2);
      builder.CreateStore(
          llvm::ConstantInt::get(i8_ty, range.bit_index), bit_idx_ptr);
      auto* offset_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 4);
      builder.CreateStore(
          llvm::ConstantInt::get(i32_ty, range.byte_offset), offset_ptr);
      auto* size_ptr = builder.CreateStructGEP(trigger_type, elem_ptr, 5);
      builder.CreateStore(
          llvm::ConstantInt::get(i32_ty, range.byte_size), size_ptr);
    }
  }
}

// Fill late-bound trigger record array for dynamic-index edge triggers.
void FillLateBoundArray(
    Context& context, llvm::Value* base_ptr, llvm::Type* lb_type,
    llvm::Type* array_type, const std::vector<mir::WaitTrigger>& triggers) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  uint32_t lb_idx = 0;
  for (uint32_t i = 0; i < triggers.size(); ++i) {
    if (!triggers[i].late_bound) continue;
    const auto& lb = *triggers[i].late_bound;
    // Skip local-index entries (no rebind subscription needed).
    if (lb.index_slot == mir::kInvalidSlotId) continue;

    auto* elem_ptr =
        builder.CreateConstGEP2_32(array_type, base_ptr, 0, lb_idx);

    // trigger_index (field 0)
    auto* f0 = builder.CreateStructGEP(lb_type, elem_ptr, 0);
    builder.CreateStore(llvm::ConstantInt::get(i32_ty, i), f0);

    // index_slot_id (field 1)
    auto* f1 = builder.CreateStructGEP(lb_type, elem_ptr, 1);
    builder.CreateStore(
        llvm::ConstantInt::get(i32_ty, lb.index_slot.value), f1);

    // index_byte_offset (field 2)
    auto* f2 = builder.CreateStructGEP(lb_type, elem_ptr, 2);
    builder.CreateStore(
        llvm::ConstantInt::get(i32_ty, lb.index_byte_offset), f2);

    // index_byte_size (field 3)
    auto* f3 = builder.CreateStructGEP(lb_type, elem_ptr, 3);
    builder.CreateStore(llvm::ConstantInt::get(i32_ty, lb.index_byte_size), f3);

    // index_base (field 4)
    auto* f4 = builder.CreateStructGEP(lb_type, elem_ptr, 4);
    builder.CreateStore(
        llvm::ConstantInt::get(
            i32_ty, static_cast<uint32_t>(lb.mapping.index_base)),
        f4);

    // index_step (field 5)
    auto* f5 = builder.CreateStructGEP(lb_type, elem_ptr, 5);
    builder.CreateStore(
        llvm::ConstantInt::get(
            i8_ty, static_cast<uint8_t>(lb.mapping.index_step)),
        f5);

    // index_bit_width (field 6)
    auto* f6 = builder.CreateStructGEP(lb_type, elem_ptr, 6);
    builder.CreateStore(llvm::ConstantInt::get(i8_ty, lb.index_bit_width), f6);

    // index_is_signed (field 7)
    auto* f7 = builder.CreateStructGEP(lb_type, elem_ptr, 7);
    builder.CreateStore(
        llvm::ConstantInt::get(
            i8_ty, static_cast<uint8_t>(lb.index_is_signed ? 1 : 0)),
        f7);

    // padding (field 8) -- skip

    // total_bits (field 9)
    auto* f9 = builder.CreateStructGEP(lb_type, elem_ptr, 9);
    builder.CreateStore(
        llvm::ConstantInt::get(i32_ty, lb.mapping.total_bits), f9);

    ++lb_idx;
  }
}

auto LowerWait(
    Context& context, const mir::Wait& wait, llvm::BasicBlock* exit_block)
    -> Result<void> {
  static_assert(sizeof(runtime::WaitTriggerRecord) == 16);

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  auto num_triggers = static_cast<uint32_t>(wait.triggers.size());

  // WaitTriggerRecord layout:
  // {i32 signal_id, i8 edge, i8 bit_index, [2 x i8] padding, i32 byte_offset,
  //  i32 byte_size}
  auto* trigger_type = llvm::StructType::get(
      llvm_ctx,
      {i32_ty, i8_ty, i8_ty, llvm::ArrayType::get(i8_ty, 2), i32_ty, i32_ty});

  // Count late-bound triggers that need runtime rebind subscriptions.
  // Local-index triggers (kInvalidSlotId) use the dynamic codegen path
  // but don't need LateBoundTriggerRecords at runtime.
  uint32_t num_late_bound = 0;
  for (const auto& t : wait.triggers) {
    if (t.late_bound && t.late_bound->index_slot != mir::kInvalidSlotId)
      ++num_late_bound;
  }

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

    if (num_late_bound > 0) {
      // LateBoundTriggerRecord layout:
      // {i32 trigger_index, i32 index_slot_id, i32 index_byte_offset,
      //  i32 index_byte_size, i32 index_base, i8 index_step,
      //  i8 index_bit_width, i8 index_is_signed, i8 padding, i32 total_bits}
      static_assert(sizeof(runtime::LateBoundTriggerRecord) == 28);
      auto* lb_type = llvm::StructType::get(
          llvm_ctx, {i32_ty, i32_ty, i32_ty, i32_ty, i32_ty, i8_ty, i8_ty,
                     i8_ty, i8_ty, i32_ty});
      auto* lb_array_type = llvm::ArrayType::get(lb_type, num_late_bound);
      auto* lb_alloca =
          builder.CreateAlloca(lb_array_type, nullptr, "late_bound");
      FillLateBoundArray(
          context, lb_alloca, lb_type, lb_array_type, wait.triggers);

      builder.CreateCall(
          context.GetLyraSuspendWaitWithLateBound(),
          {context.GetStatePointer(),
           llvm::ConstantInt::get(i32_ty, wait.resume.value), triggers_alloca,
           llvm::ConstantInt::get(i32_ty, num_triggers), lb_alloca,
           llvm::ConstantInt::get(i32_ty, num_late_bound)});
    } else {
      builder.CreateCall(
          context.GetLyraSuspendWait(),
          {context.GetStatePointer(),
           llvm::ConstantInt::get(i32_ty, wait.resume.value), triggers_alloca,
           llvm::ConstantInt::get(i32_ty, num_triggers)});
    }
  } else {
    // Large path: runtime allocates scratch buffer, we fill it, runtime copies
    auto* heap_ptr = builder.CreateCall(
        context.GetLyraAllocTriggers(),
        {llvm::ConstantInt::get(i32_ty, num_triggers)}, "heap_triggers");

    auto* array_type = llvm::ArrayType::get(trigger_type, num_triggers);
    FillTriggerArray(
        context, heap_ptr, trigger_type, array_type, wait.triggers);

    if (num_late_bound > 0) {
      static_assert(sizeof(runtime::LateBoundTriggerRecord) == 28);
      auto* lb_type = llvm::StructType::get(
          llvm_ctx, {i32_ty, i32_ty, i32_ty, i32_ty, i32_ty, i8_ty,
                     llvm::ArrayType::get(i8_ty, 3), i32_ty});
      auto* lb_array_type = llvm::ArrayType::get(lb_type, num_late_bound);
      auto* lb_alloca =
          builder.CreateAlloca(lb_array_type, nullptr, "late_bound");
      FillLateBoundArray(
          context, lb_alloca, lb_type, lb_array_type, wait.triggers);

      builder.CreateCall(
          context.GetLyraSuspendWaitWithLateBound(),
          {context.GetStatePointer(),
           llvm::ConstantInt::get(i32_ty, wait.resume.value), heap_ptr,
           llvm::ConstantInt::get(i32_ty, num_triggers), lb_alloca,
           llvm::ConstantInt::get(i32_ty, num_late_bound)});
    } else {
      builder.CreateCall(
          context.GetLyraSuspendWait(),
          {context.GetStatePointer(),
           llvm::ConstantInt::get(i32_ty, wait.resume.value), heap_ptr,
           llvm::ConstantInt::get(i32_ty, num_triggers)});
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
  for (const auto& cond_op : dispatch.conditions) {
    auto cond_or_err = LoadConditionAsI1(context, cond_op);
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
    const std::vector<llvm::BasicBlock*>& blocks, llvm::BasicBlock* exit_block,
    PhiWiringState& phi_state) -> Result<void> {
  // Set origin for error reporting.
  // OriginScope preserves outer (function/process) origin if term.origin is
  // Invalid.
  OriginScope origin_scope(context, term.origin);

  return std::visit(
      common::Overloaded{
          [&](const mir::Jump& t) -> Result<void> {
            return LowerJump(context, t, blocks, phi_state);
          },
          [&](const mir::Branch& t) -> Result<void> {
            return LowerBranch(context, t, blocks, phi_state);
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

  // Collect actual resume targets: blocks that are jumped to after Delay/Wait.
  // bb0 is always accessible (initial execution) but handled as the default
  // case.
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
          // Repeat doesn't have an explicit resume target in terminator
        },
        block.terminator.data);
  }

  // Assert: resume target blocks (and bb0) must NOT have block params.
  // Resume edges are additional predecessors that can't provide PHI incoming
  // values.
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
              "resume target block {} has {} params; resume targets must have "
              "no block params",
              target, process.blocks[target].params.size()));
    }
  }

  // Create switch with bb0 as default (initial execution)
  auto* sw = builder.CreateSwitch(
      resume_block_arg, llvm_blocks[0],
      static_cast<unsigned>(resume_targets.size()));

  // Add cases only for actual resume targets (bb0 is the default, not a case)
  for (size_t target : resume_targets) {
    sw->addCase(
        llvm::ConstantInt::get(i32_ty, static_cast<uint64_t>(target)),
        llvm_blocks[target]);
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

    for (const auto& instruction : block.statements) {
      auto result = LowerStatement(context, instruction);
      if (!result) return std::unexpected(result.error());
    }

    auto term_result = LowerTerminator(
        context, block.terminator, llvm_blocks, exit_block, phi_state);
    if (!term_result) return std::unexpected(term_result.error());
  }

  // Wire PHI incoming edges and validate
  phi_state.WireIncomingEdges();
  phi_state.ValidatePhiWiring(llvm_blocks, func->getName().str());

  // Exit block: just return
  builder.SetInsertPoint(exit_block);
  builder.CreateRetVoid();

  // Clear cached pointers for next function
  context.SetStatePointer(nullptr);
  context.SetDesignPointer(nullptr);
  context.SetFramePointer(nullptr);
  context.SetEnginePointer(nullptr);

  VerifyLlvmFunction(func, "GenerateProcessFunction");
  return func;
}

auto DeclareUserFunction(
    Context& context, mir::FunctionId func_id, const std::string& name)
    -> Result<llvm::Function*> {
  auto& module = context.GetModule();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& arena = context.GetMirArena();
  const auto& func = arena[func_id];

  llvm::FunctionType* fn_type = nullptr;

  // Determine LLVM function type based on thunk_kind (from MIR, not side
  // table). MIR is the source of truth for calling convention.
  switch (func.thunk_kind) {
    case mir::ThunkKind::kMonitorCheck: {
      // Monitor check: (design*, engine*, prev_buf*)
      auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
      auto* void_ty = llvm::Type::getVoidTy(llvm_ctx);
      fn_type =
          llvm::FunctionType::get(void_ty, {ptr_ty, ptr_ty, ptr_ty}, false);
      break;
    }
    default: {
      // All other thunks use standard signature: (design*, engine*)
      // or with out-param: (out_ptr*, design*, engine*)
      auto fn_type_or_err = context.BuildUserFunctionType(func.signature);
      if (!fn_type_or_err) return std::unexpected(fn_type_or_err.error());
      fn_type = *fn_type_or_err;
      break;
    }
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
              std::is_same_v<T, mir::IndexValidityRvalueInfo> ||
              std::is_same_v<T, mir::ConcatRvalueInfo> ||
              std::is_same_v<T, mir::ReplicateRvalueInfo> ||
              std::is_same_v<T, mir::RuntimeQueryRvalueInfo> ||
              std::is_same_v<T, mir::MathCallRvalueInfo> ||
              std::is_same_v<T, mir::SystemTfRvalueInfo> ||
              std::is_same_v<T, mir::ArrayQueryRvalueInfo>) {
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
          }
          // StrobeEffect, TimeFormatEffect, MonitorControlEffect: no operands
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
            } else if constexpr (std::is_same_v<T, mir::QualifiedDispatch>) {
              // QualifiedDispatch::conditions is vector<Operand>
              for (const auto& cond : term.conditions) {
                CollectFromOperand(cond, arena);
              }
            }
          },
          block.terminator.data);
    }
  }
};

}  // namespace

// Special lowering for monitor check thunks that adds comparison logic.
// The thunk evaluates expressions, compares against prev_buffer, and only
// prints if values changed.
auto DefineMonitorCheckThunk(
    Context& context, mir::FunctionId func_id, llvm::Function* llvm_func,
    const Context::MonitorLayout& layout) -> Result<void> {
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

  // Arguments: design*, engine*, prev_buffer*
  auto* design_arg = llvm_func->getArg(0);
  design_arg->setName("design");
  context.SetDesignPointer(design_arg);

  auto* engine_arg = llvm_func->getArg(1);
  engine_arg->setName("engine");
  context.SetEnginePointer(engine_arg);

  auto* prev_buf_arg = llvm_func->getArg(2);
  prev_buf_arg->setName("prev_buf");

  // Collect local/temp places for storage allocation
  PlaceCollector collector;
  collector.CollectFromFunction(func, arena);

  // Allocate and initialize all collected places at function entry.
  // INVARIANT: Thunks must follow the same allocate-all + init-all policy as
  // user functions; no lazy storage creation is allowed. ComputePlacePointer
  // will throw InternalError if storage is missing.
  for (const auto& [key, root] : collector.roots) {
    auto alloca_or_err = context.GetOrCreatePlaceStorage(root);
    if (!alloca_or_err) return std::unexpected(alloca_or_err.error());
    context.InitializePlaceStorage(*alloca_or_err, root.type);
  }

  // Find the DisplayEffect from check thunk's MIR body (correct operand
  // context). layout only provides offsets/byte_sizes; format_ops come from the
  // thunk itself.
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
        func.origin, "monitor check thunk missing DisplayEffect",
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
              // At Return: evaluate operands from check thunk's DisplayEffect
              // (correct MIR operand context), store to temp buffer, compare.
              if (temp_buf != nullptr) {
                auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

                // Evaluate each operand from the check thunk's DisplayEffect
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
                      "terminator not yet supported in monitor check thunk",
                      UnsupportedCategory::kFeature));
            },
        },
        block.terminator.data);
    if (!term_result) return std::unexpected(term_result.error());
  }

  // Wire PHI incoming edges and validate
  phi_state.WireIncomingEdges();
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

  VerifyLlvmFunction(llvm_func, "DefineMonitorCheckThunk");

  context.EndFunction();
  context.SetDesignPointer(nullptr);
  context.SetEnginePointer(nullptr);

  return {};
}

// Emit setup thunk epilogue: serialize current values + call
// LyraMonitorRegister. Called at end of setup thunk's exit block, before
// return.
auto EmitMonitorSetupEpilogue(
    Context& context, mir::FunctionId setup_thunk_id,
    const Context::MonitorSetupInfo& info, llvm::Value* design_ptr,
    llvm::Value* engine_ptr) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& arena = context.GetMirArena();

  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  // Find the DisplayEffect from setup thunk's MIR body (correct operand
  // context).
  const mir::Function& setup_func = arena[setup_thunk_id];
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
        "$monitor setup thunk missing DisplayEffect",
        UnsupportedCategory::kFeature));
  }

  // Get check thunk function pointer for registration
  llvm::Function* check_fn = context.GetUserFunction(info.check_thunk);
  if (check_fn == nullptr) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "$monitor check thunk not found for setup epilogue",
        UnsupportedCategory::kFeature));
  }

  // Look up layout via check_thunk (single source of truth)
  const auto* layout = context.GetMonitorLayout(info.check_thunk);
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
    // Use display_effect->ops (correct MIR operand context for setup thunk).
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
      // Must match check thunk's serialization.
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

  // Call LyraMonitorRegister(engine, check_fn, design, init_buf, size)
  builder.CreateCall(
      context.GetLyraMonitorRegister(),
      {engine_ptr, check_fn, design_ptr, init_buf,
       llvm::ConstantInt::get(i32_ty, layout->total_size)});

  return {};
}

auto DefineUserFunction(
    Context& context, mir::FunctionId func_id, llvm::Function* llvm_func)
    -> Result<void> {
  const auto& arena = context.GetMirArena();
  const auto& func = arena[func_id];

  // Monitor check thunks have special lowering with comparison logic.
  // Use thunk_kind from MIR (source of truth), layout from side table (codegen
  // artifact).
  if (func.thunk_kind == mir::ThunkKind::kMonitorCheck) {
    const auto* layout = context.GetMonitorLayout(func_id);
    if (layout == nullptr) {
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          func.origin, "monitor check thunk missing layout",
          UnsupportedCategory::kFeature));
    }
    return DefineMonitorCheckThunk(context, func_id, llvm_func, *layout);
  }

  auto& llvm_ctx = context.GetLlvmContext();
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

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

  // DesignState* and Engine* follow sret (if present)
  auto* design_arg = llvm_func->getArg(arg_offset);
  design_arg->setName("design");
  context.SetDesignPointer(design_arg);

  auto* engine_arg = llvm_func->getArg(arg_offset + 1);
  engine_arg->setName("engine");
  context.SetEnginePointer(engine_arg);

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
  // entry. This matches MIR interpreter semantics and ensures deterministic
  // behavior. Order: 1) allocate all, 2) initialize all, 3) store parameters
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
  // Arguments start at index arg_offset + 2 (after sret/design/engine).
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
          llvm_func->getArg(static_cast<unsigned>(i + arg_offset + 2));
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
                        "DefineUserFunction",
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

  // Wire PHI incoming edges and validate
  phi_state.WireIncomingEdges();
  phi_state.ValidatePhiWiring(llvm_blocks, llvm_func->getName().str());

  // Exit block: return the value
  builder.SetInsertPoint(exit_block);

  // Setup thunks need serialization + registration before returning.
  // Use thunk_kind from MIR (source of truth), setup_info from side table
  // (codegen artifact).
  if (func.thunk_kind == mir::ThunkKind::kMonitorSetup) {
    const auto* setup_info = context.GetMonitorSetupInfo(func_id);
    if (setup_info == nullptr) {
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          func.origin, "monitor setup thunk missing info",
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

  VerifyLlvmFunction(llvm_func, "DefineUserFunction");

  // Clean up function scope
  context.EndFunction();
  context.SetDesignPointer(nullptr);

  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
