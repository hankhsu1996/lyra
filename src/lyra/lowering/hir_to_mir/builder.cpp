#include "lyra/lowering/hir_to_mir/builder.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::hir_to_mir {

MirBuilder::MirBuilder(mir::Arena* arena, Context* ctx, OriginMap* origin_map)
    : arena_(arena),
      ctx_(ctx),
      origin_map_(origin_map),
      current_block_(kInvalidBlockIndex),
      blocks_{} {
  assert(arena_ && ctx_);
}

void MirBuilder::RecordStatementOrigin(hir::StatementId stmt_id) {
  if (origin_map_ == nullptr) {
    return;  // No map, skip recording
  }

  // Record this statement as the source for subsequent MIR nodes.
  // We use kInstruction as the kind since most emitted MIR comes from stmt
  // lowering. The mir_index is 0 since we're tracking at statement granularity,
  // not per-instruction.
  common::OriginId origin =
      origin_map_->Record(MirNodeKind::kInstruction, 0, stmt_id);
  current_origin_ = origin;
}

auto MirBuilder::CreateBlock() -> BlockIndex {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "CreateBlock called after Finish()");
  }
  BlockIndex idx{static_cast<uint32_t>(blocks_.size())};
  blocks_.emplace_back();
  return idx;
}

void MirBuilder::SetCurrentBlock(BlockIndex block) {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "SetCurrentBlock called after Finish()");
  }
  if (block.value >= blocks_.size()) {
    throw common::InternalError(
        "MirBuilder", "SetCurrentBlock: invalid block index");
  }
  current_block_ = block;
}

auto MirBuilder::CurrentBlock() const -> BlockIndex {
  return current_block_;
}

void MirBuilder::EmitAssign(mir::PlaceId target, mir::Operand source) {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "EmitAssign called after Finish()");
  }
  if (current_block_ == kInvalidBlockIndex) {
    throw common::InternalError("MirBuilder", "EmitAssign: no current block");
  }
  blocks_[current_block_.value].instructions.push_back(
      mir::Instruction{
          .data = mir::Assign{.target = target, .source = std::move(source)},
          .origin = current_origin_});
}

void MirBuilder::EmitCompute(mir::PlaceId target, mir::Rvalue value) {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "EmitCompute called after Finish()");
  }
  if (current_block_ == kInvalidBlockIndex) {
    throw common::InternalError("MirBuilder", "EmitCompute: no current block");
  }
  blocks_[current_block_.value].instructions.push_back(
      mir::Instruction{
          .data = mir::Compute{.target = target, .value = std::move(value)},
          .origin = current_origin_});
}

void MirBuilder::EmitEffect(mir::EffectOp op) {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "EmitEffect called after Finish()");
  }
  if (current_block_ == kInvalidBlockIndex) {
    throw common::InternalError("MirBuilder", "EmitEffect: no current block");
  }
  blocks_[current_block_.value].instructions.push_back(
      mir::Instruction{
          .data = mir::Effect{.op = std::move(op)}, .origin = current_origin_});
}

auto MirBuilder::EmitTemp(TypeId type, mir::Rvalue value) -> mir::PlaceId {
  mir::PlaceId temp = ctx_->AllocTemp(type);
  EmitCompute(temp, std::move(value));
  return temp;
}

auto MirBuilder::EmitTempAssign(TypeId type, mir::Operand source)
    -> mir::PlaceId {
  mir::PlaceId temp = ctx_->AllocTemp(type);
  EmitAssign(temp, source);
  return temp;
}

auto MirBuilder::EmitUnary(
    mir::UnaryOp op, mir::Operand operand, TypeId result_type) -> mir::Operand {
  mir::Rvalue rvalue{
      .operands = {std::move(operand)},
      .info = mir::UnaryRvalueInfo{.op = op},
  };
  return mir::Operand::Use(EmitTemp(result_type, std::move(rvalue)));
}

auto MirBuilder::EmitBinary(
    mir::BinaryOp op, mir::Operand lhs, mir::Operand rhs, TypeId result_type)
    -> mir::Operand {
  mir::Rvalue rvalue{
      .operands = {std::move(lhs), std::move(rhs)},
      .info = mir::BinaryRvalueInfo{.op = op},
  };
  return mir::Operand::Use(EmitTemp(result_type, std::move(rvalue)));
}

auto MirBuilder::EmitCast(
    mir::Operand source, TypeId source_type, TypeId target_type)
    -> mir::Operand {
  if (source_type == target_type) {
    return source;
  }
  mir::Rvalue rvalue{
      .operands = {std::move(source)},
      .info =
          mir::CastRvalueInfo{
              .source_type = source_type, .target_type = target_type},
  };
  return mir::Operand::Use(EmitTemp(target_type, std::move(rvalue)));
}

auto MirBuilder::EmitIndexValidity(
    mir::Operand index, int64_t lower, int64_t upper, bool check_known)
    -> mir::Operand {
  mir::Rvalue rvalue{
      .operands = {std::move(index)},
      .info =
          mir::IndexValidityRvalueInfo{
              .lower_bound = lower,
              .upper_bound = upper,
              .check_known = check_known},
  };
  return mir::Operand::Use(EmitTemp(ctx_->GetBitType(), std::move(rvalue)));
}

auto MirBuilder::EmitGuardedUse(
    mir::Operand validity, mir::PlaceId place, TypeId result_type)
    -> mir::Operand {
  mir::Rvalue rvalue{
      .operands = {std::move(validity)},
      .info =
          mir::GuardedUseRvalueInfo{.place = place, .result_type = result_type},
  };
  return mir::Operand::Use(EmitTemp(result_type, std::move(rvalue)));
}

void MirBuilder::EmitGuardedAssign(
    mir::PlaceId target, mir::Operand source, mir::Operand validity) {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "EmitGuardedAssign called after Finish()");
  }
  if (current_block_ == kInvalidBlockIndex) {
    throw common::InternalError(
        "MirBuilder", "EmitGuardedAssign: no current block");
  }
  blocks_[current_block_.value].instructions.push_back(
      mir::Instruction{
          .data =
              mir::GuardedAssign{
                  .target = target,
                  .source = std::move(source),
                  .validity = std::move(validity)},
          .origin = current_origin_});
}

void MirBuilder::EmitNonBlockingAssign(
    mir::PlaceId target, mir::Operand source) {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "EmitNonBlockingAssign called after Finish()");
  }
  if (current_block_ == kInvalidBlockIndex) {
    throw common::InternalError(
        "MirBuilder", "EmitNonBlockingAssign: no current block");
  }
  blocks_[current_block_.value].instructions.push_back(
      mir::Instruction{
          .data =
              mir::NonBlockingAssign{
                  .target = target, .source = std::move(source)},
          .origin = current_origin_});
}

void MirBuilder::SealCurrentBlock(mir::Terminator terminator) {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "SealCurrentBlock called after Finish()");
  }
  if (current_block_ == kInvalidBlockIndex) {
    throw common::InternalError(
        "MirBuilder", "SealCurrentBlock: no current block");
  }
  auto& block = blocks_[current_block_.value];
  if (block.terminator.has_value()) {
    throw common::InternalError(
        "MirBuilder", "SealCurrentBlock: block already has terminator");
  }
  block.terminator = std::move(terminator);
}

void MirBuilder::EmitJump(BlockIndex target) {
  SealCurrentBlock(
      mir::Terminator{
          .data = mir::Jump{.target = mir::BasicBlockId{target.value}},
          .origin = current_origin_});
}

void MirBuilder::EmitBranch(
    mir::Operand cond, BlockIndex then_bb, BlockIndex else_bb) {
  if (cond.kind != mir::Operand::Kind::kUse) {
    throw common::InternalError(
        "EmitBranch", "branch condition must be a Use operand");
  }
  auto cond_place = std::get<mir::PlaceId>(cond.payload);

  SealCurrentBlock(
      mir::Terminator{
          .data =
              mir::Branch{
                  .condition = cond_place,
                  .then_target = mir::BasicBlockId{then_bb.value},
                  .else_target = mir::BasicBlockId{else_bb.value},
              },
          .origin = current_origin_});
}

void MirBuilder::EmitQualifiedDispatch(
    mir::DispatchQualifier qualifier, mir::DispatchStatementKind statement_kind,
    const std::vector<mir::PlaceId>& conditions,
    const std::vector<BlockIndex>& targets, bool has_else) {
  // Validate invariant: targets = one per condition + else fallthrough
  if (targets.size() != conditions.size() + 1) {
    throw common::InternalError(
        "EmitQualifiedDispatch",
        "targets.size() must equal conditions.size() + 1");
  }

  std::vector<mir::BasicBlockId> bb_targets;
  bb_targets.reserve(targets.size());
  for (const auto& target : targets) {
    bb_targets.push_back(mir::BasicBlockId{target.value});
  }

  SealCurrentBlock(
      mir::Terminator{
          .data =
              mir::QualifiedDispatch{
                  .qualifier = qualifier,
                  .statement_kind = statement_kind,
                  .conditions = conditions,
                  .targets = std::move(bb_targets),
                  .has_else = has_else,
              },
          .origin = current_origin_});
}

void MirBuilder::EmitReturn() {
  SealCurrentBlock(
      mir::Terminator{.data = mir::Return{}, .origin = current_origin_});
}

void MirBuilder::EmitRepeat() {
  SealCurrentBlock(
      mir::Terminator{.data = mir::Repeat{}, .origin = current_origin_});
}

void MirBuilder::EmitDelay(uint64_t ticks, BlockIndex resume) {
  SealCurrentBlock(
      mir::Terminator{
          .data =
              mir::Delay{
                  .ticks = ticks,
                  .resume = mir::BasicBlockId{resume.value},
              },
          .origin = current_origin_});
}

void MirBuilder::EmitWait(
    std::vector<mir::WaitTrigger> triggers, BlockIndex resume) {
  SealCurrentBlock(
      mir::Terminator{
          .data =
              mir::Wait{
                  .triggers = std::move(triggers),
                  .resume = mir::BasicBlockId{resume.value},
              },
          .origin = current_origin_});
}

void MirBuilder::EmitTerminate(std::optional<mir::Finish> info) {
  if (info) {
    SealCurrentBlock(
        mir::Terminator{.data = *std::move(info), .origin = current_origin_});
  } else {
    // Implicit process termination: use Return (don't stop simulation).
    // Only explicit $finish/$stop/$fatal should use Finish terminator.
    SealCurrentBlock(
        mir::Terminator{.data = mir::Return{}, .origin = current_origin_});
  }
}

namespace {

// Helper to validate terminator targets are in-range.
void ValidateTerminatorTargets(const mir::Terminator& term, size_t num_blocks) {
  auto check_target = [num_blocks](mir::BasicBlockId target) {
    if (target.value >= num_blocks) {
      throw common::InternalError(
          "MirBuilder", "terminator target out of bounds");
    }
  };

  std::visit(
      [&](const auto& t) {
        using T = std::decay_t<decltype(t)>;
        if constexpr (std::is_same_v<T, mir::Jump>) {
          check_target(t.target);
        } else if constexpr (std::is_same_v<T, mir::Branch>) {
          check_target(t.then_target);
          check_target(t.else_target);
        } else if constexpr (std::is_same_v<T, mir::Switch>) {
          for (const auto& target : t.targets) {
            check_target(target);
          }
        } else if constexpr (std::is_same_v<T, mir::QualifiedDispatch>) {
          for (const auto& target : t.targets) {
            check_target(target);
          }
        } else if constexpr (std::is_same_v<T, mir::Delay>) {
          check_target(t.resume);
        } else if constexpr (std::is_same_v<T, mir::Wait>) {
          check_target(t.resume);
        }
        // Return, Finish, Repeat have no targets
      },
      term.data);
}

}  // namespace

auto MirBuilder::Finish() -> std::vector<mir::BasicBlock> {
  if (finished_) {
    throw common::InternalError("MirBuilder", "Finish called twice");
  }

  // Verify all blocks are sealed and terminator targets are in-range
  for (const auto& bb : blocks_) {
    if (!bb.terminator.has_value()) {
      throw common::InternalError("MirBuilder", "block has no terminator");
    }
    ValidateTerminatorTargets(*bb.terminator, blocks_.size());
  }

  finished_ = true;

  // Build result - terminator targets are already local indices
  std::vector<mir::BasicBlock> result;
  result.reserve(blocks_.size());
  for (auto& bb : blocks_) {
    result.push_back(
        mir::BasicBlock{
            .instructions = std::move(bb.instructions),
            .terminator = std::move(*bb.terminator),
        });
  }
  return result;
}

void MirBuilder::PushLoop(LoopContext ctx) {
  loop_stack_.push_back(ctx);
}

void MirBuilder::PopLoop() {
  loop_stack_.pop_back();
}

auto MirBuilder::CurrentLoop() const -> const LoopContext* {
  return loop_stack_.empty() ? nullptr : &loop_stack_.back();
}

}  // namespace lyra::lowering::hir_to_mir
