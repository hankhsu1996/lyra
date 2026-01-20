#include "lyra/lowering/hir_to_mir/builder.hpp"

#include <cassert>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/basic_block.hpp"

namespace lyra::lowering::hir_to_mir {

MirBuilder::MirBuilder(mir::Arena* arena, Context* ctx)
    : arena_(arena), ctx_(ctx), current_block_(kInvalidBlockIndex), blocks_{} {
  assert(arena_ && ctx_);
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
  blocks_[current_block_.value].instructions.emplace_back(
      mir::Assign{.target = target, .source = std::move(source)});
}

void MirBuilder::EmitCompute(mir::PlaceId target, mir::Rvalue value) {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "EmitCompute called after Finish()");
  }
  if (current_block_ == kInvalidBlockIndex) {
    throw common::InternalError("MirBuilder", "EmitCompute: no current block");
  }
  blocks_[current_block_.value].instructions.emplace_back(
      mir::Compute{.target = target, .value = std::move(value)});
}

void MirBuilder::EmitEffect(mir::EffectOp op) {
  if (finished_) {
    throw common::InternalError(
        "MirBuilder", "EmitEffect called after Finish()");
  }
  if (current_block_ == kInvalidBlockIndex) {
    throw common::InternalError("MirBuilder", "EmitEffect: no current block");
  }
  blocks_[current_block_.value].instructions.emplace_back(
      mir::Effect{.op = std::move(op)});
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
  blocks_[current_block_.value].instructions.emplace_back(
      mir::GuardedAssign{
          .target = target,
          .source = std::move(source),
          .validity = std::move(validity)});
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
          .kind = mir::Terminator::Kind::kJump,
          .targets = {mir::BasicBlockId{target.value}},
          .condition_operand = 0,
          .termination_info = std::nullopt,
      });
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
          .kind = mir::Terminator::Kind::kBranch,
          .targets =
              {mir::BasicBlockId{then_bb.value},
               mir::BasicBlockId{else_bb.value}},
          .condition_operand = static_cast<int>(cond_place.value),
          .termination_info = std::nullopt,
      });
}

void MirBuilder::EmitReturn() {
  SealCurrentBlock(
      mir::Terminator{
          .kind = mir::Terminator::Kind::kReturn,
          .targets = {},
          .condition_operand = 0,
          .termination_info = std::nullopt,
      });
}

void MirBuilder::EmitRepeat() {
  SealCurrentBlock(
      mir::Terminator{
          .kind = mir::Terminator::Kind::kRepeat,
          .targets = {},
          .condition_operand = 0,
          .termination_info = std::nullopt,
      });
}

void MirBuilder::EmitTerminate(std::optional<mir::TerminationInfo> info) {
  SealCurrentBlock(
      mir::Terminator{
          .kind = mir::Terminator::Kind::kFinish,
          .targets = {},
          .condition_operand = 0,
          .termination_info = info,
      });
}

auto MirBuilder::Finish() -> std::vector<mir::BasicBlock> {
  if (finished_) {
    throw common::InternalError("MirBuilder", "Finish called twice");
  }

  // Verify all blocks are sealed and terminator targets are in-range
  for (const auto& bb : blocks_) {
    if (!bb.terminator.has_value()) {
      throw common::InternalError("MirBuilder", "block has no terminator");
    }
    for (const auto& target : bb.terminator->targets) {
      if (target.value >= blocks_.size()) {
        throw common::InternalError(
            "MirBuilder", "terminator target out of bounds");
      }
    }
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
