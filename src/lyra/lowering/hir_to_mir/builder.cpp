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
      });
}

void MirBuilder::EmitReturn() {
  SealCurrentBlock(
      mir::Terminator{
          .kind = mir::Terminator::Kind::kReturn,
          .targets = {},
          .condition_operand = 0,
      });
}

void MirBuilder::EmitRepeat() {
  SealCurrentBlock(
      mir::Terminator{
          .kind = mir::Terminator::Kind::kRepeat,
          .targets = {},
          .condition_operand = 0,
      });
}

void MirBuilder::EmitFinish() {
  SealCurrentBlock(
      mir::Terminator{
          .kind = mir::Terminator::Kind::kFinish,
          .targets = {},
          .condition_operand = 0,
      });
}

auto MirBuilder::Finish() -> std::vector<mir::BasicBlockId> {
  if (finished_) {
    throw common::InternalError("MirBuilder", "Finish called twice");
  }

  // Verify current block is sealed (no half-built blocks)
  if (current_block_ != kInvalidBlockIndex) {
    const auto& block = blocks_[current_block_.value];
    if (!block.terminator.has_value()) {
      throw common::InternalError(
          "MirBuilder", "Finish: current block has no terminator");
    }
  }

  finished_ = true;

  std::vector<mir::BasicBlockId> arena_ids;
  arena_ids.reserve(blocks_.size());

  for (uint32_t i = 0; i < blocks_.size(); ++i) {
    auto& bb = blocks_[i];
    if (!bb.terminator.has_value()) {
      throw common::InternalError(
          "MirBuilder", "Finish: block has no terminator");
    }

    // Convert builder-local indices in terminator targets to arena IDs
    mir::Terminator term = std::move(*bb.terminator);
    for (auto& target : term.targets) {
      target = ToArenaId(BlockIndex{target.value});
    }

    mir::BasicBlock block{
        .id = mir::BasicBlockId{i},
        .instructions = std::move(bb.instructions),
        .terminator = std::move(term),
    };

    mir::BasicBlockId arena_id = arena_->AddBasicBlock(std::move(block));
    arena_ids.push_back(arena_id);
  }

  return arena_ids;
}

auto MirBuilder::ToArenaId(BlockIndex idx) -> mir::BasicBlockId {
  return mir::BasicBlockId{idx.value};
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
