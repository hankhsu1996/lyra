#include "lyra/lowering/hir_to_mir/builder.hpp"

#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::hir_to_mir {

MirBuilder::MirBuilder(mir::Arena& arena, Context& ctx)
    : arena_(arena),
      ctx_(ctx),
      current_block_(mir::kInvalidBasicBlockId),
      blocks_{},
      current_instructions_{} {
}

auto MirBuilder::CreateBlock() -> mir::BasicBlockId {
  mir::BasicBlockId id{static_cast<uint32_t>(blocks_.size())};
  blocks_.push_back(id);
  return id;
}

void MirBuilder::SetCurrentBlock(mir::BasicBlockId block) {
  if (!current_instructions_.empty()) {
    throw common::InternalError(
        "MirBuilder",
        "SetCurrentBlock called with pending "
        "instructions (missing terminator?)");
  }
  current_block_ = block;
}

auto MirBuilder::CurrentBlock() const -> mir::BasicBlockId {
  return current_block_;
}

void MirBuilder::EmitAssign(mir::PlaceId target, mir::Operand source) {
  current_instructions_.push_back(
      mir::Assign{.target = target, .source = std::move(source)});
}

void MirBuilder::EmitCompute(mir::PlaceId target, mir::Rvalue value) {
  current_instructions_.push_back(
      mir::Compute{.target = target, .value = std::move(value)});
}

auto MirBuilder::EmitTemp(TypeId type, mir::Rvalue value) -> mir::PlaceId {
  mir::PlaceId temp = ctx_.AllocTemp(type);
  EmitCompute(temp, std::move(value));
  return temp;
}

void MirBuilder::EmitJump(mir::BasicBlockId target) {
  mir::BasicBlock block{
      .id = current_block_,
      .instructions = std::move(current_instructions_),
      .terminator =
          mir::Terminator{
              .kind = mir::Terminator::Kind::kJump,
              .targets = {target},
              .condition_operand = 0,
          },
  };
  arena_.AddBasicBlock(std::move(block));
  current_instructions_.clear();
}

void MirBuilder::EmitBranch(
    mir::Operand cond, mir::BasicBlockId then_bb, mir::BasicBlockId else_bb) {
  (void)cond;  // TODO(hankhsu): Store condition operand properly
  mir::BasicBlock block{
      .id = current_block_,
      .instructions = std::move(current_instructions_),
      .terminator =
          mir::Terminator{
              .kind = mir::Terminator::Kind::kBranch,
              .targets = {then_bb, else_bb},
              .condition_operand = 0,
          },
  };
  arena_.AddBasicBlock(std::move(block));
  current_instructions_.clear();
}

void MirBuilder::EmitReturn(std::optional<mir::Operand> value) {
  (void)value;  // TODO(hankhsu): Store return value properly
  mir::BasicBlock block{
      .id = current_block_,
      .instructions = std::move(current_instructions_),
      .terminator =
          mir::Terminator{
              .kind = mir::Terminator::Kind::kReturn,
              .targets = {},
              .condition_operand = 0,
          },
  };
  arena_.AddBasicBlock(std::move(block));
  current_instructions_.clear();
}

void MirBuilder::EmitRepeat() {
  mir::BasicBlock block{
      .id = current_block_,
      .instructions = std::move(current_instructions_),
      .terminator =
          mir::Terminator{
              .kind = mir::Terminator::Kind::kRepeat,
              .targets = {},
              .condition_operand = 0,
          },
  };
  arena_.AddBasicBlock(std::move(block));
  current_instructions_.clear();
}

void MirBuilder::EmitFinish() {
  mir::BasicBlock block{
      .id = current_block_,
      .instructions = std::move(current_instructions_),
      .terminator =
          mir::Terminator{
              .kind = mir::Terminator::Kind::kFinish,
              .targets = {},
              .condition_operand = 0,
          },
  };
  arena_.AddBasicBlock(std::move(block));
  current_instructions_.clear();
}

}  // namespace lyra::lowering::hir_to_mir
