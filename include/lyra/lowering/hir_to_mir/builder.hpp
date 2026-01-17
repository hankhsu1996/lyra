#pragma once

#include <cstdint>
#include <limits>
#include <optional>
#include <vector>

#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::hir_to_mir {

// Builder-local block index. Used during construction to reference blocks
// before they are materialized into the Arena.
struct BlockIndex {
  uint32_t value;

  auto operator==(const BlockIndex&) const -> bool = default;
};

inline constexpr BlockIndex kInvalidBlockIndex{
    std::numeric_limits<uint32_t>::max()};

// Loop context for break/continue lowering.
struct LoopContext {
  BlockIndex exit_block;
  BlockIndex continue_block;
};

class MirBuilder {
 public:
  MirBuilder(mir::Arena* arena, Context* ctx);

  auto CreateBlock() -> BlockIndex;
  void SetCurrentBlock(BlockIndex block);
  [[nodiscard]] auto CurrentBlock() const -> BlockIndex;

  void EmitAssign(mir::PlaceId target, mir::Operand source);
  void EmitCompute(mir::PlaceId target, mir::Rvalue value);
  void EmitEffect(mir::EffectOp op);
  auto EmitTemp(TypeId type, mir::Rvalue value) -> mir::PlaceId;

  void EmitJump(BlockIndex target);
  void EmitBranch(mir::Operand cond, BlockIndex then_bb, BlockIndex else_bb);
  void EmitReturn();
  void EmitRepeat();
  void EmitTerminate(std::optional<mir::TerminationInfo> info = std::nullopt);

  // Materialize all blocks into the Arena. Returns the arena IDs in the same
  // order as blocks were created. Call this once at the end of lowering.
  auto Finish() -> std::vector<mir::BasicBlockId>;

  // Map a builder-local BlockIndex to an arena BasicBlockId.
  [[nodiscard]] static auto ToArenaId(BlockIndex idx) -> mir::BasicBlockId;

  auto GetArena() -> mir::Arena& {
    return *arena_;
  }
  auto GetContext() -> Context& {
    return *ctx_;
  }

  void PushLoop(LoopContext ctx);
  void PopLoop();
  [[nodiscard]] auto CurrentLoop() const -> const LoopContext*;

 private:
  struct BlockBuilder {
    std::vector<mir::Instruction> instructions;
    std::optional<mir::Terminator> terminator;
  };

  void SealCurrentBlock(mir::Terminator terminator);

  mir::Arena* arena_;
  Context* ctx_;
  BlockIndex current_block_;
  std::vector<BlockBuilder> blocks_;
  std::vector<LoopContext> loop_stack_;
  bool finished_ = false;
};

}  // namespace lyra::lowering::hir_to_mir
