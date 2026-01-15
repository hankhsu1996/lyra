#pragma once

#include <optional>
#include <vector>

#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::hir_to_mir {

class MirBuilder {
 public:
  MirBuilder(mir::Arena& arena, Context& ctx);

  auto CreateBlock() -> mir::BasicBlockId;
  void SetCurrentBlock(mir::BasicBlockId block);
  [[nodiscard]] auto CurrentBlock() const -> mir::BasicBlockId;

  void EmitAssign(mir::PlaceId target, mir::Operand source);
  void EmitCompute(mir::PlaceId target, mir::Rvalue value);
  auto EmitTemp(TypeId type, mir::Rvalue value) -> mir::PlaceId;

  void EmitJump(mir::BasicBlockId target);
  void EmitBranch(
      mir::Operand cond, mir::BasicBlockId then_bb, mir::BasicBlockId else_bb);
  void EmitReturn(std::optional<mir::Operand> value);
  void EmitRepeat();
  void EmitFinish();

  auto GetArena() -> mir::Arena& {
    return arena_;
  }
  auto GetContext() -> Context& {
    return ctx_;
  }

  [[nodiscard]] auto GetBlocks() const
      -> const std::vector<mir::BasicBlockId>& {
    return blocks_;
  }

 private:
  mir::Arena& arena_;
  Context& ctx_;
  mir::BasicBlockId current_block_;
  std::vector<mir::BasicBlockId> blocks_;
  std::vector<mir::Instruction> current_instructions_;
};

}  // namespace lyra::lowering::hir_to_mir
