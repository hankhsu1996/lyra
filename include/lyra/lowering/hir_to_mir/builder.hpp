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
  auto EmitTempAssign(TypeId type, mir::Operand source) -> mir::PlaceId;

  // Emit a Select rvalue and materialize to temp. Always returns Use(temp).
  // This enforces the invariant that Select results are never raw projections.
  auto EmitSelect(
      mir::Operand cond, mir::Operand true_val, mir::Operand false_val,
      TypeId result_type) -> mir::Operand;

  // Emit a unary operation and materialize to temp.
  auto EmitUnary(mir::UnaryOp op, mir::Operand operand, TypeId result_type)
      -> mir::Operand;

  // Emit a binary operation and materialize to temp.
  auto EmitBinary(
      mir::BinaryOp op, mir::Operand lhs, mir::Operand rhs, TypeId result_type)
      -> mir::Operand;

  // Emit IndexValidity rvalue: computes "index is valid access" predicate.
  // Returns 1-bit 2-state bool: (lower <= index <= upper) && is_known(index).
  auto EmitIndexValidity(
      mir::Operand index, int64_t lower, int64_t upper, bool check_known)
      -> mir::Operand;

  // Emit GuardedUse rvalue: conditionally read from place with OOB safety.
  // Returns: validity ? Use(place) : oob_default
  auto EmitGuardedUse(
      mir::Operand validity, mir::PlaceId place, TypeId result_type)
      -> mir::Operand;

  // Emit GuardedAssign instruction: conditionally write to place.
  // Semantics: if (validity) Assign(target, source); else no-op
  void EmitGuardedAssign(
      mir::PlaceId target, mir::Operand source, mir::Operand validity);

  void EmitJump(BlockIndex target);
  void EmitBranch(mir::Operand cond, BlockIndex then_bb, BlockIndex else_bb);
  void EmitReturn();
  void EmitRepeat();
  void EmitTerminate(std::optional<mir::TerminationInfo> info = std::nullopt);

  // Materialize all blocks. Returns blocks in the same order as they were
  // created. BasicBlockId targets in terminators are local indices.
  // Call this once at the end of lowering.
  auto Finish() -> std::vector<mir::BasicBlock>;

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
