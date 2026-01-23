#pragma once

#include <cstdint>
#include <limits>
#include <optional>
#include <vector>

#include "lyra/common/unsupported_error.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/origin_map.hpp"
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
  MirBuilder(mir::Arena* arena, Context* ctx, OriginMap* origin_map = nullptr);

  auto CreateBlock() -> BlockIndex;
  void SetCurrentBlock(BlockIndex block);
  [[nodiscard]] auto CurrentBlock() const -> BlockIndex;

  void EmitAssign(mir::PlaceId target, mir::Operand source);
  void EmitCompute(mir::PlaceId target, mir::Rvalue value);
  void EmitEffect(mir::EffectOp op);
  auto EmitTemp(TypeId type, mir::Rvalue value) -> mir::PlaceId;
  auto EmitTempAssign(TypeId type, mir::Operand source) -> mir::PlaceId;

  // Emit a unary operation and materialize to temp.
  auto EmitUnary(mir::UnaryOp op, mir::Operand operand, TypeId result_type)
      -> mir::Operand;

  // Emit a binary operation and materialize to temp.
  auto EmitBinary(
      mir::BinaryOp op, mir::Operand lhs, mir::Operand rhs, TypeId result_type)
      -> mir::Operand;

  // Emit a cast operation and materialize to temp.
  auto EmitCast(mir::Operand source, TypeId source_type, TypeId target_type)
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
  void EmitQualifiedDispatch(
      mir::DispatchQualifier qualifier,
      mir::DispatchStatementKind statement_kind,
      const std::vector<mir::PlaceId>& conditions,
      const std::vector<BlockIndex>& targets, bool has_else);
  void EmitReturn();
  void EmitRepeat();
  void EmitDelay(uint64_t ticks, BlockIndex resume);
  void EmitWait(std::vector<mir::WaitTrigger> triggers, BlockIndex resume);
  void EmitTerminate(std::optional<mir::Finish> info = std::nullopt);

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

  // Origin tracking for error reporting.
  // RecordStatementOrigin records a statement origin in the OriginMap (if set)
  // and sets it as the current origin for subsequent emit calls.
  void RecordStatementOrigin(hir::StatementId stmt_id);

  void SetCurrentOrigin(common::OriginId origin) {
    current_origin_ = origin;
  }
  [[nodiscard]] auto GetCurrentOrigin() const -> common::OriginId {
    return current_origin_;
  }

 private:
  struct BlockBuilder {
    std::vector<mir::Instruction> instructions;
    std::optional<mir::Terminator> terminator;
  };

  void SealCurrentBlock(mir::Terminator terminator);

  mir::Arena* arena_;
  Context* ctx_;
  OriginMap* origin_map_;
  BlockIndex current_block_;
  std::vector<BlockBuilder> blocks_;
  std::vector<LoopContext> loop_stack_;
  common::OriginId current_origin_ = common::OriginId::Invalid();
  bool finished_ = false;
};

}  // namespace lyra::lowering::hir_to_mir
