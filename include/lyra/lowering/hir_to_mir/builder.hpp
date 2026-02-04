#pragma once

#include <cstdint>
#include <functional>
#include <limits>
#include <optional>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::lowering::hir_to_mir {

// Forward declaration for cache type (definition in materialize_cache.hpp)
struct PlaceMaterializationCache;

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

  // Instruction emission.
  void EmitAssign(mir::PlaceId target, mir::Operand source);
  void EmitAssign(mir::PlaceId target, mir::Rvalue value);
  void EmitEffect(mir::EffectOp op);

  // Allocate PlaceTemp from Rvalue and emit Assign to initialize.
  auto EmitPlaceTemp(TypeId type, mir::Rvalue value) -> mir::PlaceId;

  // Emit DefineTemp statement and return UseTemp operand.
  // For expression intermediates - no alloca, no storage.
  auto EmitValueTemp(TypeId type, mir::Rvalue value) -> mir::Operand;
  auto EmitValueTempAssign(TypeId type, mir::Operand source) -> mir::Operand;

  // Allocate PlaceTemp from Operand. ALWAYS allocates - no caching.
  auto MaterializeOperandToPlace(TypeId type, mir::Operand value)
      -> mir::PlaceId;

  // Return existing PlaceId if kUse, otherwise materialize to new PlaceTemp.
  // kUse: returns place directly (caller ensures type matches base place type)
  // kConst/kUseTemp: allocates fresh PlaceTemp (no caching)
  auto MaterializeIfNeededToPlace(TypeId type, mir::Operand operand)
      -> mir::PlaceId;

  // Memoized materialization for projection bases.
  // kUse: returns place directly
  // kUseTemp: memoizes by (temp_id, type) to avoid duplicate allocations
  // kConst: materializes without memoization
  auto EnsurePlaceCached(
      TypeId type, mir::Operand operand, PlaceMaterializationCache& cache)
      -> mir::PlaceId;

  // Emit a Call instruction for user function invocation.
  // If return_type is void, dest is nullopt. Otherwise allocates temp.
  // Returns Use of result place, or Poison for void calls.
  auto EmitCall(
      mir::FunctionId callee, std::vector<mir::Operand> args,
      TypeId return_type) -> mir::Operand;

  // Emit a Call instruction with writebacks for output/inout parameters.
  // writebacks: list of output/inout parameter destinations.
  // For direct destination passing, dest is passed directly to callee.
  auto EmitCallWithWritebacks(
      mir::FunctionId callee, std::vector<mir::Operand> in_args,
      std::vector<mir::CallWriteback> writebacks, TypeId return_type)
      -> mir::Operand;

  // Emit a BuiltinCall instruction for container-mutating builtins.
  // For pop methods that return a value, returns Use of result place.
  // For void methods (push/delete/insert), returns Poison operand.
  auto EmitBuiltinCall(
      mir::BuiltinMethod method, mir::PlaceId receiver,
      std::vector<mir::Operand> args, TypeId return_type) -> mir::Operand;

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
  // Semantics: if (guard) dest := rhs; else no-op
  void EmitGuardedAssign(
      mir::PlaceId dest, mir::RightHandSide rhs, mir::Operand guard);

  // Emit DeferredAssign instruction: schedule dest := rhs for NBA region.
  void EmitDeferredAssign(mir::PlaceId dest, mir::RightHandSide rhs);

  // Emit unified Call for system TF (expression form).
  // Allocates staging temps for return and writebacks.
  // Returns Use(ret.tmp) for caller to assign to real dest.
  // Writeback tuple: (dest, type, pass_mode) - defaults to kStaged.
  auto EmitSystemTfCallExpr(
      SystemTfOpcode opcode, std::vector<mir::Operand> in_args,
      TypeId return_type,
      std::vector<std::tuple<mir::PlaceId, TypeId, mir::PassMode>> writebacks)
      -> mir::Operand;

  // Overload with explicit WritebackKind per writeback.
  // Use when at least one writeback is kDirectToDest.
  auto EmitSystemTfCallExpr(
      SystemTfOpcode opcode, std::vector<mir::Operand> in_args,
      TypeId return_type,
      std::vector<
          std::tuple<mir::PlaceId, TypeId, mir::PassMode, mir::WritebackKind>>
          writebacks) -> mir::Operand;

  // High-level control flow primitives.
  // These own all CFG mechanics: block creation, branches, reachability joins.
  // Statement lowering should use these instead of raw CFG operations.
  //
  // Callback contract: each body callback must either:
  //   (a) terminate its block (emit a terminator), or
  //   (b) leave a valid insertion point representing the fallthrough end block.
  // The builder captures CurrentBlock() after each callback to determine where
  // the fallthrough jump should go. Callbacks may create new blocks internally.

  // Emit if-then: condition ? then_body : (fall through)
  // After return, insertion point is at merge block (always reachable via
  // else).
  void EmitIf(mir::Operand condition, std::function<void()> then_body);

  // Emit if-then-else: condition ? then_body : else_body
  // After return, insertion point is at merge block if any branch fell through,
  // otherwise cleared (no insertion point).
  void EmitIfElse(
      mir::Operand condition, std::function<void()> then_body,
      std::function<void()> else_body);

  // Emit priority if chain: cascade of condition checks with short-circuit.
  // Each condition is evaluated lazily; on match, corresponding body executes.
  // After return, insertion point is at merge if any branch fell through.
  void EmitPriorityChain(
      const std::vector<std::function<mir::Operand()>>& conditions,
      const std::vector<std::function<void()>>& bodies,
      std::function<void()> else_body);

  // Emit unique dispatch: all conditions evaluated upfront, QualifiedDispatch.
  // After return, insertion point is at merge if any branch fell through.
  void EmitUniqueDispatch(
      mir::DispatchQualifier qualifier,
      mir::DispatchStatementKind statement_kind,
      const std::vector<mir::Operand>& conditions,
      const std::vector<std::function<void()>>& bodies,
      std::function<void()> else_body, bool has_else);

  // Case item for EmitCaseCascade: expressions (OR'd together) + body.
  struct CaseItem {
    std::vector<std::function<mir::Operand()>> expressions;
    std::function<void()> body;
  };

  // Emit case cascade: selector compared against each item's expressions.
  // For each item, expressions are OR'd (any match -> body executes).
  // After return, insertion point is at merge if any branch fell through.
  void EmitCaseCascade(
      mir::PlaceId selector, mir::BinaryOp comparison_op,
      const std::vector<CaseItem>& items, std::function<void()> default_body);

  // Low-level CFG primitives (for special cases like loops, delays, waits).
  // Prefer high-level primitives when possible.
  auto CreateBlock() -> BlockIndex;

  // Create a block with parameters that define SSA temps at block entry.
  // Returns the block index and the temp_ids for each parameter.
  // Callers use UseTemp(temp_id) to reference these temps.
  auto CreateBlockWithParams(std::vector<TypeId> param_types)
      -> std::pair<BlockIndex, std::vector<int>>;

  void SetCurrentBlock(BlockIndex block);
  [[nodiscard]] auto CurrentBlock() const -> BlockIndex;

  // Emit jump with optional edge args for target block params.
  void EmitJump(BlockIndex target, std::vector<mir::Operand> args = {});

  // Emit branch with optional edge args for each target's block params.
  void EmitBranch(
      mir::Operand cond, BlockIndex then_bb,
      std::vector<mir::Operand> then_args, BlockIndex else_bb,
      std::vector<mir::Operand> else_args);

  // Legacy overload without edge args (for backward compatibility).
  void EmitBranch(mir::Operand cond, BlockIndex then_bb, BlockIndex else_bb);
  void EmitQualifiedDispatch(
      mir::DispatchQualifier qualifier,
      mir::DispatchStatementKind statement_kind,
      const std::vector<mir::Operand>& conditions,
      const std::vector<BlockIndex>& targets, bool has_else);
  void EmitReturn(mir::Operand value);
  void EmitRepeat();
  void EmitDelay(uint64_t ticks, BlockIndex resume);
  void EmitWait(std::vector<mir::WaitTrigger> triggers, BlockIndex resume);
  void EmitTerminate(std::optional<mir::Finish> info = std::nullopt);

  // Emit implicit return for function epilogue.
  // If unreachable: no-op (all paths explicitly returned).
  // If reachable + void return type: emit Terminate.
  // If reachable + non-void return type: throws InternalError.
  void EmitImplicitReturn(TypeId return_type);

  // Emit process epilogue based on kind.
  // If unreachable: no-op (body explicitly terminated, e.g., $finish).
  // If reachable + kLooping: emit Repeat.
  // If reachable + kOnce/kFinal: emit Terminate.
  void EmitProcessEpilogue(mir::ProcessKind kind);

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

  // Single-exit form for functions: set exit block for return lowering.
  void SetExitBlock(BlockIndex block);
  [[nodiscard]] auto GetExitBlock() const -> BlockIndex;

  // Origin tracking for error reporting.
  // SetCurrentHirSource sets the HIR source for subsequent instructions/
  // terminators. Recording happens at emit time with proper MIR references.
  void SetCurrentHirSource(hir::StatementId stmt_id);
  void SetCurrentHirSource(hir::ExpressionId expr_id);
  void ClearCurrentHirSource();

  // Legacy API - sets current_origin_ directly (used for pre-recorded origins).
  void SetCurrentOrigin(common::OriginId origin) {
    current_origin_ = origin;
  }
  [[nodiscard]] auto GetCurrentOrigin() const -> common::OriginId {
    return current_origin_;
  }

  // Record a projection origin for error reporting.
  // Used by lvalue lowering to attach origins to projections.
  auto RecordProjectionOrigin(hir::ExpressionId expr_id) -> common::OriginId;

  // Returns true if there is a valid insertion point (current block exists
  // and has no terminator yet). Used for fallthrough detection and control
  // flow decisions.
  [[nodiscard]] auto IsReachable() const -> bool;

 private:
  // Clear the insertion point, making subsequent emissions no-ops.
  // Used structurally when all branches of a control flow construct terminate.
  void ClearInsertionPoint();

  struct BlockBuilder {
    std::vector<mir::BlockParam> params;
    std::vector<mir::Statement> statements;
    std::optional<mir::Terminator> terminator;
  };

  // Centralized instruction emission - all EmitX instructions delegate here.
  // Handles finished/reachability checks in one place.
  template <class InstT>
  void EmitInst(InstT inst);

  // Centralized terminator emission - all terminator emitters delegate here.
  // Handles finished/reachability checks. After sealing, IsReachable() is
  // false.
  template <class TermT>
  void EmitTerm(TermT term);

  // HIR source for instructions/terminators (statement or expression only).
  // FunctionId/ProcessId are recorded separately at the function/process level.
  using InstructionHirSource =
      std::variant<hir::StatementId, hir::ExpressionId>;

  mir::Arena* arena_;
  Context* ctx_;
  OriginMap* origin_map_;
  BlockIndex current_block_;
  std::vector<BlockBuilder> blocks_;
  std::vector<LoopContext> loop_stack_;
  common::OriginId current_origin_ = common::OriginId::Invalid();
  std::optional<InstructionHirSource>
      current_hir_source_;  // For deferred recording
  bool finished_ = false;
  BlockIndex exit_block_ = kInvalidBlockIndex;  // Single-exit form
};

}  // namespace lyra::lowering::hir_to_mir
