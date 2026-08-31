#pragma once

// Realization of HIR patterns (LRM 12.6) as MIR: the boolean test a pattern
// imposes on a value, and the bindings it introduces when that test passes.
// Shared by every construct whose predicate is a clause sequence -- the
// conditional statement, the conditional expression, and the pattern case
// statement.

#include <optional>
#include <span>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/case_cascade.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The boolean test a pattern imposes on `subject`, the expression reaching the
// value it is matched against, or nullopt when the pattern matches
// unconditionally (a wildcard, a bare binding, a structure of those). Emits
// only the predicate's own nodes into the frame's block. Descending is one
// expression deep: how a level comes apart is read from that level's pattern
// node, which carries the type of what it matches.
template <ExprLowerer Lowerer>
auto BuildPatternPredicate(
    Lowerer& lowerer, WalkFrame frame, mir::ExprId subject,
    hir::PatternId pattern_id) -> diag::Result<std::optional<mir::ExprId>>;

// Declares each identifier the pattern introduces as a local of `decl_frame`'s
// block and assigns it, from the matching position of the subject, in
// `assign_frame`'s block; registers it so the arm's body resolves references
// to it. Assignment belongs where the predicate has already succeeded, while
// the declaration has to reach every reader, and those are not always the
// same block -- LRM 12.4.2 lets a qualified `if` read a binding from outside
// the level whose predicate produced it.
template <ExprLowerer Lowerer>
void EmitPatternBindings(
    Lowerer& lowerer, WalkFrame decl_frame, WalkFrame assign_frame,
    mir::ExprId subject, hir::PatternId pattern_id);

// The `if` that runs a chain's else-arm: it guards on the chain having failed,
// read from the same flag the chain sets. An `else` cannot simply hang off the
// chain's outermost level, because a clause below the top fails by falling out
// of an inner `if` that the outermost `else` never sees.
[[nodiscard]] auto BuildChainElseIf(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::LocalId taken_flag, mir::TypeId bit1_type, mir::BlockId else_scope)
    -> mir::IfStmt;

// One level of a clause chain: the `if` for `clauses.front()`, recursing for
// the tail, with the innermost level running `emit_then`. Each clause guards
// one level, and the identifiers its pattern introduces are assigned inside
// that level, which is where the clauses to its right and the then-arm are
// emitted -- that is what puts a binding in scope for them (LRM 12.4 /
// 12.6.2). Their declarations go to `decl_frame`, the block the chain
// started in, because a reader can sit outside the level that assigned them.
//
// A clause carrying a pattern is snapshotted to a local: its value is read
// both by the predicate at this level and by the bindings one level down. A
// clause without a pattern needs neither, so a plain `if (expr)` is one
// level whose predicate is `expr` -- no snapshot, no scope.
//
// `taken_flag`, when present, is set just before the then-arm, so a caller
// can tell whether the chain as a whole held rather than which level failed.
template <ExprLowerer Lowerer, typename EmitThen>
auto BuildClauseChainLevel(
    Lowerer& lowerer, WalkFrame decl_frame, WalkFrame frame,
    std::span<const hir::ConditionClause> clauses,
    std::optional<mir::LocalId> taken_flag, const EmitThen& emit_then)
    -> diag::Result<mir::IfStmt> {
  const mir::TypeId bit1_type = lowerer.Owner().Unit().builtins.bit1;
  auto& block = *frame.current_block;

  const hir::ConditionClause& clause = clauses.front();
  const hir::Expr& clause_hir = lowerer.HirExprs().Get(clause.expr);
  auto clause_or = lowerer.LowerExpr(clause_hir, frame);
  if (!clause_or) return std::unexpected(std::move(clause_or.error()));
  const mir::ExprId clause_id = block.exprs.Add(*std::move(clause_or));

  mir::Block level_block;
  const WalkFrame level_frame = frame.WithBlock(&level_block);

  std::optional<mir::ExprId> predicate = clause_id;
  if (clause.pattern.has_value()) {
    const mir::TypeId subject_mir_type =
        lowerer.Owner().TranslateType(clause_hir.type);
    const mir::LocalId subject_var =
        AppendCaseSnapshot(lowerer.Owner(), frame, clause_id).sel_var;
    auto pred_or = BuildPatternPredicate(
        lowerer, frame,
        block.exprs.Add(mir::MakeLocalRefExpr(subject_var, subject_mir_type)),
        *clause.pattern);
    if (!pred_or) return std::unexpected(std::move(pred_or.error()));
    predicate = *pred_or;
    EmitPatternBindings(
        lowerer, decl_frame, level_frame,
        level_block.exprs.Add(
            mir::MakeLocalRefExpr(subject_var, subject_mir_type)),
        *clause.pattern);
  }

  if (clauses.size() == 1) {
    if (taken_flag.has_value()) {
      const mir::ExprId flag_ref =
          level_block.exprs.Add(mir::MakeLocalRefExpr(*taken_flag, bit1_type));
      const mir::ExprId one =
          BuildBit1Literal(lowerer.Owner().Unit(), level_block, true);
      level_block.AppendStmt(
          mir::ExprStmt{
              .expr = level_block.exprs.Add(
                  mir::MakeAssignExpr(flag_ref, one, bit1_type))});
    }
    auto then_or = emit_then(level_frame);
    if (!then_or) return std::unexpected(std::move(then_or.error()));
  } else {
    auto tail_or = BuildClauseChainLevel(
        lowerer, decl_frame, level_frame, clauses.subspan(1), taken_flag,
        emit_then);
    if (!tail_or) return std::unexpected(std::move(tail_or.error()));
    level_block.AppendStmt(*std::move(tail_or));
  }

  // LRM 12.6: a pattern that constrains nothing always succeeds, so a clause
  // with no test guards its level with the literal that always holds.
  const mir::ExprId predicate_id =
      predicate.has_value()
          ? *predicate
          : BuildBit1Literal(lowerer.Owner().Unit(), block, true);
  return mir::IfStmt{
      .condition =
          ReduceToCondition(lowerer.Owner().Unit(), block, predicate_id),
      .then_scope = block.child_scopes.Add(std::move(level_block)),
      .else_scope = std::nullopt};
}

// Starts a clause chain in `frame`'s block, which is therefore where the
// bindings its patterns introduce are declared.
template <ExprLowerer Lowerer, typename EmitThen>
auto BuildClauseChainIf(
    Lowerer& lowerer, WalkFrame frame,
    std::span<const hir::ConditionClause> clauses,
    std::optional<mir::LocalId> taken_flag, const EmitThen& emit_then)
    -> diag::Result<mir::IfStmt> {
  return BuildClauseChainLevel(
      lowerer, frame, frame, clauses, taken_flag, emit_then);
}

}  // namespace lyra::lowering::hir_to_mir
