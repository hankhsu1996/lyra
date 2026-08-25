#pragma once

#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// References to the snapshot var produced by AppendCaseSnapshot. Predicate
// builders use these to compare `_lyra_case_sel` against each item's labels.
struct CaseSnapshotRefs {
  mir::LocalId sel_var;
  mir::TypeId sel_type;
};

// Appends the SV-case selector snapshot to `frame.current_block`:
//   T _lyra_case_sel = T-default;   // LocalDeclStmt
//   _lyra_case_sel = <cond>;        // ExprStmt(AssignExpr)
//
// Caller has already lowered the selector into the wrapper's expression
// table and passes its ExprId. The decl + assign are split (instead of a
// single var-decl-with-init) so the cpp backend's packed-init gap does not
// bite when the selector unifies to a packed-explicit type.
auto AppendCaseSnapshot(
    const UnitLowerer& unit_lowerer, WalkFrame frame, mir::ExprId cond_expr_id)
    -> CaseSnapshotRefs;

// Assembles the SV-case if/else-if cascade. `wrapper_block` already holds the
// snapshot decl + assign (from AppendCaseSnapshot); it is moved in. body_scopes
// and default_scope are pre-lowered Blocks. `frame` is the frame the cascade
// lives in; it carries the parent block and closure context so per-level
// predicate blocks get the right composition when the predicate builder
// constructs them.
//
// build_predicate(level_frame, item_idx) is invoked once per item from
// innermost to outermost, so it is borrowed rather than forwarded: a callable
// forwarded more than once would be consumed by its first call. level_frame's
// current_block points at the block into which the predicate is lowered
// (wrapper_block for the outermost item, a fresh intermediate block for the
// rest). The snapshot var is a body-local, so the predicate names it directly
// with no hop bookkeeping.
//
// An item count of zero is the ordinary shape of `case (x) default: ...`, whose
// only arm the front end reports separately from the items. It is also what a
// miscounted item list looks like, which is why the count is taken from the
// vector here and never accepted as a parameter beside it.
//
// Returns a BlockStmt wrapping the snapshot + cascade.
template <typename PredicateBuilder>
auto BuildCaseCascade(
    WalkFrame frame, mir::Block wrapper_block,
    std::optional<std::string> outer_label, std::vector<mir::Block> body_scopes,
    std::optional<mir::Block> default_scope, mir::TypeId bit1_type,
    const PredicateBuilder& build_predicate) -> diag::Result<mir::Stmt> {
  const std::size_t item_count = body_scopes.size();
  std::optional<mir::Block> tail = std::move(default_scope);

  for (std::size_t i = item_count; i-- > 1;) {
    mir::Block level_block;
    const WalkFrame level_frame = frame.WithBlock(&level_block);
    auto pred_or = build_predicate(level_frame, i);
    if (!pred_or) {
      return std::unexpected(std::move(pred_or.error()));
    }

    const mir::BlockId body_scope_id =
        level_block.child_scopes.Add(std::move(body_scopes[i]));
    std::optional<mir::BlockId> else_scope_id;
    if (tail.has_value()) {
      else_scope_id = level_block.child_scopes.Add(std::move(*tail));
    }

    level_block.AppendStmt(
        mir::IfStmt{
            .condition = ReduceToCondition(level_block, *pred_or, bit1_type),
            .then_scope = body_scope_id,
            .else_scope = else_scope_id});

    tail = std::move(level_block);
  }

  if (item_count > 0) {
    const WalkFrame wrapper_frame = frame.WithBlock(&wrapper_block);
    auto pred0_or = build_predicate(wrapper_frame, 0);
    if (!pred0_or) {
      return std::unexpected(std::move(pred0_or.error()));
    }

    const mir::BlockId body0_id =
        wrapper_block.child_scopes.Add(std::move(body_scopes[0]));
    std::optional<mir::BlockId> else0_id;
    if (tail.has_value()) {
      else0_id = wrapper_block.child_scopes.Add(std::move(*tail));
    }

    wrapper_block.AppendStmt(
        mir::IfStmt{
            .condition = ReduceToCondition(wrapper_block, *pred0_or, bit1_type),
            .then_scope = body0_id,
            .else_scope = else0_id});
  } else if (tail.has_value()) {
    const mir::BlockId def_id =
        wrapper_block.child_scopes.Add(std::move(*tail));
    wrapper_block.AppendStmt(mir::BlockStmt{.scope = def_id});
  }

  const mir::BlockId wrapper_scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper_block));

  return mir::Stmt{
      .label = std::move(outer_label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

}  // namespace lyra::lowering::hir_to_mir
