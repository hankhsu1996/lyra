#pragma once

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/expr.hpp"
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
    const ModuleLowerer& module, WalkFrame frame, mir::ExprId cond_expr_id)
    -> CaseSnapshotRefs;

// Builds (sel <op> L_0) || (sel <op> L_1) || ... into the enclosing scope
// reached via `frame.current_block` and returns the final predicate
// ExprId. `compare_op` selects the per-label primitive (kEquality for plain
// case / case-inside-value items, kCasezEquality / kCasexEquality for casez /
// casex). `lower_label(label_frame, label_idx)` lowers one label into the
// scope reached via `label_frame.current_block` and returns its
// ExprId. label_count must be >= 1.
template <typename LabelLowerer>
auto BuildEqualityChain(
    WalkFrame frame, CaseSnapshotRefs snapshot, mir::TypeId bit_type,
    mir::BinaryOp compare_op, std::uint32_t sel_hops, std::size_t label_count,
    LabelLowerer&& lower_label) -> diag::Result<mir::ExprId> {
  auto& enc_scope = *frame.current_block;
  std::optional<mir::ExprId> acc;
  for (std::size_t i = 0; i < label_count; ++i) {
    auto label_or = std::forward<LabelLowerer>(lower_label)(frame, i);
    if (!label_or) {
      return std::unexpected(std::move(label_or.error()));
    }
    const mir::ExprId label_id = *label_or;
    const mir::ExprId sel_ref = enc_scope.exprs.Add(
        mir::Expr{
            .data =
                mir::LocalRef{
                    .hops = mir::BlockHops{.value = sel_hops},
                    .var = snapshot.sel_var},
            .type = snapshot.sel_type});
    const mir::ExprId cmp = enc_scope.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = compare_op, .lhs = sel_ref, .rhs = label_id},
            .type = bit_type});
    if (acc.has_value()) {
      acc = enc_scope.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = mir::BinaryOp::kLogicalOr, .lhs = *acc, .rhs = cmp},
              .type = bit_type});
    } else {
      acc = cmp;
    }
  }
  return *acc;
}

// Assembles the SV-case if/else-if cascade. `wrapper_block` already holds the
// snapshot decl + assign (from AppendCaseSnapshot); it is moved in. body_scopes
// and default_scope are pre-lowered Blocks (caller is responsible for
// any depth bookkeeping required during their lowering). `frame` is the frame
// the cascade lives in; it carries the parent block, depth, and
// closure context so per-level predicate blocks get the right composition when
// the predicate builder constructs them.
//
// build_predicate(level_frame, item_idx, sel_hops) is invoked once per item
// from innermost (item_count - 1) to outermost (0). level_frame's
// current_block points at the block into which the predicate is
// lowered (wrapper_block for the outermost item, a fresh intermediate block
// for the rest). sel_hops gives the hop count from that block to the one
// where the snapshot var was declared.
//
// Returns a BlockStmt wrapping the snapshot + cascade.
template <typename PredicateBuilder>
auto BuildCaseCascade(
    WalkFrame frame, mir::Block wrapper_block,
    std::optional<std::string> outer_label, std::size_t item_count,
    std::vector<mir::Block> body_scopes,
    std::optional<mir::Block> default_scope, PredicateBuilder&& build_predicate)
    -> diag::Result<mir::Stmt> {
  std::optional<mir::Block> tail = std::move(default_scope);

  for (std::size_t i = item_count; i-- > 1;) {
    mir::Block level_block;
    const WalkFrame level_frame = frame.WithBlock(&level_block).Deeper();
    auto pred_or = std::forward<PredicateBuilder>(build_predicate)(
        level_frame, i, static_cast<std::uint32_t>(i));
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
        mir::Stmt{
            .label = std::nullopt,
            .data = mir::IfStmt{
                .condition = *pred_or,
                .then_scope = body_scope_id,
                .else_scope = else_scope_id}});

    tail = std::move(level_block);
  }

  if (item_count > 0) {
    const WalkFrame wrapper_frame = frame.WithBlock(&wrapper_block).Deeper();
    auto pred0_or =
        std::forward<PredicateBuilder>(build_predicate)(wrapper_frame, 0, 0);
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
        mir::Stmt{
            .label = std::nullopt,
            .data = mir::IfStmt{
                .condition = *pred0_or,
                .then_scope = body0_id,
                .else_scope = else0_id}});
  } else if (tail.has_value()) {
    const mir::BlockId def_id =
        wrapper_block.child_scopes.Add(std::move(*tail));
    wrapper_block.AppendStmt(
        mir::Stmt{
            .label = std::nullopt, .data = mir::BlockStmt{.scope = def_id}});
  }

  const mir::BlockId wrapper_scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper_block));

  return mir::Stmt{
      .label = outer_label, .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

}  // namespace lyra::lowering::hir_to_mir
