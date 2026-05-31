#pragma once

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/procedural_scope_helpers.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// References to the snapshot var produced by AppendCaseSnapshot. Predicate
// builders use these to compare `_lyra_case_sel` against each item's labels.
struct CaseSnapshotRefs {
  mir::ProceduralVarId sel_var;
  mir::TypeId sel_type;
};

// Appends the SV-case selector snapshot to wrapper_state:
//   T _lyra_case_sel;        // ProceduralVarDeclStmt (no init)
//   _lyra_case_sel = <cond>; // ExprStmt(AssignExpr)
//
// Caller has already lowered the selector into wrapper_state's expression
// table and passes its ExprId. The decl + assign are split (instead of a
// single var-decl-with-init) so the cpp backend's packed-init gap does not
// bite when the selector unifies to a packed-explicit type.
auto AppendCaseSnapshot(
    ProceduralScopeLoweringState& wrapper_state, mir::ExprId cond_expr_id)
    -> CaseSnapshotRefs;

// Builds (sel <op> L_0) || (sel <op> L_1) || ... into enc_state and returns
// the final predicate ExprId. `compare_op` selects the per-label primitive
// (kEquality for plain case / case-inside-value items, kCasezEquality /
// kCasexEquality for casez / casex). `lower_label(enc, label_idx)` lowers one
// label into `enc` and returns its ExprId. label_count must be >= 1.
template <typename LabelLowerer>
auto BuildEqualityChain(
    ProceduralScopeLoweringState& enc_state, CaseSnapshotRefs snapshot,
    mir::TypeId bit_type, mir::BinaryOp compare_op, std::uint32_t sel_hops,
    std::size_t label_count, LabelLowerer&& lower_label)
    -> diag::Result<mir::ExprId> {
  std::optional<mir::ExprId> acc;
  for (std::size_t i = 0; i < label_count; ++i) {
    auto label_or = std::forward<LabelLowerer>(lower_label)(enc_state, i);
    if (!label_or) {
      return std::unexpected(std::move(label_or.error()));
    }
    const mir::ExprId label_id = *label_or;
    const mir::ExprId sel_ref = enc_state.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = sel_hops},
                    .var = snapshot.sel_var},
            .type = snapshot.sel_type});
    const mir::ExprId cmp = enc_state.AddExpr(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = compare_op, .lhs = sel_ref, .rhs = label_id},
            .type = bit_type});
    if (acc.has_value()) {
      acc = enc_state.AddExpr(
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

// Assembles the SV-case if/else-if cascade. wrapper_state already holds the
// snapshot decl + assign (from AppendCaseSnapshot); it is moved in. body_scopes
// and default_scope are pre-lowered ProceduralScopes (caller is responsible for
// any depth bookkeeping required during their lowering).
//
// build_predicate(enc, item_idx, sel_hops) is invoked once per item from
// innermost (item_count - 1) to outermost (0). The enc scope is wrapper_state
// for the outermost item and a fresh intermediate scope (the prior level's
// else_scope contents) for the rest. sel_hops gives the hop count from enc to
// the scope where the snapshot var was declared.
//
// Returns a BlockStmt wrapping the snapshot + cascade.
template <typename PredicateBuilder>
auto BuildCaseCascade(
    ProceduralScopeLoweringState wrapper_state,
    std::optional<std::string> outer_label, std::size_t item_count,
    std::vector<mir::ProceduralScope> body_scopes,
    std::optional<mir::ProceduralScope> default_scope,
    PredicateBuilder&& build_predicate) -> diag::Result<mir::Stmt> {
  std::optional<mir::ProceduralScope> tail = std::move(default_scope);

  for (std::size_t i = item_count; i-- > 1;) {
    ProceduralScopeLoweringState level_state;
    auto pred_or = std::forward<PredicateBuilder>(build_predicate)(
        level_state, i, static_cast<std::uint32_t>(i));
    if (!pred_or) {
      return std::unexpected(std::move(pred_or.error()));
    }

    std::vector<mir::ProceduralScope> if_child_scopes;
    const mir::ProceduralScopeId body_scope_id =
        AddChildProceduralScope(if_child_scopes, std::move(body_scopes[i]));
    std::optional<mir::ProceduralScopeId> else_scope_id;
    if (tail.has_value()) {
      else_scope_id =
          AddChildProceduralScope(if_child_scopes, std::move(*tail));
    }

    const mir::StmtId if_id = level_state.AddStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data =
                mir::IfStmt{
                    .condition = *pred_or,
                    .then_scope = body_scope_id,
                    .else_scope = else_scope_id},
            .child_procedural_scopes = std::move(if_child_scopes)});
    level_state.AddRootStmt(if_id);

    tail = level_state.Finish();
  }

  if (item_count > 0) {
    auto pred0_or =
        std::forward<PredicateBuilder>(build_predicate)(wrapper_state, 0, 0);
    if (!pred0_or) {
      return std::unexpected(std::move(pred0_or.error()));
    }

    std::vector<mir::ProceduralScope> if0_child_scopes;
    const mir::ProceduralScopeId body0_id =
        AddChildProceduralScope(if0_child_scopes, std::move(body_scopes[0]));
    std::optional<mir::ProceduralScopeId> else0_id;
    if (tail.has_value()) {
      else0_id = AddChildProceduralScope(if0_child_scopes, std::move(*tail));
    }

    const mir::StmtId if0_id = wrapper_state.AddStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data =
                mir::IfStmt{
                    .condition = *pred0_or,
                    .then_scope = body0_id,
                    .else_scope = else0_id},
            .child_procedural_scopes = std::move(if0_child_scopes)});
    wrapper_state.AddRootStmt(if0_id);
  } else if (tail.has_value()) {
    std::vector<mir::ProceduralScope> def_child_scopes;
    const mir::ProceduralScopeId def_id =
        AddChildProceduralScope(def_child_scopes, std::move(*tail));
    const mir::StmtId def_block_id = wrapper_state.AddStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data = mir::BlockStmt{.scope = def_id},
            .child_procedural_scopes = std::move(def_child_scopes)});
    wrapper_state.AddRootStmt(def_block_id);
  }

  std::vector<mir::ProceduralScope> outer_child_scopes;
  const mir::ProceduralScopeId wrapper_scope_id =
      AddChildProceduralScope(outer_child_scopes, wrapper_state.Finish());

  return mir::Stmt{
      .label = outer_label,
      .data = mir::BlockStmt{.scope = wrapper_scope_id},
      .child_procedural_scopes = std::move(outer_child_scopes)};
}

}  // namespace lyra::lowering::hir_to_mir
