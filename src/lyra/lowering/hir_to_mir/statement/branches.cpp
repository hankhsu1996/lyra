#include "lyra/lowering/hir_to_mir/statement/branches.hpp"

#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/case_cascade.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/deferred_check_cascade.hpp"
#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerIfStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::IfStmt& i, diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  if (i.check.has_value()) {
    return LowerUniqueIfStmt(process, frame, std::move(label), i, span);
  }
  auto& block = *frame.current_block;
  auto cond_expr_or =
      process.LowerExpr(process.HirBody().exprs.Get(i.condition), frame);
  if (!cond_expr_or) {
    return std::unexpected(std::move(cond_expr_or.error()));
  }
  const mir::ExprId cond_id = ReduceToCondition(
      block, block.exprs.Add(*std::move(cond_expr_or)),
      process.Owner().Unit().builtins.bit1);

  auto then_or = LowerStmtIntoChildScope(process, frame, i.then_stmt);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::BlockId then_scope_id =
      frame.current_block->child_scopes.Add(std::move(*then_or));

  std::optional<mir::BlockId> else_scope_id;
  if (i.else_stmt.has_value()) {
    auto else_or = LowerStmtIntoChildScope(process, frame, *i.else_stmt);
    if (!else_or) return std::unexpected(std::move(else_or.error()));
    else_scope_id = frame.current_block->child_scopes.Add(std::move(*else_or));
  }

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::IfStmt{
          .condition = cond_id,
          .then_scope = then_scope_id,
          .else_scope = else_scope_id}};
}

auto LowerCaseStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CaseStmt& c, diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  const mir::TypeId bit_type = process.Owner().Unit().builtins.bit1;
  const mir::BinaryOp compare_op = [&] {
    switch (c.condition_kind) {
      case hir::CaseCondition::kNormal:
        return mir::BinaryOp::kEquality;
      case hir::CaseCondition::kWildcardJustZ:
        return mir::BinaryOp::kCasezEquality;
      case hir::CaseCondition::kWildcardXOrZ:
        return mir::BinaryOp::kCasexEquality;
    }
    throw InternalError("LowerCaseStmt: unknown hir::CaseCondition");
  }();

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  auto cond_or =
      process.LowerExpr(hir_proc.exprs.Get(c.condition), wrapper_frame);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_expr_id = wrapper.exprs.Add(*std::move(cond_or));

  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(process.Owner(), wrapper_frame, cond_expr_id);

  std::vector<mir::Block> body_scopes;
  body_scopes.reserve(c.items.size());
  for (const auto& item : c.items) {
    auto body_or = LowerStmtIntoChildScope(process, wrapper_frame, item.stmt);
    if (!body_or) {
      return std::unexpected(std::move(body_or.error()));
    }
    body_scopes.push_back(std::move(*body_or));
  }

  std::optional<mir::Block> default_scope;
  if (c.default_stmt.has_value()) {
    auto def_or =
        LowerStmtIntoChildScope(process, wrapper_frame, *c.default_stmt);
    if (!def_or) {
      return std::unexpected(std::move(def_or.error()));
    }
    default_scope = std::move(*def_or);
  }

  if (c.check.has_value()) {
    std::vector<mir::ExprId> predicates;
    predicates.reserve(c.items.size());
    for (const auto& item : c.items) {
      auto pred_or = BuildEqualityChain(
          wrapper_frame, snapshot, bit_type, compare_op, item.labels.size(),
          [&](WalkFrame label_frame,
              std::size_t li) -> diag::Result<mir::ExprId> {
            auto lab_or = process.LowerExpr(
                hir_proc.exprs.Get(item.labels[li]), label_frame);
            if (!lab_or) {
              return std::unexpected(std::move(lab_or.error()));
            }
            return label_frame.current_block->exprs.Add(*std::move(lab_or));
          },
          process.Owner().Unit());
      if (!pred_or) return std::unexpected(std::move(pred_or.error()));
      predicates.push_back(*pred_or);
    }

    std::vector<DeferredCheckBranch> branches;
    branches.reserve(c.items.size());
    for (std::size_t i = 0; i < c.items.size(); ++i) {
      branches.push_back(
          DeferredCheckBranch{
              .predicate = predicates[i], .body = std::move(body_scopes[i])});
    }
    return BuildDeferredCheckCascade(
        process.Owner(), frame, std::move(wrapper), std::move(branches),
        std::move(default_scope), *c.check, std::move(label), span);
  }

  auto build_predicate = [&](WalkFrame level_frame, std::size_t item_idx) {
    return BuildEqualityChain(
        level_frame, snapshot, bit_type, compare_op,
        c.items[item_idx].labels.size(),
        [&](WalkFrame label_frame,
            std::size_t li) -> diag::Result<mir::ExprId> {
          auto lab_or = process.LowerExpr(
              hir_proc.exprs.Get(c.items[item_idx].labels[li]), label_frame);
          if (!lab_or) {
            return std::unexpected(std::move(lab_or.error()));
          }
          return label_frame.current_block->exprs.Add(*std::move(lab_or));
        },
        process.Owner().Unit());
  };

  return BuildCaseCascade(
      frame, std::move(wrapper), std::move(label), c.items.size(),
      std::move(body_scopes), std::move(default_scope), bit_type,
      build_predicate);
}

auto LowerCaseInsideStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CaseInsideStmt& c, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  const mir::TypeId bit_type = process.Owner().Unit().builtins.bit1;

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  auto cond_or =
      process.LowerExpr(hir_proc.exprs.Get(c.condition), wrapper_frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_expr_id = wrapper.exprs.Add(*std::move(cond_or));

  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(process.Owner(), wrapper_frame, cond_expr_id);

  std::vector<mir::Block> body_scopes;
  body_scopes.reserve(c.items.size());
  for (const auto& item : c.items) {
    auto body_or = LowerStmtIntoChildScope(process, wrapper_frame, item.stmt);
    if (!body_or) {
      return std::unexpected(std::move(body_or.error()));
    }
    body_scopes.push_back(std::move(*body_or));
  }

  std::optional<mir::Block> default_scope;
  if (c.default_stmt.has_value()) {
    auto def_or =
        LowerStmtIntoChildScope(process, wrapper_frame, *c.default_stmt);
    if (!def_or) {
      return std::unexpected(std::move(def_or.error()));
    }
    default_scope = std::move(*def_or);
  }

  auto build_item_predicate =
      [&](WalkFrame level_frame,
          std::size_t item_idx) -> diag::Result<mir::ExprId> {
    auto& enc = *level_frame.current_block;
    const mir::ExprId sel_ref = enc.exprs.Add(
        mir::MakeLocalRefExpr(snapshot.sel_var, snapshot.sel_type));
    const auto& item = c.items[item_idx];
    if (item.items.empty()) {
      throw InternalError(
          "LowerCaseInsideStmt: case-inside item has empty range_list");
    }
    std::optional<mir::ExprId> acc;
    for (const auto& inside_item : item.items) {
      auto pred_or = BuildHirInsideItemPredicate(
          process, level_frame, sel_ref, inside_item, bit_type);
      if (!pred_or) return std::unexpected(std::move(pred_or.error()));
      if (acc.has_value()) {
        acc = enc.exprs.Add(
            mir::Expr{
                .data =
                    mir::BinaryExpr{
                        .op = mir::BinaryOp::kLogicalOr,
                        .lhs = *acc,
                        .rhs = *pred_or},
                .type = bit_type});
      } else {
        acc = *pred_or;
      }
    }
    return *acc;
  };

  if (c.check.has_value()) {
    std::vector<mir::ExprId> predicates;
    predicates.reserve(c.items.size());
    for (std::size_t i = 0; i < c.items.size(); ++i) {
      auto pred_or = build_item_predicate(wrapper_frame, i);
      if (!pred_or) return std::unexpected(std::move(pred_or.error()));
      predicates.push_back(*pred_or);
    }

    std::vector<DeferredCheckBranch> branches;
    branches.reserve(c.items.size());
    for (std::size_t i = 0; i < c.items.size(); ++i) {
      branches.push_back(
          DeferredCheckBranch{
              .predicate = predicates[i], .body = std::move(body_scopes[i])});
    }
    return BuildDeferredCheckCascade(
        process.Owner(), frame, std::move(wrapper), std::move(branches),
        std::move(default_scope), *c.check, std::move(label), span);
  }

  return BuildCaseCascade(
      frame, std::move(wrapper), std::move(label), c.items.size(),
      std::move(body_scopes), std::move(default_scope), bit_type,
      build_item_predicate);
}

}  // namespace lyra::lowering::hir_to_mir
