#include "lyra/lowering/hir_to_mir/statement/branches.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/case_cascade.hpp"
#include "lyra/lowering/hir_to_mir/deferred_check_cascade.hpp"
#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/cast.hpp"
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
  const mir::ExprId cond_id = block.exprs.Add(*std::move(cond_expr_or));

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
  const mir::TypeId bit_type = process.Module().Unit().builtins.bit1;
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
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper).Deeper();

  auto cond_or =
      process.LowerExpr(hir_proc.exprs.Get(c.condition), wrapper_frame);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  mir::ExprId cond_expr_id = wrapper.exprs.Add(*std::move(cond_or));

  // Peel an outer CastExpr widening the selector when state-kind is
  // preserved -- the cpp backend cannot init form=explicit from form=int,
  // and the cascade builds its own per-label compares. Differing state-kind
  // would make the snapshot 2-state while wildcard labels are 4-state and
  // PackedArray::ExpectSameShape rejects the per-label compare.
  auto packed_state = [&](mir::TypeId tid) -> std::optional<bool> {
    const auto& ty = process.Module().Unit().GetType(tid);
    if (const auto* pa = std::get_if<mir::PackedArrayType>(&ty.data)) {
      return pa->IsFourState();
    }
    if (const auto* en = std::get_if<mir::EnumType>(&ty.data)) {
      return en->base.IsFourState();
    }
    return std::nullopt;
  };
  if (const auto* cv =
          std::get_if<mir::CastExpr>(&wrapper.exprs.Get(cond_expr_id).data)) {
    const mir::TypeId dst_tid = wrapper.exprs.Get(cond_expr_id).type;
    const mir::TypeId src_tid = wrapper.exprs.Get(cv->operand).type;
    const auto dst_state = packed_state(dst_tid);
    const auto src_state = packed_state(src_tid);
    if (dst_state.has_value() && src_state.has_value() &&
        *dst_state == *src_state) {
      cond_expr_id = cv->operand;
    }
  }

  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(process.Module(), wrapper_frame, cond_expr_id);

  std::vector<mir::Block> body_scopes;
  body_scopes.reserve(c.items.size());
  for (std::size_t i = 0; i < c.items.size(); ++i) {
    auto body_or = LowerStmtIntoChildScope(
        process, wrapper_frame.DeeperBy(i), c.items[i].stmt);
    if (!body_or) {
      return std::unexpected(std::move(body_or.error()));
    }
    body_scopes.push_back(std::move(*body_or));
  }

  // The cascade places the default at the same depth as the last item's body
  // (their if-then-else share an enclosing level scope), so the default
  // lowers at `items.size() - 1` extras; an empty item list is the cascade's
  // degenerate `if (true) default` form at the wrapper depth.
  const WalkFrame default_enter_frame =
      c.items.empty() ? wrapper_frame
                      : wrapper_frame.DeeperBy(c.items.size() - 1);
  std::optional<mir::Block> default_scope;
  if (c.default_stmt.has_value()) {
    auto def_or =
        LowerStmtIntoChildScope(process, default_enter_frame, *c.default_stmt);
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
          wrapper_frame, snapshot, bit_type, compare_op, 0, item.labels.size(),
          [&](WalkFrame label_frame,
              std::size_t li) -> diag::Result<mir::ExprId> {
            auto lab_or = process.LowerExpr(
                hir_proc.exprs.Get(item.labels[li]), label_frame);
            if (!lab_or) {
              return std::unexpected(std::move(lab_or.error()));
            }
            return label_frame.current_block->exprs.Add(*std::move(lab_or));
          },
          process.Module().Unit());
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
        process.Module(), frame, std::move(wrapper), std::move(branches),
        std::move(default_scope), *c.check, std::move(label), span);
  }

  auto build_predicate = [&](WalkFrame enc_frame, std::size_t item_idx,
                             std::uint32_t sel_hops) {
    const WalkFrame level_frame = enc_frame.DeeperBy(item_idx);
    return BuildEqualityChain(
        level_frame, snapshot, bit_type, compare_op, sel_hops,
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
        process.Module().Unit());
  };

  return BuildCaseCascade(
      frame, std::move(wrapper), std::move(label), c.items.size(),
      std::move(body_scopes), std::move(default_scope), build_predicate);
}

auto LowerCaseInsideStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CaseInsideStmt& c, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  const mir::TypeId bit_type = process.Module().Unit().builtins.bit1;

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper).Deeper();

  auto cond_or =
      process.LowerExpr(hir_proc.exprs.Get(c.condition), wrapper_frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  mir::ExprId cond_expr_id = wrapper.exprs.Add(*std::move(cond_or));

  if (const auto* cv =
          std::get_if<mir::CastExpr>(&wrapper.exprs.Get(cond_expr_id).data)) {
    cond_expr_id = cv->operand;
  }

  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(process.Module(), wrapper_frame, cond_expr_id);

  std::vector<mir::Block> body_scopes;
  body_scopes.reserve(c.items.size());
  for (std::size_t i = 0; i < c.items.size(); ++i) {
    auto body_or = LowerStmtIntoChildScope(
        process, wrapper_frame.DeeperBy(i), c.items[i].stmt);
    if (!body_or) {
      return std::unexpected(std::move(body_or.error()));
    }
    body_scopes.push_back(std::move(*body_or));
  }

  const WalkFrame default_enter_frame =
      c.items.empty() ? wrapper_frame
                      : wrapper_frame.DeeperBy(c.items.size() - 1);
  std::optional<mir::Block> default_scope;
  if (c.default_stmt.has_value()) {
    auto def_or =
        LowerStmtIntoChildScope(process, default_enter_frame, *c.default_stmt);
    if (!def_or) {
      return std::unexpected(std::move(def_or.error()));
    }
    default_scope = std::move(*def_or);
  }

  auto build_item_predicate =
      [&](WalkFrame enc_frame, std::size_t item_idx,
          std::uint32_t sel_hops) -> diag::Result<mir::ExprId> {
    const WalkFrame level_frame = enc_frame.DeeperBy(item_idx);
    auto& enc = *level_frame.current_block;
    const mir::ExprId sel_ref = enc.exprs.Add(
        mir::Expr{
            .data =
                mir::LocalRef{
                    .hops = mir::BlockHops{.value = sel_hops},
                    .var = snapshot.sel_var},
            .type = snapshot.sel_type});
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
      auto pred_or = build_item_predicate(wrapper_frame, i, 0);
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
        process.Module(), frame, std::move(wrapper), std::move(branches),
        std::move(default_scope), *c.check, std::move(label), span);
  }

  return BuildCaseCascade(
      frame, std::move(wrapper), std::move(label), c.items.size(),
      std::move(body_scopes), std::move(default_scope), build_item_predicate);
}

}  // namespace lyra::lowering::hir_to_mir
