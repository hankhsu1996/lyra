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
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/case_cascade.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/qualified_statement_check.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The equality primitive one case label is tested with. Every condition
// compares its labels exactly: an x or a z stands for itself and matches
// itself, where logical equality would answer x and send the whole statement
// to its default arm (LRM 12.5). The membership condition (LRM 12.5.4) tests
// its labels a different way and never asks.
auto CaseCompareOp(hir::CaseCondition condition) -> mir::BinaryOp {
  switch (condition) {
    case hir::CaseCondition::kNormal:
      return mir::BinaryOp::kCaseEquality;
    case hir::CaseCondition::kWildcardJustZ:
      return mir::BinaryOp::kCasezEquality;
    case hir::CaseCondition::kWildcardXOrZ:
      return mir::BinaryOp::kCasexEquality;
    case hir::CaseCondition::kInside:
      break;
  }
  throw InternalError("CaseCompareOp: condition has no equality primitive");
}

// Turns pre-lowered item bodies into the statement that selects among them.
// The linear search terminates at the first matching item (LRM 12.5), and a
// qualifier overrides that only where it asserts uniqueness: deciding whether
// two items matched cannot stop at the first that did, so those evaluate every
// item up front and dispatch on the resulting values (LRM 12.5.3). A qualifier
// asserting only totality, or nothing at all, leaves the search as it is.
template <typename PredicateBuilder>
auto BuildCaseSelection(
    ProcessLowerer& process, WalkFrame frame, mir::Block wrapper,
    std::optional<std::string> label, std::vector<mir::Block> body_scopes,
    std::optional<mir::Block> default_scope,
    std::optional<hir::UniquePriorityCheck> check, diag::SourceSpan span,
    const PredicateBuilder& build_predicate) -> diag::Result<mir::Stmt> {
  if (!check.has_value()) {
    return BuildCaseCascade(
        frame, std::move(wrapper), std::move(label), std::move(body_scopes),
        std::move(default_scope), process.Owner().Unit(), build_predicate);
  }

  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);
  const QualifiedAssertions asserts =
      AssertionsOf(*check, default_scope.has_value());

  // The arm reached when nothing matched is the statement's own default where
  // it has one, and the report totality owes where it has none. A statement
  // never wants both, because a default is what discharges that assertion.
  std::optional<mir::Block> fall_through;
  if (asserts.totality) {
    fall_through = BuildTotalityReportScope(
        process.Owner(), wrapper_frame, *check, QualifiedArmKind::kCaseItem,
        span);
  } else {
    fall_through = std::move(default_scope);
  }

  if (!asserts.uniqueness) {
    return BuildCaseCascade(
        frame, std::move(wrapper), std::move(label), std::move(body_scopes),
        std::move(fall_through), process.Owner().Unit(), build_predicate);
  }

  std::vector<QualifiedArm> arms;
  arms.reserve(body_scopes.size());
  for (std::size_t i = 0; i < body_scopes.size(); ++i) {
    auto pred_or = build_predicate(wrapper_frame, i);
    if (!pred_or) return std::unexpected(std::move(pred_or.error()));
    arms.push_back(
        QualifiedArm{.predicate = *pred_or, .body = std::move(body_scopes[i])});
  }
  return BuildUniquenessCheckCascade(
      process.Owner(), frame, std::move(wrapper), std::move(arms),
      std::move(fall_through), *check, QualifiedArmKind::kCaseItem,
      std::move(label), span);
}

// One arm of an if-else-if construct: the clause chain that decides it, the
// statement it runs, and the arm reached when the chain does not hold.
// `fall_through` is the source's own `else` where it wrote one, and the report
// a totality assertion owes where it did not.
auto LowerChainArm(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::IfStmt& arm, std::optional<mir::Block> fall_through)
    -> diag::Result<mir::Stmt> {
  const mir::TypeId bit1_type = process.Owner().Unit().builtins.bit1;
  auto& block = *frame.current_block;

  // The fall-through arm runs when the chain fails, which the chain reports
  // through a flag. Nothing observes the flag when there is no such arm, so it
  // is only declared alongside one.
  std::optional<mir::LocalId> taken_flag;
  if (fall_through.has_value()) {
    taken_flag = frame.bindings->DeclareAnonymous(
        mir::LocalDecl{.name = "_lyra_cond_taken", .type = bit1_type});
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = *taken_flag,
            .init = BuildBit1Literal(process.Owner().Unit(), block, false)});
  }

  auto emit_then = [&](WalkFrame body_frame) -> diag::Result<void> {
    auto then_or = LowerStmtIntoChildScope(process, body_frame, arm.then_stmt);
    if (!then_or) return std::unexpected(std::move(then_or.error()));
    auto& body_block = *body_frame.current_block;
    body_block.AppendStmt(
        mir::BlockStmt{
            .scope = body_block.child_scopes.Add(std::move(*then_or))});
    return {};
  };

  auto chain_or = BuildClauseChainIf(
      process, frame, std::span<const hir::ConditionClause>{arm.conditions},
      taken_flag, emit_then);
  if (!chain_or) return std::unexpected(std::move(chain_or.error()));
  const mir::IfStmt chain = *std::move(chain_or);

  if (!fall_through.has_value()) {
    return mir::Stmt{.label = std::move(label), .data = chain};
  }

  const mir::BlockId else_scope =
      block.child_scopes.Add(std::move(*fall_through));

  // The chain and its fall-through arm are two statements, so the label goes on
  // the second and the first is appended ahead of it.
  block.AppendStmt(chain);
  return mir::Stmt{
      .label = std::move(label),
      .data = BuildChainElseIf(
          process.Owner().Unit(), block, *taken_flag, bit1_type, else_scope)};
}

// A qualified series whose check reads no arm predicate. The arms are tested in
// order and the first that holds terminates the series (LRM 12.4.1), which is
// what the unqualified construct already does, so the series is built from its
// innermost arm outward with the fall-through arm at the bottom.
auto LowerOrderedIfSeries(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const QualifiedIfSeries& series, std::optional<mir::Block> fall_through)
    -> diag::Result<mir::Stmt> {
  std::optional<mir::Block> tail = std::move(fall_through);
  for (std::size_t i = series.arms.size(); i-- > 1;) {
    mir::Block level;
    const WalkFrame level_frame = frame.WithBlock(&level);
    auto arm_or = LowerChainArm(
        process, level_frame, std::nullopt, *series.arms[i], std::move(tail));
    if (!arm_or) return std::unexpected(std::move(arm_or.error()));
    level.AppendStmt(*std::move(arm_or));
    tail = std::move(level);
  }
  return LowerChainArm(
      process, frame, std::move(label), *series.arms[0], std::move(tail));
}

}  // namespace

auto LowerIfStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::IfStmt& i, diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  if (!i.check.has_value()) {
    std::optional<mir::Block> else_arm;
    if (i.else_stmt.has_value()) {
      auto else_or = LowerStmtIntoChildScope(process, frame, *i.else_stmt);
      if (!else_or) return std::unexpected(std::move(else_or.error()));
      else_arm = std::move(*else_or);
    }
    return LowerChainArm(
        process, frame, std::move(label), i, std::move(else_arm));
  }

  const QualifiedIfSeries series = SeriesOf(process.HirBody(), i);
  if (AssertionsOf(*i.check, series.else_arm.has_value()).uniqueness) {
    return LowerUniquenessIfSeries(
        process, frame, std::move(label), series, *i.check, span);
  }

  auto fall_through_or =
      LowerIfFallThrough(process, frame, series, *i.check, span);
  if (!fall_through_or) {
    return std::unexpected(std::move(fall_through_or.error()));
  }
  return LowerOrderedIfSeries(
      process, frame, std::move(label), series, std::move(*fall_through_or));
}

auto LowerCaseStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CaseStmt& c, diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  auto& unit = process.Owner().Unit();
  const mir::TypeId bit_type = unit.builtins.bit1;

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

  // LRM 12.5 / 12.5.1 test a label with an equality primitive -- exact, or one
  // of the two do-not-care forms; LRM 12.5.4 tests it with set membership
  // instead. That is the whole difference between the four case conditions.
  const auto build_label_predicate =
      [&](WalkFrame label_frame,
          hir::ExprId label) -> diag::Result<mir::ExprId> {
    auto& label_block = *label_frame.current_block;
    const mir::ExprId sel = label_block.exprs.Add(
        mir::MakeLocalRefExpr(snapshot.sel_var, snapshot.sel_type));
    if (c.condition_kind == hir::CaseCondition::kInside) {
      return BuildHirInsideItemPredicate(
          process, label_frame, sel, label, bit_type);
    }
    auto label_or = process.LowerExpr(hir_proc.exprs.Get(label), label_frame);
    if (!label_or) return std::unexpected(std::move(label_or.error()));
    return label_block.exprs.Add(BuildMirBinaryExpr(
        unit, label_block, CaseCompareOp(c.condition_kind), sel,
        label_block.exprs.Add(*std::move(label_or)), bit_type));
  };

  // An item is selected when any of its labels matches (LRM 12.5).
  const auto build_item_predicate =
      [&](WalkFrame level_frame,
          std::size_t item_idx) -> diag::Result<mir::ExprId> {
    const auto& labels = c.items[item_idx].labels;
    if (labels.empty()) {
      throw InternalError("LowerCaseStmt: case item has no labels");
    }
    auto& level_block = *level_frame.current_block;
    std::vector<mir::ExprId> tests;
    tests.reserve(labels.size());
    for (const hir::ExprId label : labels) {
      auto pred_or = build_label_predicate(level_frame, label);
      if (!pred_or) return std::unexpected(std::move(pred_or.error()));
      tests.push_back(*pred_or);
    }
    return BuildMirLogicalOr(unit, level_block, bit_type, tests);
  };

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

  return BuildCaseSelection(
      process, frame, std::move(wrapper), std::move(label),
      std::move(body_scopes), std::move(default_scope), c.check, span,
      build_item_predicate);
}

auto LowerPatternCaseStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::PatternCaseStmt& c, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  auto& unit = process.Owner().Unit();
  const mir::TypeId bit1_type = unit.builtins.bit1;

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  const hir::Expr& cond_hir = process.HirBody().exprs.Get(c.condition);
  const mir::TypeId subject_mir_type =
      process.Owner().TranslateType(cond_hir.type);

  auto cond_or = process.LowerExpr(cond_hir, wrapper_frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_expr_id = wrapper.exprs.Add(*std::move(cond_or));
  const CaseSnapshotRefs snapshot =
      AppendCaseSnapshot(process.Owner(), wrapper_frame, cond_expr_id);

  std::vector<mir::Block> body_scopes;
  body_scopes.reserve(c.items.size());

  for (const auto& item : c.items) {
    mir::Block body_block;
    const WalkFrame body_frame = wrapper_frame.WithBlock(&body_block);
    const mir::ExprId subject_ref = body_block.exprs.Add(
        mir::MakeLocalRefExpr(snapshot.sel_var, subject_mir_type));

    // An item's scope is entered only once its pattern has matched, so the
    // identifiers it binds both live and are assigned there, ahead of the
    // body that names them. The predicate that decides the match is built
    // later, once, by the selection below.
    EmitPatternBindings(
        process, body_frame, body_frame, subject_ref, item.pattern);

    auto user_body_or = LowerStmtIntoChildScope(process, body_frame, item.stmt);
    if (!user_body_or) {
      return std::unexpected(std::move(user_body_or.error()));
    }
    const mir::BlockId user_body_id =
        body_block.child_scopes.Add(std::move(*user_body_or));
    body_block.AppendStmt(mir::BlockStmt{.scope = user_body_id});
    body_scopes.push_back(std::move(body_block));
  }

  std::optional<mir::Block> default_scope;
  if (c.default_stmt.has_value()) {
    auto def_or =
        LowerStmtIntoChildScope(process, wrapper_frame, *c.default_stmt);
    if (!def_or) return std::unexpected(std::move(def_or.error()));
    default_scope = std::move(*def_or);
  }

  const auto build_predicate =
      [&](WalkFrame level_frame,
          std::size_t item_idx) -> diag::Result<mir::ExprId> {
    auto& level_block = *level_frame.current_block;
    const auto& item = c.items[item_idx];
    const mir::ExprId subject_ref = level_block.exprs.Add(
        mir::MakeLocalRefExpr(snapshot.sel_var, subject_mir_type));
    auto match_or =
        BuildPatternPredicate(process, level_frame, subject_ref, item.pattern);
    if (!match_or) return std::unexpected(std::move(match_or.error()));

    // LRM 12.6.1: an item is selected when its pattern matches and its filter
    // holds. Either may be absent, and an item that constrains nothing is the
    // empty conjunction -- it always selects.
    std::vector<mir::ExprId> tests;
    if (match_or->has_value()) tests.push_back(**match_or);
    if (item.filter.has_value()) {
      auto filter_or = process.LowerExpr(
          process.HirBody().exprs.Get(*item.filter), level_frame);
      if (!filter_or) return std::unexpected(std::move(filter_or.error()));
      tests.push_back(level_block.exprs.Add(*std::move(filter_or)));
    }
    return BuildMirLogicalAnd(unit, level_block, bit1_type, tests);
  };

  // LRM 12.6.1: `unique` / `unique0` / `priority` apply to a pattern-matching
  // case as they do to an ordinary one -- every item tests the same subject
  // snapshot with no cross-item dependency, so the qualified shape needs
  // nothing a pattern item cannot supply.
  return BuildCaseSelection(
      process, frame, std::move(wrapper), std::move(label),
      std::move(body_scopes), std::move(default_scope), c.check, span,
      build_predicate);
}

}  // namespace lyra::lowering::hir_to_mir
