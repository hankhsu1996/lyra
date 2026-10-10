#include "lyra/lowering/hir_to_mir/statement/branches.hpp"

#include <cstddef>
#include <expected>
#include <functional>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"
#include "lyra/lowering/hir_to_mir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/qualified_statement_check.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/struct_methods.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// What one arm of a branching statement contributes, lowered into the frame it
// is handed: whether the arm is taken, and the statement it runs.
using ArmTest =
    std::function<diag::Result<mir::ExprId>(const WalkFrame&, std::size_t)>;
using ArmBody =
    std::function<diag::Result<mir::Block>(const WalkFrame&, std::size_t)>;

// A series of arms tried in order, the first whose test holds ending it and
// `fall_through` running where none does:
//
//   if (test 0) body 0 else { if (test 1) body 1 else { ... fall_through } }
//
// Each arm is the `else` of the one before it, and an `else` holds its `if` and
// nothing beside it, so a series of any length is one flat run. An arm's test
// is lowered before its statement, since a test may introduce what the
// statement names. Answers with the first arm, which stands in `frame`'s block.
auto BuildArmChain(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::size_t arm_count, const ArmTest& test_of, const ArmBody& body_of,
    std::optional<mir::Block> fall_through) -> diag::Result<mir::IfStmt> {
  if (arm_count == 0) {
    throw InternalError("BuildArmChain: a series has no arms");
  }
  const auto arm_at =
      [&](const WalkFrame& at, std::size_t arm,
          std::optional<mir::Block> otherwise) -> diag::Result<mir::IfStmt> {
    mir::Block& block = *at.current_block;
    auto held_or = test_of(at, arm);
    if (!held_or) return std::unexpected(std::move(held_or.error()));
    auto body_or = body_of(at, arm);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    std::optional<mir::BlockId> else_scope;
    if (otherwise.has_value()) {
      else_scope = block.child_scopes.Add(*std::move(otherwise));
    }
    return mir::IfStmt{
        .condition = ReduceToCondition(unit, block, *held_or),
        .then_scope = block.child_scopes.Add(std::move(*body_or)),
        .else_scope = else_scope};
  };

  std::optional<mir::Block> tail = std::move(fall_through);
  for (std::size_t arm = arm_count; arm-- > 1;) {
    mir::Block level;
    auto arm_or = arm_at(frame.WithBlock(&level), arm, std::move(tail));
    if (!arm_or) return std::unexpected(std::move(arm_or.error()));
    level.AppendStmt(*std::move(arm_or));
    tail = std::move(level);
  }
  return arm_at(frame, 0, std::move(tail));
}

// A statement or nothing, lowered as the arm a branching statement runs when
// none of its own held: an `else`, a `default`.
auto LowerCatchAll(
    ProcessLowerer& process, const WalkFrame& frame,
    std::optional<hir::StmtId> stmt)
    -> diag::Result<std::optional<mir::Block>> {
  if (!stmt.has_value()) {
    return std::optional<mir::Block>{};
  }
  auto lowered = LowerStmtIntoChildScope(process, frame, *stmt);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return std::optional<mir::Block>{std::move(*lowered)};
}

// A branching statement that needs steps of its own ahead of its arms, which
// `wrapper` holds and the statement is: a case statement's subject, evaluated
// once (LRM 12.5), and every arm's answer where the qualifier asserts
// uniqueness. Deciding whether two arms held cannot stop at the first that
// did, so such a statement evaluates every arm's test up front and then
// selects by the answers it kept (LRM 12.4.2, 12.5.3). A qualifier asserting
// only totality, or nothing at all, leaves the search stopping at the first
// arm that holds.
auto BuildWrappedBranching(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block& wrapper,
    std::optional<std::string> label,
    std::optional<hir::UniquePriorityCheck> check, QualifiedArmKind arm_kind,
    std::size_t arm_count, ArmTest test_of, const ArmBody& body_of,
    std::optional<hir::StmtId> catch_all, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  UnitLowerer& owner = process.Owner();
  const mir::CompilationUnit& unit = owner.Unit();
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  auto catch_all_or = LowerCatchAll(process, wrapper_frame, catch_all);
  if (!catch_all_or) return std::unexpected(std::move(catch_all_or.error()));
  const bool asserts_uniqueness =
      AssertionsOf(check, catch_all_or->has_value()).uniqueness;
  std::optional<mir::Block> fall_through = BuildFallThrough(
      owner, wrapper_frame, std::move(*catch_all_or), check, arm_kind, span);

  if (asserts_uniqueness) {
    std::vector<mir::ExprId> held;
    held.reserve(arm_count);
    for (std::size_t arm = 0; arm < arm_count; ++arm) {
      auto held_or = test_of(wrapper_frame, arm);
      if (!held_or) return std::unexpected(std::move(held_or.error()));
      held.push_back(*held_or);
    }
    test_of = [kept = BuildUniquenessCheck(
                   owner, wrapper_frame, held, *check, arm_kind, span),
               bit = unit.builtins.bit1](
                  const WalkFrame& at,
                  std::size_t arm) -> diag::Result<mir::ExprId> {
      return at.current_block->exprs.Add(mir::MakeLocalRefExpr(kept[arm], bit));
    };
  }

  // No arms is the ordinary shape of `case (x) default: ...`, whose only arm
  // the front end reports apart from the items.
  if (arm_count > 0) {
    auto chain_or = BuildArmChain(
        unit, wrapper_frame, arm_count, test_of, body_of,
        std::move(fall_through));
    if (!chain_or) return std::unexpected(std::move(chain_or.error()));
    wrapper.AppendStmt(*std::move(chain_or));
  } else if (fall_through.has_value()) {
    wrapper.AppendStmt(
        mir::BlockStmt{
            .scope = wrapper.child_scopes.Add(*std::move(fall_through))});
  }
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{
          .scope = frame.current_block->child_scopes.Add(std::move(wrapper))}};
}

// An if-else-if series: its arms in order, each the `else if` of the one
// before, and the `else` that ends it. A nested `if` with a qualifier or a
// label of its own is a statement in its own right, so it ends the series as
// its `else`. A qualifier governs the whole series rather than the one `if`
// that carries it (LRM 12.4.2), so both the arms it checks and the `else` that
// discharges its totality assertion are read off the series.
struct IfSeries {
  std::vector<const hir::IfStmt*> arms;
  std::optional<hir::StmtId> else_arm;
};

auto SeriesOf(const hir::ProceduralBody& body, const hir::IfStmt& root)
    -> IfSeries {
  IfSeries series{.arms = {&root}, .else_arm = root.else_stmt};
  while (series.else_arm.has_value()) {
    const hir::Stmt& stmt = body.stmts.Get(*series.else_arm);
    const auto* nested = std::get_if<hir::IfStmt>(&stmt.data);
    if (nested == nullptr || nested->check.has_value() ||
        stmt.label.has_value()) {
      break;
    }
    series.arms.push_back(nested);
    series.else_arm = nested->else_stmt;
  }
  return series;
}

// The local a case statement's subject was evaluated into once, which every
// item's test reads (LRM 12.5).
struct CaseSubject {
  mir::LocalId local;
  mir::TypeId type;
};

auto SnapshotCaseSubject(
    ProcessLowerer& process, const WalkFrame& wrapper_frame,
    hir::ExprId subject) -> diag::Result<CaseSubject> {
  mir::Block& wrapper = *wrapper_frame.current_block;
  auto lowered =
      process.LowerExpr(process.HirBody().exprs.Get(subject), wrapper_frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  const mir::ExprId value = wrapper.exprs.Add(*std::move(lowered));
  const mir::TypeId type = wrapper.exprs.Get(value).type;
  return CaseSubject{
      .local = SnapshotExprToLocal(
          process.Owner(), wrapper_frame, wrapper, type, value),
      .type = type};
}

// The test of one case label. Every condition compares its labels exactly: an
// x or a z stands for itself and matches itself, where logical equality would
// answer x and send the whole statement to its default arm (LRM 12.5). Which
// bits are do-not-care is the condition's own business, so each form has its
// own question rather than an operator a target applies. The membership
// condition (LRM 12.5.4) tests its labels a different way and never asks.
auto BuildLabelTest(
    const mir::CompilationUnit& unit, mir::Block& block,
    hir::CaseCondition condition, mir::ExprId selector, mir::ExprId label)
    -> mir::ExprId {
  const auto entry = [&](support::BuiltinFn fn) {
    return block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = fn, .receiver = selector},
                    .arguments = {label}},
            .type = unit.builtins.bit1});
  };
  switch (condition) {
    case hir::CaseCondition::kNormal:
      return BuildCaseEquality(unit, block, selector, label);
    case hir::CaseCondition::kWildcardJustZ:
      return entry(support::BuiltinFn::kCasezEquals);
    case hir::CaseCondition::kWildcardXOrZ:
      return entry(support::BuiltinFn::kCasexEquals);
    case hir::CaseCondition::kInside:
      break;
  }
  throw InternalError(
      "BuildLabelTest: condition tests its labels by membership");
}

}  // namespace

// LRM 12.4: the arms are tested in order and the first that holds is taken, an
// arm holding when its predicate is a known nonzero value. The identifiers an
// arm's patterns introduce are declared ahead of the series, in the block the
// series stands in.
auto LowerIfStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::IfStmt& i, diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  const IfSeries series = SeriesOf(process.HirBody(), i);
  const auto test_declaring_in = [&](const WalkFrame& declared_in) -> ArmTest {
    return
        [&process, &series, declared_in](const WalkFrame& at, std::size_t arm) {
          return ClauseSeriesPredicate(
                     process, declared_in, series.arms[arm]->conditions)
              .evaluate(at);
        };
  };
  const ArmBody body_of = [&](const WalkFrame& at, std::size_t arm) {
    return LowerStmtIntoChildScope(process, at, series.arms[arm]->then_stmt);
  };

  if (AssertionsOf(i.check, series.else_arm.has_value()).uniqueness) {
    mir::Block wrapper;
    return BuildWrappedBranching(
        process, frame, wrapper, std::move(label), i.check,
        QualifiedArmKind::kCondition, series.arms.size(),
        test_declaring_in(frame.WithBlock(&wrapper)), body_of, series.else_arm,
        span);
  }

  auto else_or = LowerCatchAll(process, frame, series.else_arm);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  auto chain_or = BuildArmChain(
      process.Owner().Unit(), frame, series.arms.size(),
      test_declaring_in(frame), body_of,
      BuildFallThrough(
          process.Owner(), frame, std::move(*else_or), i.check,
          QualifiedArmKind::kCondition, span));
  if (!chain_or) return std::unexpected(std::move(chain_or.error()));
  return mir::Stmt{.label = std::move(label), .data = *std::move(chain_or)};
}

// LRM 12.5: the subject is evaluated once, the items are searched in order,
// and an item is selected when any of its labels matches, that search too
// stopping at the first that does. LRM 12.5 and 12.5.1 test a label with an
// equality primitive -- exact, or one of the two do-not-care forms -- and LRM
// 12.5.4 with set membership instead, which is the whole difference between the
// four case conditions.
auto LowerCaseStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CaseStmt& c, diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block wrapper;
  auto subject_or =
      SnapshotCaseSubject(process, frame.WithBlock(&wrapper), c.condition);
  if (!subject_or) return std::unexpected(std::move(subject_or.error()));
  const CaseSubject subject = *subject_or;

  const auto label_test =
      [&](const WalkFrame& at,
          hir::ExprId label_expr) -> diag::Result<mir::ExprId> {
    mir::Block& block = *at.current_block;
    const mir::ExprId selector =
        block.exprs.Add(mir::MakeLocalRefExpr(subject.local, subject.type));
    if (c.condition_kind == hir::CaseCondition::kInside) {
      return BuildSetMemberTest(
          process, at, selector, label_expr, unit.builtins.bit1);
    }
    auto written =
        process.LowerExpr(process.HirBody().exprs.Get(label_expr), at);
    if (!written) return std::unexpected(std::move(written.error()));
    return BuildLabelTest(
        unit, block, c.condition_kind, selector,
        OperandAtHandleType(
            unit, block, block.exprs.Add(*std::move(written)), subject.type));
  };
  const ArmTest item_test = [&](const WalkFrame& at,
                                std::size_t item) -> diag::Result<mir::ExprId> {
    const std::vector<hir::ExprId>& labels = c.items[item].labels;
    if (labels.empty()) {
      throw InternalError("LowerCaseStmt: case item has no labels");
    }
    // The search ends at the first label that matches, so each label after
    // the first is evaluated only on the runs the ones before it missed.
    std::vector<mir::ExprId> tests;
    tests.reserve(labels.size());
    for (const hir::ExprId label_expr : labels) {
      const Evaluation test = [&](const WalkFrame& point) {
        return label_test(point, label_expr);
      };
      auto test_or =
          tests.empty() ? test(at) : ConditionallyEvaluated(at, test);
      if (!test_or) return std::unexpected(std::move(test_or.error()));
      tests.push_back(*test_or);
    }
    return AnyHolds(unit, *at.current_block, tests);
  };

  return BuildWrappedBranching(
      process, frame, wrapper, std::move(label), c.check,
      QualifiedArmKind::kCaseItem, c.items.size(), item_test,
      [&](const WalkFrame& at, std::size_t item) {
        return LowerStmtIntoChildScope(process, at, c.items[item].stmt);
      },
      c.default_stmt, span);
}

// LRM 12.6.1: an item is selected when its pattern matches the subject -- its
// identifiers then assigned -- and then its filter holds, so an item is the
// series of those two and the filter reads what the pattern bound. The
// identifiers are declared in the wrapper, where the filter and the item's
// statement both reach them. A qualifier applies as it does to an ordinary
// case statement.
auto LowerPatternCaseStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::PatternCaseStmt& c, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);
  auto subject_or = SnapshotCaseSubject(process, wrapper_frame, c.condition);
  if (!subject_or) return std::unexpected(std::move(subject_or.error()));
  const CaseSubject subject = *subject_or;

  const ArmTest item_test = [&](const WalkFrame& at, std::size_t item) {
    std::vector<Predicate> terms;
    terms.push_back(PatternPredicate(
        process, wrapper_frame, subject.local, subject.type,
        c.items[item].pattern));
    if (c.items[item].filter.has_value()) {
      terms.push_back(ExpressionPredicate(process, *c.items[item].filter));
    }
    return SeriesPredicate(unit, std::move(terms)).evaluate(at);
  };

  return BuildWrappedBranching(
      process, frame, wrapper, std::move(label), c.check,
      QualifiedArmKind::kCaseItem, c.items.size(), item_test,
      [&](const WalkFrame& at, std::size_t item) {
        return LowerStmtIntoChildScope(process, at, c.items[item].stmt);
      },
      c.default_stmt, span);
}

}  // namespace lyra::lowering::hir_to_mir
