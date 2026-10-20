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
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/arm_chain.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"
#include "lyra/lowering/hir_to_mir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/qualified_statement_check.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

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
      return at.current_block->ReadLocal(kept[arm], bit);
    };
  }

  auto chain_or = BuildArmChain(
      unit, wrapper_frame, arm_count, test_of, body_of,
      std::move(fall_through));
  if (!chain_or) return std::unexpected(std::move(chain_or.error()));
  wrapper.AppendStmt(*std::move(chain_or));
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
// stopping at the first that does. LRM 12.5 and 12.5.1 test a label by
// comparing it as the case keyword compares two values, and LRM 12.5.4 by set
// membership instead, which is the whole difference between the forms.
auto LowerCaseStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CaseStmt& c, diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block wrapper;
  auto subject_or =
      SnapshotCaseSubject(process, frame.WithBlock(&wrapper), c.condition);
  if (!subject_or) return std::unexpected(std::move(subject_or.error()));
  const CaseSubject subject = *subject_or;

  const auto test_label =
      [&](const WalkFrame& at,
          hir::ExprId label_expr) -> diag::Result<mir::ExprId> {
    mir::Block& block = *at.current_block;
    return std::visit(
        Overloaded{
            // The selector and the label are compared where they lie, which a
            // structure's own method borrows them at.
            [&](hir::CaseKeyword keyword) -> diag::Result<mir::ExprId> {
              const mir::ExprId selector =
                  block.LocalPlace(subject.local, subject.type);
              auto written = process.LowerAsPlace(
                  process.HirBody().exprs.Get(label_expr), at);
              if (!written) return std::unexpected(std::move(written.error()));
              return BuildCaseComparison(
                  unit, block, keyword, selector,
                  OperandAtHandleType(
                      unit, block, block.exprs.Add(*std::move(written)),
                      subject.type));
            },
            [&](hir::CaseInside) -> diag::Result<mir::ExprId> {
              return BuildSetMemberTest(
                  process, at, block.ReadLocal(subject.local, subject.type),
                  label_expr, unit.builtins.bit1);
            }},
        c.label_test);
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
        return test_label(point, label_expr);
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
        c.items[item].pattern, c.keyword));
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
