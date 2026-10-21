#include "lyra/lowering/hir_to_mir/statement/branches.hpp"

#include <cstddef>
#include <cstdint>
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
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"
#include "lyra/lowering/hir_to_mir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/qualified_statement_check.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/switch_stmt.hpp"
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
// did, so such a statement evaluates every arm's conditions up front and then
// selects by the answers it kept (LRM 12.4.2, 12.5.3). A qualifier asserting
// only totality, or nothing at all, leaves the search stopping at the first
// arm that holds.
auto BuildWrappedBranching(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block& wrapper,
    std::optional<std::string> label,
    std::optional<hir::UniquePriorityCheck> check, QualifiedArmKind arm_kind,
    std::vector<SearchedArm> arms, std::optional<hir::StmtId> catch_all,
    diag::SourceSpan span) -> diag::Result<mir::Stmt> {
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
    held.reserve(arms.size());
    for (const SearchedArm& arm : arms) {
      const mir::LocalId any =
          DeclareLocal(wrapper_frame, BuildMachineBool(unit, wrapper, false));
      if (auto assigned =
              AssignAnyHoldsInOrder(unit, wrapper_frame, any, arm.conditions);
          !assigned) {
        return std::unexpected(std::move(assigned.error()));
      }
      held.push_back(wrapper.ReadLocal(any, unit.builtins.machine_bool));
    }
    const std::vector<mir::LocalId> kept = BuildUniquenessCheck(
        owner, wrapper_frame, held, *check, arm_kind, span);
    for (std::size_t arm = 0; arm < arms.size(); ++arm) {
      arms[arm].conditions = {
          [local = kept[arm], bit = unit.builtins.bit1](
              const WalkFrame& at) -> diag::Result<mir::ExprId> {
            return at.current_block->ReadLocal(local, bit);
          }};
    }
  }

  auto search_or =
      BuildArmSearch(unit, wrapper_frame, arms, std::move(fall_through));
  if (!search_or) return std::unexpected(std::move(search_or.error()));
  wrapper.AppendStmt(*std::move(search_or));
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

// Whether the items may be taken by a switch over the subject's value, which
// selects an item exactly as comparing its labels in order does where the
// keyword compares every bit (LRM 12.5; a `casez` or `casex` label leaves bits
// out, LRM 12.5.1) and no qualifier asks about the items together (LRM
// 12.5.3).
auto TakenByValue(
    hir::CaseKeyword keyword, std::optional<hir::UniquePriorityCheck> check)
    -> bool {
  switch (keyword) {
    case hir::CaseKeyword::kCase:
      return !check.has_value();
    case hir::CaseKeyword::kCasez:
    case hir::CaseKeyword::kCasex:
      return false;
  }
  throw InternalError("TakenByValue: unknown CaseKeyword");
}

// LRM 12.5 and 12.5.1: a label is compared with the subject as the keyword
// compares two values. Where the items may be taken by value and every label
// is a value of the subject's type known before the run, the items are a
// switch over the subject's value, entered only where the subject holds no x
// or z bit, each item leaving the block the switch stands in, so what runs
// where no item is taken is the default; otherwise the items are searched in
// order.
auto LowerComparedCase(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block& wrapper,
    std::optional<std::string> label, const hir::CaseStmt& c,
    hir::CaseKeyword keyword, const CaseSubject& subject, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  const auto label_of = [&](hir::ExprId label_expr) -> Evaluation {
    return [&, label_expr](const WalkFrame& at) -> diag::Result<mir::ExprId> {
      mir::Block& block = *at.current_block;
      auto written =
          process.LowerAsPlace(process.HirBody().exprs.Get(label_expr), at);
      if (!written) return std::unexpected(std::move(written.error()));
      return OperandAtHandleType(
          unit, block, block.exprs.Add(*std::move(written)), subject.type);
    };
  };
  const auto body_of = [&process](const hir::CaseItem& item) {
    return [&process, &item](const WalkFrame& at) {
      return LowerStmtIntoChildScope(process, at, item.stmt);
    };
  };

  std::vector<std::vector<Evaluation>> labels;
  labels.reserve(c.items.size());
  for (const hir::CaseItem& item : c.items) {
    std::vector<Evaluation>& of_item = labels.emplace_back();
    of_item.reserve(item.labels.size());
    for (const hir::ExprId label_expr : item.labels) {
      of_item.push_back(label_of(label_expr));
    }
  }
  std::optional<std::vector<std::vector<std::int64_t>>> values;
  if (TakenByValue(keyword, c.check)) {
    auto known = ValuesOfItems(unit, wrapper_frame, labels, subject.type);
    if (!known) return std::unexpected(std::move(known.error()));
    values = *std::move(known);
  }

  if (!values.has_value()) {
    std::vector<SearchedArm> items;
    items.reserve(c.items.size());
    for (std::size_t i = 0; i < c.items.size(); ++i) {
      SearchedArm lowering{.conditions = {}, .body = body_of(c.items[i])};
      for (const Evaluation& written : labels[i]) {
        // The subject and the label are compared where they lie, which a
        // structure's own method borrows them at.
        lowering.conditions.emplace_back(
            [&, written](const WalkFrame& at) -> diag::Result<mir::ExprId> {
              mir::Block& block = *at.current_block;
              auto lowered = written(at);
              if (!lowered) return lowered;
              return BuildCaseComparison(
                  unit, block, keyword,
                  block.LocalPlace(subject.local, subject.type), *lowered);
            });
      }
      items.push_back(std::move(lowering));
    }
    return BuildWrappedBranching(
        process, frame, wrapper, std::move(label), c.check,
        QualifiedArmKind::kCaseItem, std::move(items), c.default_stmt, span);
  }

  std::vector<ValueArm> cases;
  cases.reserve(c.items.size());
  for (std::size_t i = 0; i < c.items.size(); ++i) {
    cases.push_back(
        ValueArm{.values = (*values)[i], .body = body_of(c.items[i])});
  }
  auto default_or = LowerCatchAll(process, wrapper_frame, c.default_stmt);
  if (!default_or) return std::unexpected(std::move(default_or.error()));
  auto chosen = BuildCaseOverValues(
      unit, wrapper_frame, subject.local, subject.type, cases,
      *std::move(default_or));
  if (!chosen) return std::unexpected(std::move(chosen.error()));
  wrapper.AppendStmt(*std::move(chosen));
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{
          .scope = frame.current_block->child_scopes.Add(std::move(wrapper))}};
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

  if (AssertionsOf(i.check, series.else_arm.has_value()).uniqueness) {
    mir::Block wrapper;
    const WalkFrame declared_in = frame.WithBlock(&wrapper);
    std::vector<SearchedArm> arms;
    arms.reserve(series.arms.size());
    for (const hir::IfStmt* arm : series.arms) {
      arms.push_back(
          SearchedArm{
              .conditions = {[&process, arm, declared_in](const WalkFrame& at) {
                return ClauseSeriesPredicate(
                           process, declared_in, arm->conditions)
                    .evaluate(at);
              }},
              .body = [&process, arm](const WalkFrame& at) {
                return LowerStmtIntoChildScope(process, at, arm->then_stmt);
              }});
    }
    return BuildWrappedBranching(
        process, frame, wrapper, std::move(label), i.check,
        QualifiedArmKind::kCondition, std::move(arms), series.else_arm, span);
  }

  // Otherwise the series is built as the source nested it, each arm the
  // `else` of the one before, so it is only as deep as the front end lets a
  // source nest one. An arm's test is lowered inside that `else`, so it runs
  // only where every arm before it failed.
  const mir::CompilationUnit& unit = process.Owner().Unit();
  const auto arm_at =
      [&](const WalkFrame& at, std::size_t arm,
          std::optional<mir::Block> otherwise) -> diag::Result<mir::IfStmt> {
    mir::Block& block = *at.current_block;
    auto test =
        ClauseSeriesPredicate(process, frame, series.arms[arm]->conditions)
            .evaluate(at);
    if (!test) return std::unexpected(std::move(test.error()));
    auto body =
        LowerStmtIntoChildScope(process, at, series.arms[arm]->then_stmt);
    if (!body) return std::unexpected(std::move(body.error()));
    std::optional<mir::BlockId> else_scope;
    if (otherwise.has_value()) {
      else_scope = block.child_scopes.Add(*std::move(otherwise));
    }
    return mir::IfStmt{
        .condition = ReduceToCondition(unit, block, *test),
        .then_scope = block.child_scopes.Add(*std::move(body)),
        .else_scope = else_scope};
  };

  auto else_or = LowerCatchAll(process, frame, series.else_arm);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  std::optional<mir::Block> rest = BuildFallThrough(
      process.Owner(), frame, std::move(*else_or), i.check,
      QualifiedArmKind::kCondition, span);
  for (std::size_t arm = series.arms.size(); arm-- > 1;) {
    mir::Block level;
    auto nested = arm_at(frame.WithBlock(&level), arm, std::move(rest));
    if (!nested) return std::unexpected(std::move(nested.error()));
    level.AppendStmt(*std::move(nested));
    rest = std::move(level);
  }
  auto first = arm_at(frame, 0, std::move(rest));
  if (!first) return std::unexpected(std::move(first.error()));
  return mir::Stmt{.label = std::move(label), .data = *std::move(first)};
}

// LRM 12.5: the subject is evaluated once, the items are searched in order,
// and an item is selected when any of its labels matches, that search too
// stopping at the first that does. LRM 12.5 and 12.5.1 test a label by
// comparing it as the case keyword compares two values, and LRM 12.5.4 by set
// membership instead, which is the whole difference between the forms.
auto LowerCaseStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CaseStmt& c, diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);
  auto subject_or = SnapshotCaseSubject(process, wrapper_frame, c.condition);
  if (!subject_or) return std::unexpected(std::move(subject_or.error()));
  const CaseSubject subject = *subject_or;

  return std::visit(
      Overloaded{
          [&](hir::CaseKeyword keyword) -> diag::Result<mir::Stmt> {
            return LowerComparedCase(
                process, frame, wrapper, std::move(label), c, keyword, subject,
                span);
          },
          [&](hir::CaseInside) -> diag::Result<mir::Stmt> {
            const mir::CompilationUnit& unit = process.Owner().Unit();
            std::vector<SearchedArm> items;
            items.reserve(c.items.size());
            for (const hir::CaseItem& item : c.items) {
              SearchedArm lowering{
                  .conditions = {},
                  .body = [&process, &item](const WalkFrame& at) {
                    return LowerStmtIntoChildScope(process, at, item.stmt);
                  }};
              lowering.conditions.reserve(item.labels.size());
              for (const hir::ExprId label_expr : item.labels) {
                lowering.conditions.emplace_back([&process, &unit, subject,
                                                  label_expr](
                                                     const WalkFrame& at) {
                  return BuildSetMemberTest(
                      process, at,
                      at.current_block->ReadLocal(subject.local, subject.type),
                      label_expr, unit.builtins.bit1);
                });
              }
              items.push_back(std::move(lowering));
            }
            return BuildWrappedBranching(
                process, frame, wrapper, std::move(label), c.check,
                QualifiedArmKind::kCaseItem, std::move(items), c.default_stmt,
                span);
          }},
      c.label_test);
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

  std::vector<SearchedArm> items;
  items.reserve(c.items.size());
  for (const hir::PatternCaseItem& item : c.items) {
    items.push_back(
        SearchedArm{
            .conditions = {[&](const WalkFrame& at) {
              std::vector<Predicate> terms;
              terms.push_back(PatternPredicate(
                  process, wrapper_frame, subject.local, subject.type,
                  item.pattern, c.keyword));
              if (item.filter.has_value()) {
                terms.push_back(ExpressionPredicate(process, *item.filter));
              }
              return SeriesPredicate(unit, std::move(terms)).evaluate(at);
            }},
            .body = [&process, &item](const WalkFrame& at) {
              return LowerStmtIntoChildScope(process, at, item.stmt);
            }});
  }

  return BuildWrappedBranching(
      process, frame, wrapper, std::move(label), c.check,
      QualifiedArmKind::kCaseItem, std::move(items), c.default_stmt, span);
}

}  // namespace lyra::lowering::hir_to_mir
