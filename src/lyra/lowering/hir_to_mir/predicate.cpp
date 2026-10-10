#include "lyra/lowering/hir_to_mir/predicate.hpp"

#include <algorithm>
#include <array>
#include <expected>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/struct_methods.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ReadLocal(mir::Block& block, mir::LocalId local, mir::TypeId type)
    -> mir::ExprId {
  return block.exprs.Add(mir::MakeLocalRefExpr(local, type));
}

// LRM 11.4.11 combines the results of two arms neither of which was selected
// bit by bit: a bit both know and agree on survives and every other becomes x
// (Table 11-20). A value with no parts that can agree that way answers with the
// Table 7-1 default of its own type instead, and which of the two it is follows
// from the result type, so it is settled here rather than left for a backend to
// work out.
auto BuildCombinedArms(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId then_value,
    mir::ExprId else_value, mir::TypeId result_type) -> mir::ExprId {
  const mir::Type& type = unit.types.Get(result_type);
  if (!type.IsIntegral() && !type.Is<mir::UnpackedArrayType>()) {
    return block.exprs.Add(BuildDefaultValueExpr(unit, block, result_type));
  }
  return block.exprs.Add(MakeBuiltinCall(
      unit, block, support::BuiltinFn::kMergeConditional, then_value,
      {else_value}, result_type));
}

// Whether the truth `value` is known to be true, and whether it is known to be
// false. A condition holds only for a known nonzero value (LRM 12.4), so being
// known false is asked of the negation.
auto BuildIsKnownTrue(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId {
  return ReduceToCondition(unit, block, value);
}

auto BuildIsKnownFalse(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId {
  return ReduceToCondition(unit, block, BuildLogicalNot(unit, block, value));
}

void AppendAssign(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId local,
    mir::ExprId value) {
  const mir::TypeId type = block.exprs.Get(value).type;
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeAssignExpr(
                  unit.builtins, ReadLocal(block, local, type), value))});
}

// The selection where no predicate can be unknown, which is the chain of its
// arms: each predicate after the first, and every value, is evaluated only
// where the chain reaches it.
auto BuildDecidedSelection(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const SelectionArm> arms, const Evaluation& otherwise,
    mir::TypeId result_type) -> diag::Result<mir::ExprId> {
  mir::Block& block = *frame.current_block;
  std::vector<SelectedValue> reached;
  reached.reserve(arms.size());
  for (const SelectionArm& arm : arms) {
    // The first predicate is evaluated wherever the selection is; each one
    // after it only where the arms before it were not selected.
    auto predicate_or =
        reached.empty() ? arm.predicate.evaluate(frame)
                        : ConditionallyEvaluated(frame, arm.predicate.evaluate);
    if (!predicate_or) return std::unexpected(std::move(predicate_or.error()));
    auto value_or = ConditionallyEvaluated(frame, arm.value);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    reached.push_back(
        {.selected = ReduceToCondition(unit, block, *predicate_or),
         .value = *value_or});
  }
  auto rest_or = ConditionallyEvaluated(frame, otherwise);
  if (!rest_or) return std::unexpected(std::move(rest_or.error()));
  return BuildSelectionChain(block, reached, *rest_or, result_type);
}

// The selection where a predicate can be unknown. The arms are tried one after
// another, each contributing to one answer:
//
//   answer = default; answered = false; ended = false
//   for each arm, and then for `otherwise` as an arm that is always selected:
//     if (!ended) {
//       p = predicate
//       if (p is not known false) {
//         v = value
//         answer = answered ? combined(answer, v) : v
//         answered = true
//         ended = p is known true
//       }
//     }
//
// An arm whose predicate is unknown leaves its value in the answer and the
// selection open, so what a later arm contributes is combined with it; the
// combination is the same whichever two are combined first, which is what lets
// one answer accumulate. An arm is a step beside the one before it, so the
// steps are as deep for three hundred arms as for one.
auto BuildMergingSelection(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const SelectionArm> arms, const Evaluation& otherwise,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const mir::TypeId boolean = unit.builtins.machine_bool;
  const mir::LocalId answer = steps.DeclareLocal(
      result_type,
      body.exprs.Add(BuildDefaultValueExpr(unit, body, result_type)));
  const mir::LocalId answered =
      steps.DeclareLocal(boolean, BuildMachineBool(unit, body, false));
  const mir::LocalId ended =
      steps.DeclareLocal(boolean, BuildMachineBool(unit, body, false));

  // `value`, evaluated into `block`, becomes the answer or is combined into it.
  const auto contribute = [&](mir::Block& block,
                              const Evaluation& value) -> diag::Result<void> {
    const WalkFrame at = steps.Frame().WithBlock(&block);
    auto value_or = value(at);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::LocalId held = DeclareLocal(at, *value_or);
    const mir::ExprId combined = BuildCombinedArms(
        unit, block, ReadLocal(block, answer, result_type),
        ReadLocal(block, held, result_type), result_type);
    AppendAssign(
        unit, block, answer,
        BuildSelectionChain(
            block,
            std::array{SelectedValue{
                .selected = ReadLocal(block, answered, boolean),
                .value = combined}},
            ReadLocal(block, held, result_type), result_type));
    AppendAssign(unit, block, answered, BuildMachineBool(unit, block, true));
    return {};
  };
  const auto append_unless_ended = [&](mir::Block step) {
    body.AppendIfThen(
        BuildConditionNot(unit, body, ReadLocal(body, ended, boolean)),
        std::move(step));
  };

  for (const SelectionArm& arm : arms) {
    mir::Block reached;
    const WalkFrame at = steps.Frame().WithBlock(&reached);
    auto predicate_or = arm.predicate.evaluate(at);
    if (!predicate_or) return std::unexpected(std::move(predicate_or.error()));
    const mir::TypeId predicate_type = reached.exprs.Get(*predicate_or).type;
    const mir::LocalId predicate = DeclareLocal(at, *predicate_or);

    mir::Block taken;
    auto contributed = contribute(taken, arm.value);
    if (!contributed) return std::unexpected(std::move(contributed.error()));
    AppendAssign(
        unit, taken, ended,
        BuildIsKnownTrue(
            unit, taken, ReadLocal(taken, predicate, predicate_type)));
    reached.AppendIfThen(
        BuildConditionNot(
            unit, reached,
            BuildIsKnownFalse(
                unit, reached, ReadLocal(reached, predicate, predicate_type))),
        std::move(taken));
    append_unless_ended(std::move(reached));
  }
  mir::Block last;
  auto contributed = contribute(last, otherwise);
  if (!contributed) return std::unexpected(std::move(contributed.error()));
  append_unless_ended(std::move(last));
  return steps.Build(ReadLocal(body, answer, result_type));
}

// One clause as a predicate: its expression, or, where it matches a pattern,
// the match of that expression's value, taken once.
template <ExprLowerer Lowerer>
auto ClausePredicate(
    Lowerer& lowerer, const WalkFrame& declared_in,
    const hir::ConditionClause& clause) -> Predicate {
  Predicate written = ExpressionPredicate(lowerer, clause.expr);
  if (!clause.pattern.has_value()) {
    return written;
  }
  return Predicate{
      .type = lowerer.Owner().Unit().builtins.bit1,
      .evaluate = [&lowerer, declared_in, pattern = *clause.pattern,
                   subject_value = std::move(written)](
                      const WalkFrame& at) -> diag::Result<mir::ExprId> {
        BlockBuilder steps(at);
        auto subject_or = subject_value.evaluate(steps.Frame());
        if (!subject_or) return std::unexpected(std::move(subject_or.error()));
        const mir::LocalId subject = SnapshotExprToLocal(
            lowerer.Owner(), steps.Frame(), steps.Body(), subject_value.type,
            *subject_or);
        auto matched =
            PatternPredicate(
                lowerer, declared_in, subject, subject_value.type, pattern)
                .evaluate(steps.Frame());
        if (!matched) return std::unexpected(std::move(matched.error()));
        return at.current_block->exprs.Add(steps.Build(*matched));
      }};
}

// The search where no term can be unknown, so each term either settles the
// answer or passes it on. That is a selection: every term but the last is an
// arm, selected where the term settles and answering what it settles the
// search as, and the last term's truth is what none of them settling leaves.
//
//   ||:  t0 ? 1 : t1 ? 1 : ... : truth(last)
//   &&:  !t0 ? 0 : !t1 ? 0 : ... : truth(last)
auto BuildDecidedSearch(
    const mir::CompilationUnit& unit, const WalkFrame& frame, SettledBy rule,
    std::span<const Predicate> terms, mir::TypeId type)
    -> diag::Result<mir::ExprId> {
  const auto settled_as = [&unit, type](bool answer) -> Evaluation {
    return [&unit, type,
            answer](const WalkFrame& at) -> diag::Result<mir::ExprId> {
      mir::Block& block = *at.current_block;
      return ConvertToType(
          unit, block, BuildBit1Literal(unit, block, answer), type);
    };
  };
  const auto is_false = [&unit](const Predicate& term) {
    return Predicate{
        .type = unit.builtins.machine_bool,
        .evaluate = [&unit,
                     &term](const WalkFrame& at) -> diag::Result<mir::ExprId> {
          auto value_or = term.evaluate(at);
          if (!value_or) return value_or;
          mir::Block& block = *at.current_block;
          return BuildConditionNot(
              unit, block, ReduceToCondition(unit, block, *value_or));
        }};
  };
  const auto arm_for = [&](const Predicate& term) {
    switch (rule) {
      case SettledBy::kTrueTerm:
        return SelectionArm{.predicate = term, .value = settled_as(true)};
      case SettledBy::kFalseTerm:
      case SettledBy::kTermNotTrue:
        return SelectionArm{
            .predicate = is_false(term), .value = settled_as(false)};
    }
    throw InternalError("BuildDecidedSearch: unknown SettledBy");
  };

  std::vector<SelectionArm> arms;
  arms.reserve(terms.size() - 1);
  for (const Predicate& term : terms.first(terms.size() - 1)) {
    arms.push_back(arm_for(term));
  }
  const Evaluation last_truth =
      [&](const WalkFrame& at) -> diag::Result<mir::ExprId> {
    auto value_or = terms.back().evaluate(at);
    if (!value_or) return value_or;
    mir::Block& block = *at.current_block;
    return ConvertToType(unit, block, BuildTruth(unit, block, *value_or), type);
  };
  return BuildDecidedSelection(unit, frame, arms, last_truth, type);
}

// The search where a term can be unknown. The answer starts as the first
// term's truth, and each later term is a step that runs only while the answer
// is open and folds its own truth in:
//
//   answer = truth(first)
//   if (answer is open) answer = answer folded with truth(next)
//   ...
//
// Folding by the operator's own table is what makes an unknown term leave the
// answer to the ones after it: x || 1 is 1 and x && 0 is 0 (LRM 11.4.7).
auto BuildOpenSearch(
    const mir::CompilationUnit& unit, const WalkFrame& frame, SettledBy rule,
    std::span<const Predicate> terms, mir::TypeId type)
    -> diag::Result<mir::ExprId> {
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const auto truth = [&](mir::Block& block, mir::ExprId value) {
    return ConvertToType(unit, block, BuildTruth(unit, block, value), type);
  };
  const auto is_open = [&](mir::ExprId answer) {
    switch (rule) {
      case SettledBy::kTrueTerm:
        return BuildConditionNot(
            unit, body, BuildIsKnownTrue(unit, body, answer));
      case SettledBy::kFalseTerm:
        return BuildConditionNot(
            unit, body, BuildIsKnownFalse(unit, body, answer));
      case SettledBy::kTermNotTrue:
        return BuildIsKnownTrue(unit, body, answer);
    }
    throw InternalError("BuildOpenSearch: unknown SettledBy");
  };
  const auto folded = [&](mir::Block& block, mir::ExprId answer,
                          mir::ExprId term) {
    switch (rule) {
      case SettledBy::kTrueTerm:
        return BuildMirLogicalOr(unit, block, type, std::array{answer, term});
      case SettledBy::kFalseTerm:
        return BuildMirLogicalAnd(unit, block, type, std::array{answer, term});
      // A term is reached only while every term before it was true, so its
      // own truth is the answer so far.
      case SettledBy::kTermNotTrue:
        return term;
    }
    throw InternalError("BuildOpenSearch: unknown SettledBy");
  };

  auto first_or = terms.front().evaluate(steps.Frame());
  if (!first_or) return std::unexpected(std::move(first_or.error()));
  const mir::LocalId answer = steps.DeclareLocal(type, truth(body, *first_or));
  for (const Predicate& term : terms.subspan(1)) {
    mir::Block step;
    auto term_or = term.evaluate(steps.Frame().WithBlock(&step));
    if (!term_or) return std::unexpected(std::move(term_or.error()));
    AppendAssign(
        unit, step, answer,
        folded(step, ReadLocal(step, answer, type), truth(step, *term_or)));
    body.AppendIfThen(is_open(ReadLocal(body, answer, type)), std::move(step));
  }
  return frame.current_block->exprs.Add(
      steps.Build(ReadLocal(body, answer, type)));
}

}  // namespace

auto BuildSearch(
    const mir::CompilationUnit& unit, const WalkFrame& frame, SettledBy rule,
    std::span<const Predicate> terms, mir::TypeId type)
    -> diag::Result<mir::ExprId> {
  if (terms.size() < 2) {
    throw InternalError("BuildSearch: a search has fewer than two terms");
  }
  if (CarriesUnknowns(unit, type)) {
    return BuildOpenSearch(unit, frame, rule, terms, type);
  }
  return BuildDecidedSearch(unit, frame, rule, terms, type);
}

auto ConditionallyEvaluated(const WalkFrame& frame, const Evaluation& operand)
    -> diag::Result<mir::ExprId> {
  BlockBuilder point(frame);
  auto value_or = operand(point.Frame());
  if (!value_or) return std::unexpected(std::move(value_or.error()));
  return frame.current_block->exprs.Add(point.Build(*value_or));
}

auto BuildLogicalNot(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId {
  const mir::TypeId type = block.exprs.Get(value).type;
  return block.exprs.Add(MakeUnary(
      unit, block, mir::UnaryOp::kLogicalNot, value,
      OneBitAnswerType(unit, {&type, 1})));
}

auto BuildTruth(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId {
  const mir::TypeId type_id = block.exprs.Get(value).type;
  const mir::Type& type = unit.types.Get(type_id);
  if (!type.IsIntegral()) {
    return ConditionAsBit(unit, block, value);
  }
  // A one-bit value that can hold no x or z already is its own truth. One that
  // can is not: a truth is 1, 0 or x and never z (LRM 11.4.7), which is what
  // reducing it by OR answers.
  if (type.Integral().bit_width == 1 && !CarriesUnknowns(unit, type_id)) {
    return value;
  }
  return block.exprs.Add(MakeBuiltinCall(
      unit, block, support::BuiltinFn::kReductionOr, value, {},
      OneBitAnswerType(unit, {&type_id, 1})));
}

template <ExprLowerer Lowerer>
auto ExpressionPredicate(Lowerer& lowerer, hir::ExprId id) -> Predicate {
  return Predicate{
      .type = lowerer.Owner().TranslateType(lowerer.HirExprs().Get(id).type),
      .evaluate = [&lowerer,
                   id](const WalkFrame& at) -> diag::Result<mir::ExprId> {
        auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(id), at);
        if (!lowered) return std::unexpected(std::move(lowered.error()));
        return at.current_block->exprs.Add(*std::move(lowered));
      }};
}

auto Negated(const mir::CompilationUnit& unit, Predicate predicate)
    -> Predicate {
  const mir::TypeId type = OneBitAnswerType(unit, {&predicate.type, 1});
  return Predicate{
      .type = type,
      .evaluate = [&unit, operand = std::move(predicate)](
                      const WalkFrame& at) -> diag::Result<mir::ExprId> {
        auto value_or = operand.evaluate(at);
        if (!value_or) return value_or;
        mir::Block& block = *at.current_block;
        return BuildLogicalNot(unit, block, BuildTruth(unit, block, *value_or));
      }};
}

auto SeriesPredicate(
    const mir::CompilationUnit& unit, std::vector<Predicate> terms)
    -> Predicate {
  if (terms.empty()) {
    throw InternalError("SeriesPredicate: a predicate has no terms");
  }
  if (terms.size() == 1) {
    return std::move(terms.front());
  }
  std::vector<mir::TypeId> term_types;
  term_types.reserve(terms.size());
  for (const Predicate& term : terms) {
    term_types.push_back(term.type);
  }
  const mir::TypeId type = OneBitAnswerType(unit, term_types);
  return Predicate{
      .type = type,
      .evaluate = [&unit, type, terms = std::move(terms)](
                      const WalkFrame& at) -> diag::Result<mir::ExprId> {
        return BuildSearch(unit, at, SettledBy::kTermNotTrue, terms, type);
      }};
}

template <ExprLowerer Lowerer>
auto ClauseSeriesPredicate(
    Lowerer& lowerer, const WalkFrame& declared_in,
    std::span<const hir::ConditionClause> clauses) -> Predicate {
  std::vector<Predicate> terms;
  terms.reserve(clauses.size());
  for (const hir::ConditionClause& clause : clauses) {
    terms.push_back(ClausePredicate(lowerer, declared_in, clause));
  }
  return SeriesPredicate(lowerer.Owner().Unit(), std::move(terms));
}

auto BuildSelection(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const SelectionArm> arms, const Evaluation& otherwise,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  if (arms.empty()) {
    throw InternalError("BuildSelection: a selection has no arms");
  }
  const bool can_be_unknown =
      std::ranges::any_of(arms, [&](const SelectionArm& arm) {
        return CarriesUnknowns(unit, arm.predicate.type);
      });
  if (can_be_unknown) {
    return BuildMergingSelection(unit, frame, arms, otherwise, result_type);
  }
  auto selection_or =
      BuildDecidedSelection(unit, frame, arms, otherwise, result_type);
  if (!selection_or) return std::unexpected(std::move(selection_or.error()));
  return frame.current_block->exprs.Get(*selection_or);
}

template auto ExpressionPredicate(ProcessLowerer&, hir::ExprId) -> Predicate;
template auto ExpressionPredicate(const StructuralScopeLowerer&, hir::ExprId)
    -> Predicate;
template auto ClauseSeriesPredicate(
    ProcessLowerer&, const WalkFrame&, std::span<const hir::ConditionClause>)
    -> Predicate;
template auto ClauseSeriesPredicate(
    const StructuralScopeLowerer&, const WalkFrame&,
    std::span<const hir::ConditionClause>) -> Predicate;

}  // namespace lyra::lowering::hir_to_mir
