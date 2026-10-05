#include "lyra/lowering/hir_to_mir/predicate.hpp"

#include <expected>
#include <optional>
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
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ReadLocal(mir::Block& block, mir::LocalId local, mir::TypeId type)
    -> mir::ExprId {
  return block.exprs.Add(mir::MakeLocalRefExpr(local, type));
}

// The negation of `operand`, at the type the operand has.
auto BuildLogicalNot(mir::Block& block, mir::ExprId operand) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::UnaryExpr{
                  .op = mir::UnaryOp::kLogicalNot, .operand = operand},
          .type = block.exprs.Get(operand).type});
}

auto MakeConditional(
    mir::ExprId condition, mir::ExprId then_value, mir::ExprId else_value,
    mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::ConditionalExpr{
              .condition = condition,
              .then_value = then_value,
              .else_value = else_value},
      .type = type};
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
  if (!type.IsIntegralPacked() && !type.Is<mir::UnpackedArrayType>()) {
    return block.exprs.Add(BuildDefaultValueExpr(unit, block, result_type));
  }
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kMergeConditional,
                          .receiver = then_value},
                  .arguments = {else_value}},
          .type = result_type});
}

// The selection by a predicate whose truth is three-valued. Its three outcomes
// are a chain of two selections over the two ways it can settle, so the
// operator states itself in the primitives every selection already uses, and no
// consumer is left to invent a way to evaluate an arm conditionally:
//
//   p = predicate
//   if (p is not known false) a = then
//   if (p is not known true)  b = else
//   p is known true ? a : p is known false ? b : combined(a, b)
//
// An arm's value is needed on two of the three outcomes -- the one that selects
// it and the one that combines both -- so it is evaluated once, into a local,
// on exactly those. Written out at each outcome instead, an arm that is itself
// such a selection would double its text at every level of a chain.
auto BuildMergingSelection(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    const Predicate& predicate, mir::TypeId result_type,
    const Evaluation& then_arm, const Evaluation& else_arm)
    -> diag::Result<mir::Expr> {
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  auto predicate_or = predicate.evaluate(steps.Frame());
  if (!predicate_or) return std::unexpected(std::move(predicate_or.error()));
  const mir::LocalId held = steps.DeclareLocal(predicate.type, *predicate_or);

  const auto known = [&](bool truth) {
    const mir::ExprId read = ReadLocal(body, held, predicate.type);
    return ReduceToCondition(
        unit, body, truth ? read : BuildLogicalNot(body, read));
  };
  // The local holding `arm`'s value, assigned unless the predicate is known to
  // be `selects_other`.
  const auto evaluate_unless_known =
      [&](bool selects_other,
          const Evaluation& arm) -> diag::Result<mir::LocalId> {
    const mir::LocalId local = steps.DeclareLocal(
        result_type,
        body.exprs.Add(BuildDefaultValueExpr(unit, body, result_type)));
    mir::Block evaluated;
    auto value_or = arm(steps.Frame().WithBlock(&evaluated));
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    evaluated.AppendStmt(
        mir::ExprStmt{
            .expr = evaluated.exprs.Add(
                mir::MakeAssignExpr(
                    unit.builtins, ReadLocal(evaluated, local, result_type),
                    *value_or))});
    body.AppendStmt(
        mir::IfStmt{
            .condition = BuildLogicalNot(body, known(selects_other)),
            .then_scope = body.child_scopes.Add(std::move(evaluated)),
            .else_scope = std::nullopt});
    return local;
  };
  auto then_or = evaluate_unless_known(false, then_arm);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  auto else_or = evaluate_unless_known(true, else_arm);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const auto then_value = [&] {
    return ReadLocal(body, *then_or, result_type);
  };
  const auto else_value = [&] {
    return ReadLocal(body, *else_or, result_type);
  };

  const mir::ExprId unsettled = body.exprs.Add(MakeConditional(
      known(false), else_value(),
      BuildCombinedArms(unit, body, then_value(), else_value(), result_type),
      result_type));
  return steps.Build(body.exprs.Add(
      MakeConditional(known(true), then_value(), unsettled, result_type)));
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

// The series over `terms`, at `series_type`:
//
//   t = truth(first)
//   t is true ? truth(series(rest)) : t
//
// The first term's truth both decides and may be the answer, so it is evaluated
// once into a local; nothing after a term that is not true is evaluated,
// because the rest is an arm of the selection. A lone term is its own value.
auto BuildConjunction(
    const mir::CompilationUnit& unit, std::span<const Predicate> terms,
    mir::TypeId series_type, const WalkFrame& frame)
    -> diag::Result<mir::ExprId> {
  if (terms.size() == 1) {
    return terms.front().evaluate(frame);
  }
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const auto truth = [&](mir::ExprId value) {
    return ConvertToType(
        unit, body, BuildTruth(unit, body, value), series_type);
  };
  auto first_or = terms.front().evaluate(steps.Frame());
  if (!first_or) return std::unexpected(std::move(first_or.error()));
  const mir::LocalId held = steps.DeclareLocal(series_type, truth(*first_or));
  auto rest_or = ConditionallyEvaluated(
      steps.Frame(), [&](const WalkFrame& at) -> diag::Result<mir::ExprId> {
        return BuildConjunction(unit, terms.subspan(1), series_type, at);
      });
  if (!rest_or) return std::unexpected(std::move(rest_or.error()));
  const mir::ExprId value = body.exprs.Add(MakeConditional(
      ReduceToCondition(unit, body, ReadLocal(body, held, series_type)),
      truth(*rest_or), ReadLocal(body, held, series_type), series_type));
  return frame.current_block->exprs.Add(steps.Build(value));
}

}  // namespace

auto ConditionallyEvaluated(const WalkFrame& frame, const Evaluation& operand)
    -> diag::Result<mir::ExprId> {
  BlockBuilder point(frame);
  auto value_or = operand(point.Frame());
  if (!value_or) return std::unexpected(std::move(value_or.error()));
  return frame.current_block->exprs.Add(point.Build(*value_or));
}

auto BuildTruth(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId {
  const mir::TypeId type_id = block.exprs.Get(value).type;
  const mir::Type& type = unit.types.Get(type_id);
  if (!type.IsIntegralPacked()) {
    return ConditionAsBit(unit, block, value);
  }
  // A one-bit value already is the answer reducing it by OR gives.
  if (type.PackedShape().BitWidth() == 1) {
    return value;
  }
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kReductionOr,
                          .receiver = value},
                  .arguments = {}},
          .type = OneBitAnswerType(unit, {&type_id, 1})});
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
        return BuildConjunction(unit, terms, type, at);
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
    const Predicate& predicate, mir::TypeId result_type,
    const Evaluation& then_arm, const Evaluation& else_arm)
    -> diag::Result<mir::Expr> {
  if (CarriesUnknowns(unit, predicate.type)) {
    return BuildMergingSelection(
        unit, frame, predicate, result_type, then_arm, else_arm);
  }
  auto predicate_or = predicate.evaluate(frame);
  if (!predicate_or) return std::unexpected(std::move(predicate_or.error()));
  auto then_or = ConditionallyEvaluated(frame, then_arm);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  auto else_or = ConditionallyEvaluated(frame, else_arm);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  return MakeConditional(
      ReduceToCondition(unit, *frame.current_block, *predicate_or), *then_or,
      *else_or, result_type);
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
