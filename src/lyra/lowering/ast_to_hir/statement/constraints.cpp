#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Constraints.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/constraint.hpp"
#include "lyra/hir/constraint_id.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

// LRM 18.5. Each form of `constraint_expression` is one alternative of the
// constraint tree, and an expression it reads lands in the body's arena beside
// every other. A `dist` is an expression in the front end's tree, standing for
// a constraint of its own, so it is taken out as one here.
auto ProcessLowerer::LowerConstraint(
    const slang::ast::Constraint& constraint, WalkFrame frame)
    -> diag::Result<hir::Constraint> {
  auto& body = *frame.current_procedural_body;
  const auto span = constraint.syntax != nullptr
                        ? frontend::SpanOf(constraint.syntax->sourceRange())
                        : diag::SourceSpan{};
  const auto expr =
      [&](const slang::ast::Expression& e) -> diag::Result<hir::ExprId> {
    auto lowered = LowerExpr(e, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return frame.Exprs().Add(*std::move(lowered));
  };
  const auto exprs = [&](std::span<const slang::ast::Expression* const> list)
      -> diag::Result<std::vector<hir::ExprId>> {
    std::vector<hir::ExprId> out;
    out.reserve(list.size());
    for (const slang::ast::Expression* e : list) {
      auto lowered = expr(*e);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      out.push_back(*lowered);
    }
    return out;
  };
  const auto make = [&](hir::ConstraintData data) {
    return hir::Constraint{.data = std::move(data), .span = span};
  };
  // A constraint nested in this one is lowered and stored in the body's
  // arena, where this one names it.
  const auto nested = [&](const slang::ast::Constraint& inner)
      -> diag::Result<hir::ConstraintId> {
    auto lowered = LowerConstraint(inner, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return body.constraints.Add(*std::move(lowered));
  };
  switch (constraint.kind) {
    case slang::ast::ConstraintKind::Invalid:
      throw InternalError(
          "AST->HIR constraint: an invalid constraint reached lowering, which "
          "the front end reports first");
    case slang::ast::ConstraintKind::List: {
      std::vector<hir::ConstraintId> items;
      for (const slang::ast::Constraint* item :
           constraint.as<slang::ast::ConstraintList>().list) {
        auto lowered = nested(*item);
        if (!lowered) return std::unexpected(std::move(lowered.error()));
        items.push_back(*lowered);
      }
      return make(hir::ConstraintList{.items = std::move(items)});
    }
    case slang::ast::ConstraintKind::Expression: {
      const auto& c = constraint.as<slang::ast::ExpressionConstraint>();
      if (const auto* dist = c.expr.as_if<slang::ast::DistExpression>()) {
        auto subject = expr(dist->left());
        if (!subject) return std::unexpected(std::move(subject.error()));
        std::vector<hir::DistItem> items;
        for (const auto& item : dist->items()) {
          hir::DistItem lowered{};
          if (const auto* range =
                  item.value.as_if<slang::ast::ValueRangeExpression>()) {
            auto low = expr(range->left());
            if (!low) return std::unexpected(std::move(low.error()));
            auto high = expr(range->right());
            if (!high) return std::unexpected(std::move(high.error()));
            lowered.low = *low;
            lowered.high = *high;
          } else {
            auto value = expr(item.value);
            if (!value) return std::unexpected(std::move(value.error()));
            lowered.low = *value;
          }
          if (item.weight.has_value()) {
            auto weight = expr(*item.weight->expr);
            if (!weight) return std::unexpected(std::move(weight.error()));
            lowered.weight = *weight;
            lowered.weight_per_range =
                item.weight->kind ==
                slang::ast::DistExpression::DistWeight::PerRange;
          }
          items.push_back(lowered);
        }
        std::optional<hir::ExprId> default_weight;
        if (const auto* stated = dist->defaultWeight();
            stated != nullptr && stated->expr != nullptr) {
          auto weight = expr(*stated->expr);
          if (!weight) return std::unexpected(std::move(weight.error()));
          default_weight = *weight;
        }
        return make(
            hir::DistConstraint{
                .subject = *subject,
                .items = std::move(items),
                .default_weight = default_weight,
                .is_soft = c.isSoft});
      }
      auto condition = expr(c.expr);
      if (!condition) return std::unexpected(std::move(condition.error()));
      return make(
          hir::ExpressionConstraint{
              .condition = *condition, .is_soft = c.isSoft});
    }
    case slang::ast::ConstraintKind::Implication: {
      const auto& c = constraint.as<slang::ast::ImplicationConstraint>();
      auto predicate = expr(c.predicate);
      if (!predicate) return std::unexpected(std::move(predicate.error()));
      auto inner = nested(c.body);
      if (!inner) return std::unexpected(std::move(inner.error()));
      return make(
          hir::ImplicationConstraint{.predicate = *predicate, .body = *inner});
    }
    case slang::ast::ConstraintKind::Conditional: {
      const auto& c = constraint.as<slang::ast::ConditionalConstraint>();
      auto predicate = expr(c.predicate);
      if (!predicate) return std::unexpected(std::move(predicate.error()));
      auto then_body = nested(c.ifBody);
      if (!then_body) return std::unexpected(std::move(then_body.error()));
      std::optional<hir::ConstraintId> else_body;
      if (c.elseBody != nullptr) {
        auto lowered = nested(*c.elseBody);
        if (!lowered) return std::unexpected(std::move(lowered.error()));
        else_body = *lowered;
      }
      return make(
          hir::ConditionalConstraint{
              .predicate = *predicate,
              .then_body = *then_body,
              .else_body = else_body});
    }
    case slang::ast::ConstraintKind::Uniqueness: {
      auto items =
          exprs(constraint.as<slang::ast::UniquenessConstraint>().items);
      if (!items) return std::unexpected(std::move(items.error()));
      return make(hir::UniquenessConstraint{.items = *std::move(items)});
    }
    case slang::ast::ConstraintKind::DisableSoft: {
      auto target =
          expr(constraint.as<slang::ast::DisableSoftConstraint>().target);
      if (!target) return std::unexpected(std::move(target.error()));
      return make(hir::DisableSoftConstraint{.target = *target});
    }
    case slang::ast::ConstraintKind::SolveBefore: {
      const auto& c = constraint.as<slang::ast::SolveBeforeConstraint>();
      auto solve = exprs(c.solve);
      if (!solve) return std::unexpected(std::move(solve.error()));
      auto after = exprs(c.after);
      if (!after) return std::unexpected(std::move(after.error()));
      return make(
          hir::SolveBeforeConstraint{
              .solve = *std::move(solve), .after = *std::move(after)});
    }
    // LRM 18.5.7: each loop variable is bound as a `foreach` statement binds
    // it (LRM 12.7.3), in the scope the constraint is reached under.
    case slang::ast::ConstraintKind::Foreach: {
      const auto& c = constraint.as<slang::ast::ForeachConstraint>();
      auto array = expr(c.arrayRef);
      if (!array) return std::unexpected(std::move(array.error()));
      std::vector<std::optional<hir::ProceduralVarId>> loop_vars;
      loop_vars.reserve(c.loopDims.size());
      for (const auto& dim : c.loopDims) {
        if (dim.loopVar == nullptr) {
          loop_vars.emplace_back(std::nullopt);
          continue;
        }
        auto type = Owner().InternType(dim.loopVar->getType(), span);
        if (!type) return std::unexpected(std::move(type.error()));
        loop_vars.emplace_back(
            AddProceduralVar(frame, body, *dim.loopVar, *type));
      }
      auto inner = nested(c.body);
      if (!inner) return std::unexpected(std::move(inner.error()));
      return make(
          hir::ForeachConstraint{
              .array = *array,
              .loop_vars = std::move(loop_vars),
              .body = *inner});
    }
  }
  throw InternalError("AST->HIR constraint: unknown constraint kind");
}

}  // namespace lyra::lowering::ast_to_hir
