#include "lower.hpp"

#include <cstdint>

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/numeric/SVInt.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/support/internal_error.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto LowerIntegerLiteral(
    const UnitLoweringFacts& unit_facts,
    const slang::ast::IntegerLiteral& literal) -> diag::Result<hir::Expr> {
  const auto value = literal.getValue().as<std::int64_t>();
  if (!value.has_value()) {
    return diag::Unsupported(
        unit_facts.SourceMapper().SpanOf(literal.sourceRange),
        "integer literal does not fit in a 64-bit signed integer",
        diag::UnsupportedCategory::kFeature);
  }
  return hir::Expr{.data = hir::Primary{hir::IntegerLiteral{.value = *value}}};
}

auto LowerNamedValue(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const slang::ast::NamedValueExpression& named)
    -> diag::Result<hir::Expr> {
  const auto& mapper = unit_facts.SourceMapper();
  if (named.symbol.kind != slang::ast::SymbolKind::Variable) {
    return diag::Unsupported(
        mapper.SpanOf(named.sourceRange),
        "reference to non-variable declaration is not supported",
        diag::UnsupportedCategory::kFeature);
  }
  const auto& var = named.symbol.as<slang::ast::VariableSymbol>();

  const auto binding = unit_state.LookupVarBinding(var);
  if (!binding.has_value()) {
    throw support::InternalError(
        "LowerExpressionData: variable was not bound during scope lowering");
  }
  const auto hops = stack.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw support::InternalError(
        "LowerExpressionData: variable home frame is not on the current "
        "scope stack");
  }

  return hir::Expr{
      .data = hir::Primary{hir::LocalValueRef{hir::ValueDeclRef{hir::VarDeclRef{
          .parent_scope_hops = *hops, .local_id = binding->local_id}}}}};
}

}  // namespace

auto LowerExpressionData(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const slang::ast::Expression& expr)
    -> diag::Result<hir::Expr> {
  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral:
      return LowerIntegerLiteral(
          unit_facts, expr.as<slang::ast::IntegerLiteral>());
    case slang::ast::ExpressionKind::NamedValue:
      return LowerNamedValue(
          unit_facts, unit_state, stack,
          expr.as<slang::ast::NamedValueExpression>());
    default:
      return diag::Unsupported(
          unit_facts.SourceMapper().SpanOf(expr.sourceRange),
          "this expression form is not supported yet",
          diag::UnsupportedCategory::kOperation);
  }
}

}  // namespace lyra::lowering::ast_to_hir
