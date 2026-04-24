#include "lower.hpp"

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/numeric/SVInt.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/support/unsupported.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto BuildIntegerLiteralExpr(const slang::ast::IntegerLiteral& literal)
    -> hir::Expr {
  const auto value = literal.getValue().as<int64_t>();
  if (!value.has_value()) {
    support::Unsupported("LowerExpression: integer literal out of int64 range");
  }
  return hir::Expr{.data = hir::Primary{hir::IntegerLiteral{.value = *value}}};
}

auto BuildNamedValueExpr(
    const ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::NamedValueExpression& named) -> hir::Expr {
  if (named.symbol.kind != slang::ast::SymbolKind::Variable) {
    support::Unsupported(
        "LowerExpression: reference to non-variable declaration");
  }
  const auto& var = named.symbol.as<slang::ast::VariableSymbol>();

  const auto binding = scope_state.Unit().LookupVarBinding(var);
  if (!binding.has_value()) {
    support::Unsupported("LowerExpression: reference to unbound variable");
  }
  const auto hops = stack.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    support::Unsupported(
        "LowerExpression: variable declaration not on the current lexical "
        "scope stack");
  }

  return hir::Expr{
      .data = hir::Primary{hir::LocalValueRef{hir::ValueDeclRef{hir::VarDeclRef{
          .parent_scope_hops = *hops, .local_id = binding->local_id}}}}};
}

}  // namespace

auto LowerExpression(
    const ProcessLoweringFacts& /*facts*/, ProcessLoweringState& state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::Expression& expr) -> hir::ExprId {
  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral:
      return state.AppendExpr(
          BuildIntegerLiteralExpr(expr.as<slang::ast::IntegerLiteral>()));

    case slang::ast::ExpressionKind::NamedValue:
      return state.AppendExpr(BuildNamedValueExpr(
          scope_state, stack, expr.as<slang::ast::NamedValueExpression>()));

    default:
      support::Unsupported("LowerExpression: unsupported expression kind");
  }
}

auto LowerStructuralExpression(
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::Expression& expr) -> hir::ExprId {
  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral:
      return scope_state.AppendExpr(
          BuildIntegerLiteralExpr(expr.as<slang::ast::IntegerLiteral>()));

    case slang::ast::ExpressionKind::NamedValue:
      return scope_state.AppendExpr(BuildNamedValueExpr(
          scope_state, stack, expr.as<slang::ast::NamedValueExpression>()));

    default:
      support::Unsupported(
          "LowerStructuralExpression: unsupported expression kind");
  }
}

}  // namespace lyra::lowering::ast_to_hir
