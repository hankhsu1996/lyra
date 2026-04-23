#include "lower.hpp"

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/numeric/SVInt.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/support/unsupported.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerExpression(
    const ProcessLoweringFacts& /*facts*/, ProcessLoweringState& state,
    const ModuleLoweringState& module_state, const slang::ast::Expression& expr)
    -> hir::ExprId {
  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      const auto& literal = expr.as<slang::ast::IntegerLiteral>();
      const auto value = literal.getValue().as<int64_t>();
      if (!value.has_value()) {
        support::Unsupported(
            "LowerExpression: integer literal out of int64 range");
      }
      return state.AppendExpr(
          hir::Expr{
              .data = hir::Primary{hir::IntegerLiteral{.value = *value}}});
    }

    case slang::ast::ExpressionKind::NamedValue: {
      const auto& named = expr.as<slang::ast::NamedValueExpression>();
      const hir::VarDeclId var_id =
          module_state.ResolveVariableDecl(named.symbol);
      return state.AppendExpr(
          hir::Expr{
              .data =
                  hir::Primary{hir::LocalValueRef{hir::ValueDeclRef{var_id}}}});
    }

    default:
      support::Unsupported("LowerExpression: unsupported expression kind");
  }
}

}  // namespace lyra::lowering::ast_to_hir
