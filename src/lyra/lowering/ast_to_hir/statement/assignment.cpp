#include "assignment.hpp"

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/lvalue.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/support/internal_error.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerAssignmentStatementData(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const slang::ast::AssignmentExpression& assign,
    hir::ExprId rhs_id) -> diag::Result<hir::StmtData> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.SpanOf(assign.sourceRange);

  if (assign.isNonBlocking()) {
    return diag::Unsupported(
        span, "non-blocking assignments are not supported yet",
        diag::UnsupportedCategory::kFeature);
  }
  if (assign.left().kind != slang::ast::ExpressionKind::NamedValue) {
    return diag::Unsupported(
        mapper.SpanOf(assign.left().sourceRange),
        "assignment LHS must be a simple variable reference",
        diag::UnsupportedCategory::kFeature);
  }

  const auto& named = assign.left().as<slang::ast::NamedValueExpression>();
  if (named.symbol.kind != slang::ast::SymbolKind::Variable) {
    return diag::Unsupported(
        mapper.SpanOf(named.sourceRange),
        "assignment LHS must reference a variable",
        diag::UnsupportedCategory::kFeature);
  }
  const auto& var = named.symbol.as<slang::ast::VariableSymbol>();

  const auto binding = unit_state.LookupVarBinding(var);
  if (!binding.has_value()) {
    throw support::InternalError(
        "LowerAssignmentStatementData: LHS variable was not bound during "
        "scope lowering");
  }
  const auto hops = stack.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw support::InternalError(
        "LowerAssignmentStatementData: variable home frame is not on the "
        "current scope stack");
  }

  return hir::StmtData{hir::BlockingAssignment{
      .target =
          hir::Lvalue{hir::LocalValueRef{hir::ValueDeclRef{hir::VarDeclRef{
              .parent_scope_hops = *hops, .local_id = binding->local_id}}}},
      .value = rhs_id}};
}

}  // namespace lyra::lowering::ast_to_hir
