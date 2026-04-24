#include "assignment.hpp"

#include <optional>

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "../expression/lower.hpp"
#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/hir/lvalue.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/support/unsupported.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerAssignmentExpression(
    const ProcessLoweringFacts& facts, ProcessLoweringState& state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::AssignmentExpression& assign) -> hir::StmtId {
  if (assign.isNonBlocking()) {
    support::Unsupported(
        "LowerAssignmentExpression: non-blocking assignment not supported");
  }
  if (assign.left().kind != slang::ast::ExpressionKind::NamedValue) {
    support::Unsupported(
        "LowerAssignmentExpression: LHS must be a simple variable reference");
  }

  const auto& named = assign.left().as<slang::ast::NamedValueExpression>();
  if (named.symbol.kind != slang::ast::SymbolKind::Variable) {
    support::Unsupported(
        "LowerAssignmentExpression: LHS must be a variable reference");
  }
  const auto& var = named.symbol.as<slang::ast::VariableSymbol>();

  const auto binding = scope_state.Unit().LookupVarBinding(var);
  if (!binding.has_value()) {
    support::Unsupported(
        "LowerAssignmentExpression: reference to unbound variable");
  }
  const auto hops = stack.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    support::Unsupported(
        "LowerAssignmentExpression: variable declaration not on the current "
        "lexical scope stack");
  }

  const hir::ExprId rhs_id =
      LowerExpression(facts, state, scope_state, stack, assign.right());

  return state.AppendStmt(
      hir::Stmt{
          .label = std::nullopt,
          .data = hir::BlockingAssignment{
              .target = hir::Lvalue{hir::LocalValueRef{
                  hir::ValueDeclRef{hir::VarDeclRef{
                      .parent_scope_hops = *hops,
                      .local_id = binding->local_id}}}},
              .value = rhs_id}});
}

}  // namespace lyra::lowering::ast_to_hir
