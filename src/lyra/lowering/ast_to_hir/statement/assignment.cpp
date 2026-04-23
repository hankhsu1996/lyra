#include "assignment.hpp"

#include <optional>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>

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
    const ModuleLoweringState& module_state,
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
  const hir::VarDeclId var_id = module_state.ResolveVariableDecl(named.symbol);

  const hir::ExprId rhs_id =
      LowerExpression(facts, state, module_state, assign.right());

  return state.AppendStmt(
      hir::Stmt{
          .label = std::nullopt,
          .data = hir::BlockingAssignment{
              .target =
                  hir::Lvalue{hir::LocalValueRef{hir::ValueDeclRef{var_id}}},
              .value = rhs_id}});
}

}  // namespace lyra::lowering::ast_to_hir
