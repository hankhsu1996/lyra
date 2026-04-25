#pragma once

#include <slang/ast/expressions/AssignmentExpressions.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerAssignmentStatementData(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const slang::ast::AssignmentExpression& assign,
    hir::ExprId rhs_id) -> diag::Result<hir::StmtData>;

}  // namespace lyra::lowering::ast_to_hir
