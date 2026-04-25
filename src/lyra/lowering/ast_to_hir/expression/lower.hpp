#pragma once

#include <slang/ast/Expression.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"

namespace lyra::lowering::ast_to_hir {

// Pure translator. Caller appends the returned hir::Expr into whichever
// arena it owns (process- or scope-local).
auto LowerExpressionData(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const slang::ast::Expression& expr)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
