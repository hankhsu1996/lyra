#pragma once

#include <slang/ast/Expression.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerProcExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Expr>;

auto LowerStructuralExpr(
    const UnitLoweringFacts& unit_facts, const slang::ast::Expression& expr)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
