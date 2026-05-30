#pragma once

#include <slang/ast/Expression.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerProcExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Expr>;

auto LowerStructuralExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Expr>;

// Lower a slang assignment LHS into an HIR Lvalue. Process-side only:
// structural code has no general assignment form. (Generate loop iteration
// is handled by the loop's next-value semantic, not by an assignment.)
auto LowerProcLvalue(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Lvalue>;

}  // namespace lyra::lowering::ast_to_hir
