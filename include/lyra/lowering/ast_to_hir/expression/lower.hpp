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

// Lower a slang assignment LHS into an HIR Lvalue, with selector
// subexpressions added to the process's expr pool. For the structural
// counterpart (continuous-assignment LHS, LRM 10.3) see
// LowerStructuralLvalue below.
auto LowerProcLvalue(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Lvalue>;

// Same shape as LowerProcLvalue but for scope-level assignment targets
// (continuous assignment LHS, LRM 10.3). Selector subexpressions land in the
// enclosing scope's expr pool. Root is restricted to StructuralVarRef --
// neither procedural vars nor generate-loop induction vars are visible at
// scope level.
auto LowerStructuralLvalue(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Lvalue>;

}  // namespace lyra::lowering::ast_to_hir
