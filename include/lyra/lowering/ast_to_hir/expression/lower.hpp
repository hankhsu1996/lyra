#pragma once

#include <optional>

#include <slang/ast/Expression.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace lyra::lowering::ast_to_hir {

// `compound_lvalue_context`, when set, carries the lvalue currently being
// assigned to. Slang materializes compound assignments (`a += b`) by binding
// the original lvalue inside its resolved RHS subtree as a synthetic
// `LValueReference`; lowering substitutes that node by wrapping the context
// lvalue in `LvalueRead`.
auto LowerProcExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr,
    std::optional<hir::Lvalue> compound_lvalue_context = std::nullopt)
    -> diag::Result<hir::Expr>;

auto LowerStructuralExpr(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ScopeLoweringState& scope_state, const ScopeStack& stack,
    const slang::ast::Expression& expr,
    std::optional<hir::Lvalue> compound_lvalue_context = std::nullopt)
    -> diag::Result<hir::Expr>;

// Lower a slang assignment LHS into an HIR Lvalue. Process-side only:
// structural code has no general assignment form. (Generate loop iteration
// is handled by the loop's next-value semantic, not by an assignment.)
auto LowerProcLvalue(
    const UnitLoweringFacts& unit_facts, UnitLoweringState& unit_state,
    ProcessLoweringState& proc_state, const ScopeStack& stack,
    const slang::ast::Expression& expr) -> diag::Result<hir::Lvalue>;

}  // namespace lyra::lowering::ast_to_hir
