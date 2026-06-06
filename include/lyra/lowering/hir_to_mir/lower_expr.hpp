#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// `proc_state` is taken by mutable reference: most expression lowering is
// pure-functional with respect to it, but a system-subroutine expression
// that models a closure-IIFE (e.g. `$sscanf`) constructs a new procedural
// scope on the way through, which pushes a depth frame and installs a
// capture sink. The non-const signature carries that possibility honestly
// instead of routing the mutation through `mutable` back doors.
auto LowerExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::Expr& expr)
    -> diag::Result<mir::Expr>;

// Procedural expression lowering for the constructor's for-stmt header
// machinery (generate-for init / stop / iter). `LoopVarRef` resolves
// through `ConstructorLoweringState` to a procedural induction var.
auto LowerProceduralExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState& ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::Expr& expr)
    -> diag::Result<mir::Expr>;

// Structural expression lowering for generate-control conditions and any
// other expression evaluated under a structural scope without an active
// for-stmt header. `LoopVarRef` resolves through `StructuralScopeLoweringState`
// to a `StructuralParamRef` on the constructed child object.
auto LowerStructuralExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::Expr& expr)
    -> diag::Result<mir::Expr>;

// Builds an NBA-region closure: snapshots `rhs_id_in_outer` by value into the
// body and assigns it through `lhs_in_outer`. The returned Expr has type
// `void`. `lhs_in_outer` must be an addressable expression rooted at a
// structural var (procedural-local NBA is not supported).
auto BuildNbaSubmitClosureExpr(
    const UnitLoweringState& unit_state,
    const ProceduralScopeLoweringState& outer_scope_state,
    mir::ExprId lhs_in_outer, mir::ExprId rhs_id_in_outer, mir::TypeId rhs_type)
    -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
