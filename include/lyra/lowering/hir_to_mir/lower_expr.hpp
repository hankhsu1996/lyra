#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, const hir::Expr& expr)
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

}  // namespace lyra::lowering::hir_to_mir
