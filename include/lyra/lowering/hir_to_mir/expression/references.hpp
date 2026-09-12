#pragma once

// Lowering of primary-expression leaves (LRM 11.4.1): literals -- integer,
// string, time, real -- and every form of reference to a named value, which
// `PrimaryExpr` is the sum over and which is dispatched here as one family.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Procedural-context primary lowering.
auto LowerHirPrimaryExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::Primary& p,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// Structural-context primary lowering.
auto LowerHirPrimaryExprStructural(
    const StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::Primary& p, mir::TypeId result_type) -> diag::Result<mir::Expr>;

// Translate a folded HIR integral constant to its MIR form. Shared by
// primary-literal lowering and member-default materialization.
auto LowerHirIntegralConstant(const hir::IntegralConstant& c)
    -> mir::IntegralConstant;

// The cell a static class property is reached through (LRM 8.9). It belongs to
// the type rather than to an object of it, so it is reached without a receiver,
// and where it sits follows from what replicates the class declaration.
auto LowerStaticPropertyRefExpr(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::StaticPropertyRef& r) -> mir::Expr;

// The cell a variable of some unit's namespace is reached through (LRM 26.2).
// Whether that unit is this one decides both how the reference names the
// storage and whether reaching it makes that unit a dependency, so the two are
// settled together here rather than at each site that needs one.
auto LowerExternalUnitValueRefExpr(
    UnitLowerer& unit_lowerer, const hir::ExternalUnitValueRef& r) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
