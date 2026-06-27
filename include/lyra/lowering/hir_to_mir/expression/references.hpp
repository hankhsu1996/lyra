#pragma once

// Lowering of primary-expression leaves (LRM 11.4.1): literals (integer,
// string, time, real) plus the four variable / name references (structural
// var, procedural var, loop var, cross-unit var). `PrimaryExpr` is a sum
// over these leaves and is dispatched here as one family.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Procedural-context primary lowering.
auto LowerHirPrimaryExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::Primary& p,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// Structural-context primary lowering. Reads `frame.loop_var_mode` for the
// `LoopVarRef` arm to choose between procedural-induction and
// structural-param resolution.
auto LowerHirPrimaryExprStructural(
    const ClassLowerer& lowerer, WalkFrame frame, const hir::Primary& p,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// Translate a folded HIR integral constant to its MIR form. Shared by
// primary-literal lowering and member-default materialization.
auto LowerHirIntegralConstant(const hir::IntegralConstant& c)
    -> mir::IntegralConstant;

}  // namespace lyra::lowering::hir_to_mir
