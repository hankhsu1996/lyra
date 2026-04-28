#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// Process-body expression lowering (recursive, on-demand). Resolves HIR
// LocalVarRef via `proc_state` and HIR MemberVarRef via `class_state`. HIR
// LoopVarRef cannot appear in process bodies and is rejected as an internal
// error. Children are lowered first; the parent expression's child id slots
// receive ids minted by `body_state.AddExpr`.
auto LowerExpr(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, BodyLoweringState& body_state,
    const hir::Process& hir_process, const hir::Expr& expr)
    -> diag::Result<mir::Expr>;

// Constructor-body expression lowering (recursive, on-demand). Resolves HIR
// LoopVarRef via `ctor_state` and HIR MemberVarRef via `class_state`. HIR
// LocalVarRef cannot appear in constructor bodies and is rejected as an
// internal error. The structural scope is the containing scope whose
// `Exprs()` are referenced by the HIR header expressions.
auto LowerExpr(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ConstructorLoweringState& ctor_state, BodyLoweringState& body_state,
    const hir::StructuralScope& scope, const hir::Expr& expr)
    -> diag::Result<mir::Expr>;

auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp;

}  // namespace lyra::lowering::hir_to_mir
