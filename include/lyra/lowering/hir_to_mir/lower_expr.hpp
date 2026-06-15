#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// `process` is taken by mutable reference: most expression lowering is
// pure-functional with respect to it, but a system-subroutine expression
// that models a closure-IIFE (e.g. `$sscanf`) constructs a new procedural
// scope on the way through, which pushes a depth frame and installs a
// capture sink. The non-const signature carries that possibility honestly
// instead of routing the mutation through `mutable` back doors.
auto LowerExpr(ProcessLowerer& process, WalkFrame frame, const hir::Expr& expr)
    -> diag::Result<mir::Expr>;

// Procedural expression lowering for the constructor's for-stmt header
// machinery (generate-for init / stop / iter). `LoopVarRef` resolves
// through `StructuralScopeLowerer` to a procedural induction var.
auto LowerProceduralExpr(
    const StructuralScopeLowerer& scope, WalkFrame frame, const hir::Expr& expr)
    -> diag::Result<mir::Expr>;

// Structural expression lowering for generate-control conditions and any
// other expression evaluated under a structural scope without an active
// for-stmt header. `LoopVarRef` resolves through `StructuralScopeLowerer`
// to a `StructuralParamRef` on the constructed child object.
auto LowerStructuralExpr(
    const StructuralScopeLowerer& scope, WalkFrame frame, const hir::Expr& expr)
    -> diag::Result<mir::Expr>;

// Builds an NBA-region closure: snapshots `rhs_id_in_outer` by value into the
// body and assigns it through `lhs_in_outer`. The returned Expr has type
// `void`. `lhs_in_outer` must be an addressable expression rooted at a
// structural var (procedural-local NBA is not supported).
auto BuildNbaSubmitClosureExpr(
    const ModuleLowerer& module, WalkFrame frame, mir::ExprId lhs_in_outer,
    mir::ExprId rhs_id_in_outer, mir::TypeId rhs_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
