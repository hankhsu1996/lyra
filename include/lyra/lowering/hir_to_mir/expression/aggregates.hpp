#pragma once

// Lowering of value-build aggregate expressions (LRM 11.4.12 concatenation,
// replication, and LRM 10.9 assignment patterns). Includes
// `DynamicArrayNewExpr` (LRM 7.5.1) -- its single-statement-only constraint
// is enforced separately, but the expression form is a constructor-style
// build that fits this family naturally.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// An aggregate value-build's meaning is independent of the enclosing scope, so
// one template over the pass class serves both contexts. Explicit
// instantiations for the two pass classes live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerHirConcatExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConcatExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignmentPatternExpr& a,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternReplicationExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::AssignmentPatternReplicationExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirAssociativeAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::AssociativeAssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

// Replication (LRM 11.4.12.1) is an ordinary value expression, legal wherever a
// value is, so it is one template over the pass class like the other aggregate
// families.
template <ExprLowerer Lowerer>
auto LowerHirReplicationExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ReplicationExpr& r,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// The dynamic-array constructor `new[]` (LRM 7.5.1) allocates simulation-time
// storage, which a constructor-time structural expression cannot do, so it
// stays a procedural-only handler.
auto LowerHirDynamicArrayNewExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::DynamicArrayNewExpr& n,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
