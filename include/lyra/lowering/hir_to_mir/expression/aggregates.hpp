#pragma once

// Lowering of value-build aggregate expressions (LRM 11.4.12 concatenation,
// replication, and LRM 10.9 assignment patterns), and the dynamic-array
// constructor (LRM 7.5.1), whose expression form is a constructor-style build
// of the same family.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// An aggregate value-build's meaning is independent of the enclosing scope, so
// one template over the pass class serves both contexts. Explicit
// instantiations for the two pass classes live in the implementation file.
//
// Every one of them carries the source result type beside the lowered one. Two
// things a build needs are legible only in the source type: the dimension a
// keyed index resolves against, and the element default a container carries for
// every position it does not hold, which includes a member's declaration
// initializer (LRM 7.2.2) that the lowered element type has dropped.
template <ExprLowerer Lowerer>
auto LowerHirConcatExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConcatExpr& c,
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignmentPatternExpr& a,
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternReplicationExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::AssignmentPatternReplicationExpr& a, hir::TypeId hir_result_type,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
// A keyed pattern is the one aggregate build whose shape is not readable from
// the lowered type: an index names an element, so resolving it to an offset
// takes the dimension, and a default stands for however many elements are
// left, which takes the element count. A packed array's type carries neither
// once it is the flat bit plane MIR gives it.
template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternKeyedExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignmentPatternKeyedExpr& k,
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirAssociativeAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::AssociativeAssignmentPatternExpr& a, hir::TypeId hir_result_type,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// Replication (LRM 11.4.12) is an ordinary value expression, legal wherever a
// value is, so it is one template over the pass class like the other aggregate
// families.
template <ExprLowerer Lowerer>
auto LowerHirReplicationExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ReplicationExpr& r,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// The dynamic-array constructor `new[]` (LRM 7.5.1), which the clause admits on
// the right-hand side of a variable declaration assignment as well as a
// blocking procedural one -- and a declaration's initializer runs at time zero
// with the rest of them, so the storage it allocates is simulation-time storage
// either way.
template <ExprLowerer Lowerer>
auto LowerHirDynamicArrayNewExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::DynamicArrayNewExpr& n,
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
