#pragma once

// Lowering of value-build aggregate expressions (LRM 11.4.12 concatenation,
// replication, and LRM 10.9 assignment patterns). Includes
// `DynamicArrayNewExpr` (LRM 7.5.1) -- its single-statement-only constraint
// is enforced separately, but the expression form is a constructor-style
// build that fits this family naturally.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerHirConcatExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::ConcatExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
auto LowerHirReplicationExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::ReplicationExpr& r,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
auto LowerHirAssignmentPatternExprProc(
    ProcessLowerer& process, WalkFrame frame,
    const hir::AssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
auto LowerHirAssignmentPatternReplicationExprProc(
    ProcessLowerer& process, WalkFrame frame,
    const hir::AssignmentPatternReplicationExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
auto LowerHirDynamicArrayNewExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::DynamicArrayNewExpr& n,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
auto LowerHirAssociativeAssignmentPatternExprProc(
    ProcessLowerer& process, WalkFrame frame,
    const hir::AssociativeAssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

auto LowerHirConcatExprStructural(
    const ClassLowerer& lowerer, WalkFrame frame, const hir::ConcatExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
auto LowerHirAssignmentPatternExprStructural(
    const ClassLowerer& lowerer, WalkFrame frame,
    const hir::AssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
auto LowerHirAssignmentPatternReplicationExprStructural(
    const ClassLowerer& lowerer, WalkFrame frame,
    const hir::AssignmentPatternReplicationExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
auto LowerHirAssociativeAssignmentPatternExprStructural(
    const ClassLowerer& lowerer, WalkFrame frame,
    const hir::AssociativeAssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
