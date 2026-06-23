#pragma once

// LRM 11.5 select expressions: element-, range-, and member-select.
// LRM 7.2.1: a packed struct / union field access lowers as a slice over
// the aggregate's bit plane -- MIR carries no struct-specific node.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// A select's meaning is independent of the enclosing scope, so one template
// over the pass class serves both the procedural and structural contexts;
// explicit instantiations live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerHirElementSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// LHS-context selector lowerings: like the read-context handlers but the
// base lowers through `LowerLhsExpr`, leaving the chain cell-rooted with no
// `ObservableMethod{kGet}` wrap.
template <ExprLowerer Lowerer>
auto LowerHirElementSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
