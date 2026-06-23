#pragma once

// Lowering of operator-family expressions (LRM 11.4): unary, binary,
// conditional (`?:`), conversion, and increment / decrement.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Translation table from HIR binary-op enum to MIR binary-op enum. Shared
// with compound assignment lowering, which routes its `compound_op` field
// through this same mapping (the AST-to-HIR validator rejects comparison /
// logical compound ops upstream, so by the time we get here every `BinaryOp`
// is a legitimate target).
auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp;

// An operator's meaning is independent of the enclosing scope, so one template
// over the pass class serves both the procedural and structural contexts. The
// pass class is reached through a uniform `HirExprs` / `LowerExpr` surface, so
// the body deduces from the argument; explicit instantiations for the two pass
// classes live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerHirUnaryExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::UnaryExpr& u,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirBinaryExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::BinaryExpr& b,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirConditionalExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConditionalExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
template <ExprLowerer Lowerer>
auto LowerHirConversionExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConversionExpr& cv,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// Increment / decrement is a write (LRM 11.4.2) and has no structural form, so
// it stays a procedural-only handler rather than a shared template.
auto LowerHirIncDecExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
