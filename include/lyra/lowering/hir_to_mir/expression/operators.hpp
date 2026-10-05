#pragma once

#include <span>

// Lowering of operator-family expressions (LRM 11.4): unary, binary,
// conditional (`?:`) and conversion. An increment or decrement is a write and
// lowers with assignments.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The operator a target applies to two values of one type, for a source
// operator that names one. An operator a library performs names none, nor does
// one that may leave an operand unevaluated, and neither reaches this.
auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp;

// What an assignment applies to the value its target holds, for the operator
// the source suffixed with `=` (LRM 11.4.1). The clause admits arithmetic,
// bitwise and shift compounds; the first two are operators a target applies,
// and a shift is applied by the entry that performs it, because a shift's
// amount is sized on its own and no two-values-of-one-type operator can say
// that. This is the one place that fork is taken.
auto LowerCompoundOperation(hir::BinaryOp op) -> CompoundOperation;

// A source binary operator over two operands that are both evaluated. Takes the
// lowered operand ids (already in `block`): an operator a library performs is a
// call of the entry that performs it, and the rest are the operator a target
// applies. `&&`, `||` and `->` may leave their second operand unevaluated, so
// they are selections built before both operands are lowered and never reach
// this.
auto BuildMirBinaryExpr(
    const mir::CompilationUnit& unit, mir::Block& block, hir::BinaryOp op,
    mir::ExprId lhs_id, mir::ExprId rhs_id, mir::TypeId result_type)
    -> mir::Expr;

// The conjunction and the disjunction of `tests`, every one of them evaluated,
// appended to `block` at `type`, which each test already has: for truths the
// language evaluates all of -- a membership test's items (LRM 11.4.13), a
// range's two bounds, a structure's members (LRM 11.4.5). A list searched only
// as far as the answer is open is a condition search instead. Having nothing to
// fold is not a case for the caller to branch on: it is the empty fold, whose
// value is the operator's identity.
auto BuildMirLogicalAnd(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    std::span<const mir::ExprId> tests) -> mir::ExprId;
auto BuildMirLogicalOr(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type,
    std::span<const mir::ExprId> tests) -> mir::ExprId;

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

}  // namespace lyra::lowering::hir_to_mir
