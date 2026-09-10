#pragma once

#include <span>

// Lowering of operator-family expressions (LRM 11.4): unary, binary,
// conditional (`?:`), conversion, and increment / decrement.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The operator a target applies to two values of one type, for a source
// operator that names one. An operator a library performs names none and does
// not reach this.
auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp;

// What an assignment applies to the value its target holds, for the operator
// the source suffixed with `=` (LRM 11.4.1). The clause admits arithmetic,
// bitwise and shift compounds; the first two are operators a target applies,
// and a shift is applied by the entry that performs it, because a shift's
// amount is sized on its own and no two-values-of-one-type operator can say
// that. This is the one place that fork is taken.
auto LowerCompoundOperation(hir::BinaryOp op) -> CompoundOperation;

// HIR-to-MIR binary-operator realization. Takes the lowered operand ids
// (already in `block`) and dispatches on `(op, lhs_type, rhs_type)`: an
// operator a library performs lifts to a `CallExpr` against the entry that
// performs it; real / string comparison and logical operators wrap in
// `kFromBool` (with `BoolCastExpr` around the operands for the logical
// family); the rest produce a native `BinaryExpr` for the backend to render
// mechanically. The single producer of a binary operator, so it is also the one
// place that guarantees a word-parallel operator's operands share a storage
// domain (LRM 11.6.1), inserting the reconciling conversion any synthesized
// site would otherwise have to remember. `unit` is mutable because reconciling
// may intern the operands' common type.
auto BuildMirBinaryExpr(
    mir::CompilationUnit& unit, mir::Block& block, hir::BinaryOp op,
    mir::ExprId lhs_id, mir::ExprId rhs_id, mir::TypeId result_type)
    -> mir::Expr;

// The conjunction / disjunction of `tests`, appended to `block`: the n-ary
// form of the binary operator above. MIR has only binary operators, so a
// lowering that synthesizes a boolean from a list -- a case item's labels, a
// membership test's items, a clause sequence, a structure pattern's fields --
// folds it here instead of growing the chain by hand. Having nothing to fold
// is not a case for the caller to branch on: it is the empty fold, whose value
// is that operator's identity. Both operators short-circuit, so argument order
// is evaluation order.
auto BuildMirLogicalAnd(
    mir::CompilationUnit& unit, mir::Block& block, mir::TypeId bit1_type,
    std::span<const mir::ExprId> tests) -> mir::ExprId;

auto BuildMirLogicalOr(
    mir::CompilationUnit& unit, mir::Block& block, mir::TypeId bit1_type,
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

// Increment / decrement is a write (LRM 11.4.2) and has no structural form, so
// it stays a procedural-only handler rather than a shared template.
auto LowerHirIncDecExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// The clause-chain `?:` whose predicate declares identifiers (LRM 12.6.3).
// A binding needs storage and a statement to initialize it, so the arms
// become assignments into a result local and the expression reads it back.
// A structural predicate cannot declare bindings -- AST-to-HIR rejects one --
// so this form is procedural only.
template <ExprLowerer Lowerer>
auto LowerHirBindingConditionalExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConditionalExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
