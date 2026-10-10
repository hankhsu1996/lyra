#pragma once

// Lowering of an assignment (LRM 10.4, 11.3.6) and of an increment or
// decrement (LRM 11.4.2). What a write puts where is the same question in
// either context, so each is one template; what differs is that a procedure
// may defer its update to a later region and a construction may not, so the
// deferral below it is procedural only.
//
// A write yields nothing, so each comes in two forms. Where the source reads
// its value it lowers to steps that hold what the write stores and yield that;
// where nothing reads it -- a statement, a loop's initializer or step -- it
// lowers to the write alone.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// An assignment whose value is read: the value is what it stores, the right
// side converted to the target's type, held once (LRM 11.3.6). Only a blocking
// assignment stands where a value is read.
template <ExprLowerer Lowerer>
auto LowerHirAssignExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr>;

// An assignment whose value nothing reads: the write, placed where the
// statement is reached or due later (LRM 10.4).
template <ExprLowerer Lowerer>
auto LowerHirAssignWrite(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span) -> diag::Result<mir::Expr>;

// An increment or decrement whose value is read: the value the target held
// before the step for a postfix form, after it for a prefix one.
template <ExprLowerer Lowerer>
auto LowerHirIncDecExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// An increment or decrement whose value nothing reads: the target updated by
// one, which is `target += 1` or `target -= 1`.
template <ExprLowerer Lowerer>
auto LowerHirIncDecWrite(
    Lowerer& lowerer, WalkFrame frame, const hir::IncDecExpr& inc)
    -> diag::Result<mir::Expr>;

// An assignment whose left-hand side `lhs` is a join of lvalues (LRM A.8.5) --
// a concatenation, an assignment pattern, a stream -- as steps of `frame`'s
// block: what is stored evaluated once, and each place given its share, now or
// in one update due later. An assignment operator applies to what the places
// hold together (LRM 11.4.1). Answers what was stored, which is the
// assignment's value (LRM 11.3.6).
template <ExprLowerer Lowerer>
auto AssignToJoin(
    Lowerer& lowerer, const WalkFrame& frame, const hir::AssignExpr& assign,
    const hir::Expr& lhs, diag::SourceSpan span) -> diag::Result<mir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
