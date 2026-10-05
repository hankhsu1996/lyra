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

#include <optional>
#include <span>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
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

// One part of a left-hand-side destructuring (LRM 11.4.12): the place it
// writes and the share of the distributed value it takes.
struct DestructuredPart {
  AccessPath target;
  mir::ExprId value;
};

// An assignment to a concatenation (LRM 11.4.12), as steps of `frame`'s block:
// the right side bound once at an unsigned type as wide as the targets
// together, and each target given its share, most significant first. The
// source wrote one assignment, so a nonblocking one carries every part into one
// deferred effect: a control on it is read once, and every part's share lands
// in the same slot. Answers the binding, which is the assignment's value (LRM
// 11.3.6).
auto Destructure(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::AssignExpr& assign, const hir::ConcatExpr& lhs_concat,
    diag::SourceSpan span) -> diag::Result<mir::ExprId>;

// The deferred half of a destructuring assignment. The source wrote one
// statement, so the parts are frozen together and due at one placement, which
// is what makes a control on such an assignment read once and land every part
// in the same slot (LRM 9.4.5, 10.4.2).
auto BuildDestructuredDeferredAssign(
    ProcessLowerer& process, WalkFrame frame, diag::SourceSpan span,
    const std::optional<hir::DelayOrEventControl>& control,
    std::span<const DestructuredPart> parts) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
