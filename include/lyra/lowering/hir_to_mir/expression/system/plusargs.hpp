#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 21.6 `$test$plusargs` / `$value$plusargs`. The value form is modelled
// as an IIFE: the closure body calls the runtime helper against a temp of
// the output lvalue's shape and conditionally writes the temp back to the
// original lvalue. The test form has no output and lowers to a direct call
// against the runtime handle. Both yield the SV `int` return. Neither reads
// anything a process body owns, so one template serves both pass classes; the
// value form's output argument keeps it out of a structural context, which the
// frontend rejects there.
template <ExprLowerer Lowerer>
auto LowerPlusargsSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::PlusargsSystemSubroutineInfo& info)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
