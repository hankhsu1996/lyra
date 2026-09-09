#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower `$sampled` (LRM 16.9.3): the value its operand had in the Preponed
// region of the current time step.
//
// The sampled value of an expression is that expression evaluated over the
// sampled values of the variables it reads (LRM 16.5.1), so the call
// contributes no operation of its own -- it lowers to its operand, read as of
// the Preponed region. `$sampled` is the one member of its family that names no
// clocking event, which is why nothing here needs one.
//
// Reading state and sequencing nothing, it is legal wherever a value is, so one
// template serves both pass classes.
template <ExprLowerer Lowerer>
auto LowerSampledValueSystemSubroutineCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
