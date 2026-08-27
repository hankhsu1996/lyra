#pragma once

// Lowering of the LRM 20.5 conversions between a real and an integral value.
// None of them is a single runtime operation: the value layer answers in a
// machine integer -- the truncated value, or the IEEE 754 pattern itself --
// and the destination's declared representation is what turns that answer into
// the value the call yields. Which precision and which width are involved is
// carried by the call's own result type rather than by the entry it names, so
// `real` and `shortreal` share one lowering.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Lowers `$rtoi`, `$realtobits` / `$shortrealtobits`, and `$bitstoreal` /
// `$bitstoshortreal`. The meaning is independent of the enclosing scope, so
// one template over the pass class serves both contexts; explicit
// instantiations live in the implementation file.
template <ExprLowerer Lowerer>
auto LowerRealConversionCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::BuiltinMethodRef& b, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
