#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower an LRM 20.9 bit vector function into a generic count over the operand's
// bit stream plus, where the function reports a property of that count rather
// than the count itself, a comparison against it. The counted values reach the
// runtime as a value of their own -- the control bits laid out one per bit
// position -- which the call site names for `$countbits` and the descriptor
// fixes for the rest.
//
// A bit count is a pure value query with no statement sequencing, so it is
// legal in a continuous assignment as well as in procedural code and one
// template serves both pass classes.
template <ExprLowerer Lowerer>
auto LowerBitVectorSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::BitVectorSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

// The type of the set a count is taken under: a bit holds one of four values,
// each at most once in the set, one per bit position. A set naming fewer
// repeats one it names, which admits nothing twice (LRM 20.9), so every set is
// a value of this one type.
[[nodiscard]] auto BitCountControlType(const mir::CompilationUnit& unit)
    -> mir::TypeId;

}  // namespace lyra::lowering::hir_to_mir
