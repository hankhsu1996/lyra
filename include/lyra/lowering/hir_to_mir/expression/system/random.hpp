#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower an unconstrained random number read ($urandom / $urandom_range,
// LRM 18.13.1 -- 18.13.2) into a generic `mir::CallExpr` on the runtime handle.
// Which entry a call names is settled here rather than downstream: a seed
// argument makes `$urandom` the re-seeding entry, and an omitted low bound
// makes `$urandom_range` carry the zero the standard defines it to be. The
// generator itself is the calling process's and the runtime reaches it
// ambiently, so it is never an operand. A random read is a pure value query
// with no statement sequencing, so one template serves both pass classes.
template <ExprLowerer Lowerer>
auto LowerRandomSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::RandomSystemSubroutineInfo& info) -> diag::Result<mir::Expr>;

// Lower a probabilistic distribution read ($random / $dist_*, LRM 20.14) into
// a generic `mir::CallExpr` on the runtime library. The seed is an `inout`
// argument (LRM 20.14.2), so a call completes with both the value drawn and the
// seed that draw advanced, and the sequence that binds the completion, writes
// the seed back to the design's variable, and yields the value is built here.
// A `$random` carrying no seed has no stream to advance and stays a plain draw.
template <ExprLowerer Lowerer>
auto LowerDistributionSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::DistributionSystemSubroutineInfo& info,
    diag::SourceSpan span) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
