#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a simulation control task ($finish, $stop, LRM 20.2) into a
// generic `mir::CallExpr` on the runtime handle carrying the call's origin and
// its finish number.
auto LowerTerminationSystemSubroutineCall(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call,
    const support::TerminationSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

// Lower the program control task `$exit` (LRM 24.7) into a call on the runtime
// handle carrying where the call was written, which a call it ignores reports;
// the program it ends is the one the running thread originates in.
auto LowerProgramControlSystemSubroutineCall(
    const ProcessLowerer& process, mir::Builder block, diag::SourceSpan span)
    -> mir::Expr;

// The finish number of a call to `$finish`, `$stop` or `$fatal`, which selects
// what is printed about the run ending (LRM 20.2, Table 20-1; 20.10 names it
// for `$fatal`), as the machine integer the runtime takes. LRM 20.2 makes it an
// expression, so it is read where the call runs, and converted to `int` as any
// integer argument of a system task is. One the call omits is 1 (LRM 20.2). An
// argument with no integer value is evaluated for what it does and stands for
// 1, which is what the runtime makes of any number but 0, 1 and 2.
auto BuildFinishNumber(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call)
    -> diag::Result<mir::ExprId>;

// The call to `entry` that ends the run (LRM 20.2), carrying the runtime
// handle, where the call was written, and the finish number. It departs from
// the calling execution rather than returning, so it answers nothing.
auto BuildEndRunCall(
    const ProcessLowerer& process, mir::Builder block, support::BuiltinFn entry,
    mir::ExprId finish_number, diag::SourceSpan span) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
