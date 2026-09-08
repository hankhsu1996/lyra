#pragma once

#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a simulation control task ($finish, $stop, $exit, LRM 20.2) into a
// generic `mir::CallExpr` on the runtime handle carrying the call's origin and
// its level. The level comes from the call's own argument, falling back to the
// descriptor's default when the call names none.
auto LowerTerminationSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    const hir::CallExpr& call, std::string_view name,
    const support::TerminationSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

// The diagnostic level that selects what a tool prints about a run ending (LRM
// 20.2, Table 20-1). `$fatal` takes the same value as its first argument and
// LRM 20.10 requires the two to agree, which is why one reading serves both.
// It selects a message and nothing a program can observe, so it is fixed where
// the program is compiled: a call that names one names a literal 0, 1, or 2,
// and `argument` says which argument a diagnostic should name.
auto LowerDiagnosticLevel(
    const hir::Expr& level, std::string_view argument, diag::SourceSpan span)
    -> diag::Result<int>;

}  // namespace lyra::lowering::hir_to_mir
