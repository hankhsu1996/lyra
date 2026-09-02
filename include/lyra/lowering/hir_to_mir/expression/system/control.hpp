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
// generic `mir::CallExpr` on the runtime handle carrying the level argument.
// Resolves the level from the call's literal integer argument, falling back to
// the descriptor's default when the call names none. Returns a user diagnostic
// if the argument is non-literal or is not 0, 1, or 2.
auto LowerTerminationSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    const hir::CallExpr& call, std::string_view name,
    const support::TerminationSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
