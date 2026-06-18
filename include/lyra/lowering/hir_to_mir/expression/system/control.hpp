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

// Lower a termination system subroutine call ($finish) into a generic
// `mir::CallExpr`: the engine handle (`self.Services()`) followed by the level
// argument. Resolves the optional level from the call's literal integer arg
// (falling back to the descriptor's default_level when omitted). Returns a
// user diagnostic if the level arg is non-literal or out of range.
auto LowerFinishSystemSubroutineCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    const hir::CallExpr& call, support::SystemSubroutineId id,
    std::string_view name, const support::TerminationSystemSubroutineInfo& info,
    diag::SourceSpan span) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
