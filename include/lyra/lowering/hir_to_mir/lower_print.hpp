#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a print-like system subroutine call (e.g. $display, $write) into a
// `mir::Expr` carrying a `mir::RuntimeCallExpr`. Returns a user diagnostic for
// unsupported shapes (file output, %m, malformed format string, etc.).
auto LowerPrintSystemSubroutineCall(
    const UnitLoweringState& unit_state, const ClassLoweringState& class_state,
    const ProcessLoweringState& proc_state, BodyLoweringState& body_state,
    const hir::Process& hir_proc, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::PrintSystemSubroutineInfo& print, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
