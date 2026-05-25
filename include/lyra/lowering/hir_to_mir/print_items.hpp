#pragma once

#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/runtime_print.hpp"

namespace lyra::lowering::hir_to_mir {

// Walks a system-subroutine call's argument list and produces the runtime
// print-item sequence. If the first argument is a string literal, it is parsed
// as a format string and remaining arguments are consumed by its directives;
// otherwise each remaining argument is rendered with the default decimal
// format. Shared by every system task that uses the LRM display-style format
// (display / write / strobe / monitor / info / warning / error / fatal).
auto BuildRuntimePrintItemsFromCallArgs(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_proc, const hir::CallExpr& call,
    diag::SourceSpan call_span)
    -> diag::Result<std::vector<mir::RuntimePrintItem>>;

}  // namespace lyra::lowering::hir_to_mir
