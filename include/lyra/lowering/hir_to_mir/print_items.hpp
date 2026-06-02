#pragma once

#include <cstddef>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/support/system_subroutine.hpp"
#include "lyra/value/format.hpp"

namespace lyra::lowering::hir_to_mir {

// Maps a system subroutine's bare-argument default radix (LRM 21.2.1.1:
// `$displayb` -> binary, `$displayh` -> hex, etc.) to the runtime
// FormatKind that drives single-argument format dispatch.
auto RadixToFormatKind(support::PrintRadix r) -> value::FormatKind;

// Walks a system-subroutine call's argument list and produces the runtime
// print-item sequence. If the first argument (after `arg_offset`) is a
// string literal, it is parsed as a format string and remaining arguments
// are consumed by its directives; otherwise each remaining argument is
// rendered with the descriptor's default radix (LRM 21.2.1.1).
// `arg_offset` is 1 for file-output variants whose first call argument is
// the MCD/FD descriptor and not part of the format payload; 0 otherwise.
auto BuildRuntimePrintItemsFromCallArgs(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::PrintSystemSubroutineInfo& info, std::size_t arg_offset,
    diag::SourceSpan call_span)
    -> diag::Result<std::vector<mir::RuntimePrintItem>>;

}  // namespace lyra::lowering::hir_to_mir
