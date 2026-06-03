#pragma once

#include <cstddef>
#include <cstdint>
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

// Whether the first call argument (at `arg_offset`) is required to be a
// string literal. `$display` / `$write` / `$fdisplay` / `$fwrite` accept
// either a literal format string or a value-list with auto-format
// (kOptional); `$sformat` / `$sformatf` require a literal (kRequired)
// because the LRM 21.3.3 NOTE that allows a runtime format-string is
// deferred until a runtime-side format parser lands.
enum class FormatStringRequirement : std::uint8_t {
  kOptional,
  kRequired,
};

// Maps a system subroutine's bare-argument default radix (LRM 21.2.1.1:
// `$displayb` -> binary, `$displayh` -> hex, etc.) to the runtime
// FormatKind that drives single-argument format dispatch.
auto RadixToFormatKind(support::PrintRadix r) -> value::FormatKind;

// Walks a system-subroutine call's argument list and produces the runtime
// print-item sequence. The first argument at `arg_offset` is treated as
// the format-string slot:
//   - if it is a `hir::StringLiteral`, it is parsed as a format string
//     and remaining arguments are consumed by its directives;
//   - else if `fmt_req == kOptional`, every argument from `arg_offset`
//     onward is rendered with `default_radix` (LRM 21.2.1.1);
//   - else (`fmt_req == kRequired`) lowering returns
//     `diag::Unsupported` -- runtime-evaluated format strings are
//     deferred and tracked under `docs/progress/display.md`.
// `arg_offset` is 1 for file-output variants whose first call argument
// is the MCD/FD descriptor, or for `$sformat` / `$swrite*` whose first
// argument is the output_var lvalue; 0 otherwise.
auto BuildRuntimePrintItemsFromCallArgs(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    support::PrintRadix default_radix, std::size_t arg_offset,
    FormatStringRequirement fmt_req, diag::SourceSpan call_span)
    -> diag::Result<std::vector<mir::RuntimePrintItem>>;

}  // namespace lyra::lowering::hir_to_mir
