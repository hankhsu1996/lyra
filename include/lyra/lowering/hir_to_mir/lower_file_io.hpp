#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a file IO system subroutine call ($fopen or $fclose) into a
// mir::Expr carrying a mir::RuntimeCallExpr with the corresponding
// RuntimeFileOpenCall / RuntimeFileCloseCall payload. Dispatches on
// info.kind. LRM 21.3.1 details the MCD/FD descriptor encoding the runtime
// honors when it receives these calls.
auto LowerFileIOSystemSubroutineCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::FileIOSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
