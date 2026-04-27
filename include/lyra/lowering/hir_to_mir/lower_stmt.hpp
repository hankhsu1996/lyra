#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerStmt(
    const UnitLoweringState& unit_state, const ProcessLoweringState& proc_state,
    const hir::Process& hir_proc, BodyLoweringState& body_state,
    const hir::Stmt& stmt) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
