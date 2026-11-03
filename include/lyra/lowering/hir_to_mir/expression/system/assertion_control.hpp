#pragma once

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lowers an LRM 20.11 assertion control task, a statement, into calls of the
// runtime entry that applies it: one to the whole design where the call writes
// no list, and otherwise one for each instance, generate block, named block or
// subroutine the list names, so a list whose every name selects nothing
// applies nothing. Its integer arguments are evaluated once, as the statement
// is reached.
auto LowerAssertionControlSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call,
    const support::AssertionControlSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
