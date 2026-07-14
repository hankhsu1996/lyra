#pragma once

// Lowering of `fork ... join[_any|_none]` (LRM 9.3.2). Each branch is
// composed as a closure into the fork scope's expression arena. The
// capture-sink machinery decides per-capture whether it is a by-value
// snapshot (a fork-scope local, LRM 6.21) or a by-reference alias (an
// enclosing-process variable).

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerForkStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForkStmt& f) -> diag::Result<mir::Stmt>;

auto LowerWaitForkStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label)
    -> diag::Result<mir::Stmt>;

auto LowerDisableForkStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
