#pragma once

// Lowering of expression-statements (LRM 10.4 assignments, LRM 13.5
// subroutine calls in statement position). Includes the LHS-destructuring
// desugar (LRM 11.4.12) and the copy-in/copy-out desugar for user
// subroutines with output / inout arguments.

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerExprStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ExprStmt& e) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
