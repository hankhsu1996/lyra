#pragma once

// Lowering of variable-declaration and flow-control statements: variable
// declarations (LRM 6.16 / 13.3.1 static-lifetime body locals), `return`
// (LRM 13.4.1), `break` / `continue` (LRM 12.7).

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt>;

auto LowerReturnStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ReturnStmt& r) -> diag::Result<mir::Stmt>;

auto LowerBreakStmt(
    std::optional<std::string> label, std::optional<hir::LoopLabelId> target)
    -> diag::Result<mir::Stmt>;

auto LowerContinueStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
