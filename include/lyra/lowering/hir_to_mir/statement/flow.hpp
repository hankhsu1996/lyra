#pragma once

// Lowering of variable-declaration and flow-control statements: variable
// declarations (LRM 6.21 / 13.3.1 static-lifetime body locals), `return`
// (LRM 13.4.1), `break` / `continue` (LRM 12.7).

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// The point of declaration of the variable `var` of the body (LRM 6.21): its
// storage comes into being in the frame's block, of whichever kind its
// lifetime and the branches that borrow it call for, starting at its
// declaration assignment or at its type's default value, and it is bound so
// every later reference reaches that storage. The variable is declared the
// same way whether a statement of the source declares it or a construct does
// in its own header (LRM 12.7.3). The statement that brings the storage up is
// returned unlabelled, for the caller to place.
auto LowerVarDeclaration(
    ProcessLowerer& process, WalkFrame frame, hir::ProceduralVarId var)
    -> diag::Result<mir::Stmt>;

auto LowerVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt>;

auto LowerReturnStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ReturnStmt& r) -> diag::Result<mir::Stmt>;

// A `break` leaves the innermost loop the source wrote around it (LRM 12.8).
// Where that loop is built as a nest the frame says which loop of it to name.
auto LowerBreakStmt(std::optional<std::string> label, const WalkFrame& frame)
    -> diag::Result<mir::Stmt>;

auto LowerContinueStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
