#pragma once

// Lowering of variable-declaration and flow-control statements: variable
// declarations (LRM 6.21 / 13.3.1 static-lifetime body variables), `return`
// (LRM 13.4.1), `break` / `continue` (LRM 12.7).

#include <functional>
#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// The value a variable starts at, built in the frame's block only where
// reaching its declaration brings it up: a static one was brought up before the
// body and asks for none.
using InitialValue = std::function<mir::ExprId()>;

// What a variable starts at where nothing hands it a value: its type's default
// (LRM Table 6-7 for a scalar, Table 7-1 for an aggregate), taken on each entry
// to its scope (LRM 6.21, 13.3.2).
auto DefaultInitialValue(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::ProceduralVarDecl& decl) -> InitialValue;

// What reaching the point of declaration of the variable `var` of the body does
// (LRM 6.21). An automatic one comes into being in the frame's block -- a
// frame slot, or the cell it is promoted to where a detached branch names it
// -- is bound so every later reference reaches that storage, and starts at
// `initial`. A static one was bound and brought up before the body, so reaching
// it does nothing. `name` is the identifier the binding answers to. The
// statement that brings the storage up is returned unlabelled, for the caller
// to place.
auto LowerVarStart(
    ProcessLowerer& process, WalkFrame frame, hir::ProceduralVarId var,
    const std::optional<std::string>& name, const InitialValue& initial)
    -> mir::Stmt;

// The point of declaration of `var` as the source wrote it: it answers to its
// own identifier, starts at its type's default value, and is then given its
// declaration assignment where there is one, which may read it. The variable is
// declared the same way whether a statement of the source declares it or a
// construct does in its own header (LRM 12.7.3).
auto LowerVarDeclaration(
    ProcessLowerer& process, WalkFrame frame, hir::ProceduralVarId var)
    -> diag::Result<mir::Stmt>;

auto LowerVarDeclStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::VarDeclStmt& v) -> diag::Result<mir::Stmt>;

auto LowerReturnStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ReturnStmt& r) -> diag::Result<mir::Stmt>;

// A `break` leaves the innermost loop the source wrote around it (LRM 12.8),
// and names the loop the frame says that is: for a `foreach`, the outermost
// loop of the nest it is built as.
auto LowerBreakStmt(std::optional<std::string> label, const WalkFrame& frame)
    -> diag::Result<mir::Stmt>;

auto LowerContinueStmt(std::optional<std::string> label)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
