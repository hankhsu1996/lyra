#pragma once

#include <optional>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 21.3.3 expression-position entry. Only `$sformatf` reaches here
// legitimately (a function returning a `value::String`); `$sformat` /
// `$swrite*` are tasks and slang rejects them as expressions, so a call
// arriving with `info.has_output_arg == true` is an upstream invariant
// violation and raises `InternalError`.
// Nothing in the value path reads a process body, so one template serves both
// pass classes; a continuous assignment admits `$sformatf` for the same reason
// it admits any pure value query.
template <ExprLowerer Lowerer>
auto LowerSFormatSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::SFormatSystemSubroutineInfo& info)
    -> diag::Result<mir::Expr>;

// LRM 21.3.3 statement-position entry. The `$sformat` / `$swrite*` output_var
// needs no copy-out wrapper -- it is a single lvalue fully overwritten by the
// call result, so a plain `AssignExpr` covers LRM 13.5 lvalue-binding order;
// `$sformatf` in statement position is an ordinary discard.
auto LowerSFormatSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const support::SFormatSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
