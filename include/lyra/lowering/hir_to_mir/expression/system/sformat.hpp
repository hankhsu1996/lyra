#pragma once

#include <optional>
#include <string>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
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
auto LowerSFormatSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id,
    const support::SFormatSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

// LRM 21.3.3 statement-position entry. The `$sformat` / `$swrite*` output_var
// needs no copy-out wrapper -- it is a single string-typed lvalue fully
// overwritten by the call result, so a plain `AssignExpr` covers LRM 13.5
// lvalue-binding order; `$sformatf` in statement position is an ordinary
// discard.
auto LowerSFormatSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    diag::SourceSpan span, const hir::CallExpr& call,
    support::SystemSubroutineId id, std::string_view name,
    const support::SFormatSystemSubroutineInfo& info)
    -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
