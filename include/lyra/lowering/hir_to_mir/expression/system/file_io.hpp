#pragma once

#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// Lower a file IO system subroutine call into a generic mir::CallExpr whose
// receiver is the `files` broker (runtime.Files()) and whose remaining
// arguments are the task operands. One that answers through an argument the
// call names ($fgets / $fread / $ferror) becomes the steps that call it and
// store what it settled, which is an expression like any other and so stands
// wherever the source wrote it.
auto LowerFileIOSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    std::string_view name, const support::FileIOSystemSubroutineInfo& info,
    diag::SourceSpan span) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
