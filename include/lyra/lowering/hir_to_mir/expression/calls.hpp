#pragma once

// Lowering of call expressions (LRM 13): user-defined subroutine calls,
// builtin-method calls (enum / string / event / array / iterator), and
// system-subroutine calls. The system-subroutine arm fans out to the
// per-family handlers under `expression/system/*.hpp`; this header is the
// single dispatch surface every call site recurses through.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerHirCallExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& c,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
