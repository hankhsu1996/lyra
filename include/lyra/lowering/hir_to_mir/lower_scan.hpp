#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 21.3.4.3 `$sscanf` / `$fscanf`. The call is modelled as a closure
// invoked immediately (the IIFE pattern). The closure's body parses into
// procedural-local temps, conditionally writes back to the original output
// lvalues, and yields the matched-conversion count. The body's writeback
// assignments use the existing `AssignExpr` path -- lvalue polymorphism
// stays at the backend; the runtime sees plain temp pointers plus
// per-slot type metadata.
auto LowerScanSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::ScanSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
