#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 13.5 copy-out desugaring at the statement boundary. A subroutine call
// whose actuals are observable lvalues routes the writeback through an
// explicit MIR assignment so the variable's own write path runs. Two
// callers share the shape: file IO tasks with an output argument
// ($fgets / $fread / $ferror) and the UDF F4 path (which inlines its own
// slot construction because per-formal direction picks between `output`
// and `inout` init -- only the wrap-up step is shared here).
//
// The helper takes two phases: callers register one slot per output arg via
// BuildOutputArgSlot, then assemble the call expression themselves, then
// hand the slot list + call to BuildCopyOutBlock to compose:
//   { temp_0 = actual_0; ... ; [lhs =] call(temps...); actual_0 = temp_0; ... }
// as a BlockStmt. Temps are copy-in initialized from the actual so any
// formal that may be left untouched by the callee round-trips through the
// unconditional writeback as a no-op.

struct OutputArgSlot {
  mir::ExprId actual{};
  mir::LocalRef temp{};
  mir::TypeId type{};
};

// Lower `actual_hir` as an lvalue, allocate a same-typed temp in
// `frame.current_block` initialized from the lowered actual, and
// return the slot bookkeeping. UDF F4 (LowerSubroutineCallWithWritebacks)
// does not use this helper because its formal type may differ from the
// actual type via implicit conversion, and its `output`-direction formals
// require default init per LRM 13.3.2.
auto BuildOutputArgSlot(
    ProcessLowerer& proc, WalkFrame frame, hir::ExprId actual_hir,
    std::string_view temp_name) -> diag::Result<OutputArgSlot>;

// Append `call_expr` to `wrapper` (optionally wrapping the call result in an
// implicit ConversionExpr to match `result_type` when the call's natural return
// type differs), then a writeback `actual = read(temp)` for each slot, and wrap
// the resulting scope in a BlockStmt carrying `stmt.label`. `assign_target_id`
// is the LHS for an `lhs = f(...)` shape; `nullopt` produces a bare-call
// statement. The completed `wrapper` is installed as a child of
// `*parent_frame.current_block`, and the returned Stmt's BlockStmt
// references it by id. When `call_suspends` (a bare task call -- LRM 13.4), the
// call statement is emitted as a suspension (`AwaitStmt`); an `lhs = f(...)`
// shape is a function and never suspends.
auto BuildCopyOutBlock(
    const mir::CompilationUnit& unit, mir::ExprId services_id,
    WalkFrame parent_frame, mir::Block wrapper,
    std::optional<std::string> label, mir::TypeId result_type,
    mir::Expr call_expr, bool call_suspends,
    std::optional<mir::ExprId> assign_target_id,
    const std::vector<OutputArgSlot>& slots) -> mir::Stmt;

}  // namespace lyra::lowering::hir_to_mir
