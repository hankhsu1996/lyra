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

// LRM 13.5 copy-out at a system subroutine's statement boundary. A system
// subroutine that fills an `output` actual takes a caller-owned temporary in
// that position, and the actual is assigned from it once the call returns, so
// an observable destination's own write path runs instead of the callee
// writing its storage behind its back.
//
// Composition is two steps because the caller assembles the call itself: one
// slot per output actual is registered first, then the assembled call and the
// slots compose
//   { temp_0 = actual_0; ... ; [lhs =] call(temps...); actual_0 = temp_0; ... }
// as one block. Each temp is copy-in initialized from its actual, so an
// argument the callee leaves untouched round-trips through the unconditional
// writeback as a no-op.

struct OutputArgSlot {
  mir::ExprId actual{};
  mir::LocalId temp{};
  mir::TypeId type{};
};

// Lower `actual_hir` as an lvalue, allocate a same-typed temp in
// `frame.current_block` initialized from the lowered actual, and return the
// slot bookkeeping.
auto BuildOutputArgSlot(
    ProcessLowerer& proc, WalkFrame frame, hir::ExprId actual_hir,
    std::string_view temp_name) -> diag::Result<OutputArgSlot>;

// Append `call_expr` to `wrapper` (converting its result to `result_type` when
// the call's own type differs), then a writeback `actual = read(temp)` for each
// slot, and wrap the resulting scope in a block statement carrying `label`.
// `assign_target_id` is the LHS for an `lhs = f(...)` shape; `nullopt` produces
// a bare-call statement. The completed `wrapper` becomes a child scope of
// `*parent_frame.current_block`. A call whose type says the callee completes as
// a coroutine is awaited, and the statement discards its void completion (LRM
// 13.4); an `lhs = f(...)` shape is a function and never suspends.
auto BuildCopyOutBlock(
    const mir::CompilationUnit& unit, WalkFrame parent_frame,
    mir::Block wrapper, std::optional<std::string> label,
    mir::TypeId result_type, mir::Expr call_expr,
    std::optional<mir::ExprId> assign_target_id,
    const std::vector<OutputArgSlot>& slots) -> mir::Stmt;

}  // namespace lyra::lowering::hir_to_mir
