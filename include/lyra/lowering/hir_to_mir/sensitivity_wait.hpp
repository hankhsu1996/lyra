#pragma once

#include <vector>

#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

// Materialises the value-change wait a HIR sensitivity entry list suspends on:
// an awaited runtime call taking one trigger per leaf. Every SV construct that
// waits on a signal converges here -- `always_comb` / `always_latch` (LRM
// 9.2.2.2.1), `@*` (LRM 9.4.2.2), `@(...)` (LRM 9.4.2), `wait (cond)` (LRM
// 9.4.3), and a continuous assignment -- so they all produce the identical
// wait.
//
// Lowering picks the observable-pointer expression per leaf so a backend
// forwards one stored expression rather than re-deriving the shape from the
// leaf's type: a plain field becomes an `AddressOf(FieldAccess(...))`, and a
// borrowed-pointer slot (a cross-unit reference sealed in the resolve phase, or
// another sealed pointer) is the bare `FieldAccess`.
auto MakeValueChangeWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt;

}  // namespace lyra::lowering::hir_to_mir
