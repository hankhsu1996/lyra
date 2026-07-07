#pragma once

#include <vector>

#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

// Materialises a mir::SensitivityWaitStmt from a HIR sensitivity entry list.
// Lowering picks the observable-pointer expression per leaf so the backend
// renders one stored expression rather than re-deriving the shape from the
// leaf's type: a plain field becomes an `AddressOf(FieldAccess(...))`, and
// a borrowed-pointer slot (a cross-unit reference sealed in the resolve
// phase, or another sealed pointer) is the bare `FieldAccess`. The single
// convergence point for always_comb / always_latch (LRM 9.2.2.2.1) and `@*`
// (LRM 9.4.2.2).
auto MakeSensitivityWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt;

}  // namespace lyra::lowering::hir_to_mir
