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
// leaf's type: a plain member becomes an `AddressOf(MemberAccess(...))`; a
// borrowed-pointer slot (cross-unit referent) is the bare `MemberAccess`; an
// upward `ExternalRef` member becomes a `Call(kAsObservable, ...)`. The
// single convergence point for always_comb / always_latch (LRM 9.2.2.2.1)
// and `@*` (LRM 9.4.2.2).
auto MakeSensitivityWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt;

}  // namespace lyra::lowering::hir_to_mir
