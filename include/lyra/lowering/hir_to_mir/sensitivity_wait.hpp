#pragma once

#include <vector>

#include "lyra/hir/stmt.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

// Materialises a mir::SensitivityWaitStmt from a HIR sensitivity entry list.
// Identity-shaped end-to-end (distinct from mir::EventTrigger which carries
// an expression for explicit `@(...)`). The single point where always_comb /
// always_latch (LRM 9.2.2.2.1, procedure-attached) and `@*` (LRM 9.4.2.2,
// TimedStmt-attached) converge onto the same MIR runtime construct.
auto BuildSensitivityWaitStmt(
    const StructuralScopeLowerer& scope,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt;

}  // namespace lyra::lowering::hir_to_mir
