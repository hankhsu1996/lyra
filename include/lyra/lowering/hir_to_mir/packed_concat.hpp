#pragma once

#include <span>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// Builds the value `runs` join into, given in most-significant-first order:
// their bits laid end to end, at the width and state domain they add up to.
// What that value is then held to -- a declared type, or the unsigned vector
// LRM 11.8.1 makes of a source-level join -- is the caller's, reached by
// converting the result.
//
// One run is not a join, and the value is that run itself. Saying so here is
// what lets the join primitive mean a join, so every consumer of it reads two
// operands and none carries the degenerate case.
//
// `block` is the destination scope for any intermediate expression this
// interns.
[[nodiscard]] auto BuildPackedConcat(
    mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::ExprId> runs) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
