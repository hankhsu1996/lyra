#pragma once

#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::mir::passes {

// Eliminate redundant bounds checks in canonical induction loops.
//
// Phase-1 contract: this pass recognizes the exact block layout emitted by
// LowerForLoop in HIR->MIR lowering:
//
//   preheader: init IV with constant, Jump(header)
//   header:    Branch(cmp(iv, const_bound), header+1, exit)
//   body:      one or more blocks (IV not modified here)
//   latch:     iv = iv + 1, Jump(header)
//
// For loops matching this shape where the IV range [init, max_value] is
// within the IndexInRange bounds, the pass rewrites:
//
//   IndexInRange(proven-safe) -> constant true
//   LogicalAnd(proven_true, x) -> x  (collapses 4-state validity chain)
//   GuardedUse(proven_true, place) -> Use(place)
//   GuardedAssign(proven_true, dest, rhs) -> Assign(dest, rhs)
//
// The proven_true set is seeded only from IndexInRange temps and propagated
// through simple copies. LogicalAnd simplification is therefore scoped to
// the bounds-check validity chain by construction.
//
// IndexInRange is emitted only for fixed unpacked array element access,
// so the pass is inherently scoped to fixed unpacked arrays.
//
// Phase-1 scope:
//   - Process/function-local MIR only
//   - Canonical increasing induction loops (constant init, constant bound, +1)
//   - Direct IV used as IndexInRange operand
//   - No derived indices (i + k), no nested proofs, no dynamic arrays
//   - Non-overlapping canonical loops (nested loops have distinct IVs)
void EliminateRedundantBoundsChecks(Design& design, Arena& design_arena);

}  // namespace lyra::mir::passes
