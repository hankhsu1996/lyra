#pragma once

#include <span>
#include <vector>

#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

// One leaf of an event control's wait: the storage a change is watched on, and
// the observation that decides whether a change there is an event. The leaves
// of one event expression name one observation between them, since the value
// being watched is the expression's.
struct ObservedLeaf {
  const hir::SensitivityEntry* entry;
  mir::LocalId observation;
};

// Every SV construct that waits on a signal converges on one awaited runtime
// call taking one trigger per leaf -- `always_comb` / `always_latch` (LRM
// 9.2.2.2.1), `@*` (LRM 9.4.2.2), `@(...)` (LRM 9.4.2), `wait (cond)` (LRM
// 9.4.3), and a continuous assignment -- differing only in what decides that a
// change to a leaf is an event for them.
//
// Lowering picks the observable-pointer expression per leaf so a backend
// forwards one stored expression rather than re-deriving the shape from the
// leaf's type: a plain field becomes an `AddressOf(FieldAccess(...))`, and a
// borrowed-pointer slot (a cross-unit reference sealed in the resolve phase, or
// another sealed pointer) is the bare `FieldAccess`.

// The wait of a construct the standard makes sensitive to the variables it
// reads, where a change to any of them is the event (LRM 9.2.2.2.1).
auto BuildValueChangeWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt;

// The wait of an event control (LRM 9.4.2), where a change to a watched
// variable is a candidacy and the leaf's observation says whether it is an
// event.
auto BuildEventControlWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer, std::span<const ObservedLeaf> leaves)
    -> mir::Stmt;

}  // namespace lyra::lowering::hir_to_mir
