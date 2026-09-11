#pragma once

#include <span>
#include <vector>

#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

class StructuralScopeLowerer;

// One leaf of a wait: the storage watched, and what decides whether reaching it
// is an event for the wait. The leaves watching for one event name one
// observation between them, since what is being watched for is one thing.
struct ObservedLeaf {
  hir::SensitivityEntry entry;
  mir::LocalId observation;
};

// The observable storage one leaf names, as the place an operation on the cell
// acts through. A leaf reaches either a cell of this design through its route,
// or the one program-global cell a unit's namespace owns (LRM 26.2); the two
// are told apart here rather than by everything that needs to name what a leaf
// watches.
[[nodiscard]] auto BuildObservableCellExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    const StructuralScopeLowerer& lowerer, const hir::SensitivityEntry& entry)
    -> mir::ExprId;

// Materialises an observation into a local of `block`, so every leaf watching
// for one event names one value rather than one each. `entry` says which of the
// four forms (LRM 9.4.2, 9.4.2.3, 15.5) it is, because an absent half has no
// value that could stand in for it.
[[nodiscard]] auto DeclareObservation(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::Block& block,
    support::BuiltinFn entry, std::vector<mir::ExprId> arguments)
    -> mir::LocalId;

// Every SV construct that waits for something to happen converges on one
// awaited runtime call taking one trigger per leaf -- `always_comb` /
// `always_latch` (LRM 9.2.2.2.1), `@*` (LRM 9.4.2.2), `@(...)` (LRM 9.4.2),
// `@e` (LRM 15.5.2), `wait (cond)` (LRM 9.4.3), and a continuous assignment --
// differing only in what decides that reaching a leaf is an event for them.
//
// Lowering picks the observable-pointer expression per leaf so a backend
// forwards one stored expression rather than re-deriving the shape from the
// leaf's type: a plain field becomes an `AddressOf(FieldAccess(...))`, and a
// borrowed-pointer slot (a cross-unit reference sealed in the resolve phase, or
// another sealed pointer) is the bare `FieldAccess`.

// The wait itself: reaching a leaf is a candidacy, and the leaf's observation
// says whether it is an event.
auto BuildWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer, std::span<const ObservedLeaf> leaves)
    -> mir::Stmt;

// The wait of a construct the standard makes sensitive to the variables it
// reads, where a change to any of them is the event (LRM 9.2.2.2.1). Being
// reached is the whole condition, so its leaves share the one observation that
// says so.
auto BuildValueChangeWaitStmt(
    mir::Block& target_block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list) -> mir::Stmt;

}  // namespace lyra::lowering::hir_to_mir
