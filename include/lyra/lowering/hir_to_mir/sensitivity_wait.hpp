#pragma once

#include <span>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

// One leaf of a wait: the storage watched, and what decides whether reaching it
// is an event for the wait. The leaves watching for one event name one
// observation between them, since what is being watched for is one thing.
struct ObservedLeaf {
  hir::SensitivityEntry entry;
  mir::LocalId observation;
};

// Each builder below is templated over the lowering the wait is built in -- a
// procedural body's or a scope's own. Only a leaf that is a route asks it for
// the scope that route is counted from, so a body sitting inside no scope, a
// class method of a package, builds a wait over the leaves it can have.

// The observable storage one leaf names, as the place an operation on the cell
// acts through. A leaf reaches either a cell of this design through its route,
// or the one program-global cell a unit's namespace owns (LRM 26.2); the two
// are told apart here rather than by everything that needs to name what a leaf
// watches.
template <typename Lowerer>
[[nodiscard]] auto BuildObservableCellExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::ValueTarget& cell) -> mir::ExprId;

// What an evaluation reaches through `object` -- a class handle, or the running
// method's own object -- to be read through: the same value, stated as reached
// in the report the evaluation states the places it reaches in, where it has
// one (LRM 9.4.2). One event source covers every property of an object, so the
// object is what is stated. Stating it names the value a second time, so it is
// evaluated once first.
[[nodiscard]] auto ReportedObject(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::ExprId object)
    -> mir::ExprId;

// The same for `place`, a pointer to a variable an evaluation reaches through a
// virtual interface (LRM 25.9).
[[nodiscard]] auto ReportedPlace(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::ExprId place)
    -> mir::ExprId;

// States each of `cells` as reached in the report `report` holds a pointer to,
// appending to the block `frame` is writing.
template <typename Lowerer>
auto ReportCells(
    Lowerer& lowerer, const WalkFrame& frame, mir::LocalId report,
    std::span<const hir::SensitivityEntry> cells) -> diag::Result<void>;

// Records in the report `report` holds a pointer to everything `reads` names,
// appending to the block `frame` is writing: each cell and the bits read of
// it, each object a chain passes through and each interface variable a virtual
// interface holds -- where the handle names something -- and every object
// where a read follows no chain. Each call is made so it reports into the same
// report.
template <typename Lowerer>
auto ReportReads(
    Lowerer& lowerer, const WalkFrame& frame, const hir::Reads& reads,
    mir::LocalId report) -> diag::Result<void>;

// `entry(arguments)` acting on the report `report` holds a pointer to,
// answering `type`, added to `block`'s expressions.
[[nodiscard]] auto BuildReportCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId report,
    support::BuiltinFn entry, std::vector<mir::ExprId> arguments,
    mir::TypeId type) -> mir::ExprId;

// Materialises an observation into a local of `block`, so every leaf watching
// for one event names one value rather than one each. `entry` says which of the
// four forms (LRM 9.4.2, 9.4.2.3, 15.5) it is, because an absent half has no
// value that could stand in for it.
[[nodiscard]] auto DeclareObservation(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::Block& block,
    support::BuiltinFn entry, std::vector<mir::ExprId> arguments)
    -> mir::LocalId;

// Every SV construct that waits for something to happen converges on one
// runtime call taking one trigger per leaf -- `always_comb` / `always_latch`
// (LRM 9.2.2.2.1), `@*` (LRM 9.4.2.2), `@(...)` (LRM 9.4.2), `@e` (LRM
// 15.5.2), `wait (cond)` (LRM 9.4.3), and a continuous assignment -- differing
// only in what decides that reaching a leaf is an event for them. A wait its
// process decides hands it the reports its last evaluation stated instead.
//
// Lowering picks the observable-pointer expression per leaf so a backend
// forwards one stored expression rather than re-deriving the shape from the
// leaf's type: a plain field becomes an `AddressOf(FieldAccess(...))`, and a
// borrowed-pointer slot (a cross-unit reference sealed in the resolve phase, or
// another sealed pointer) is the bare `FieldAccess`.

// The wait itself: reaching a leaf is a candidacy, and the leaf's observation
// says whether it is an event. `entry` is which of the two waits over those
// leaves this is -- one for the next occurrence, or one for a condition the
// body re-tests -- since the two ask the same leaves and part company only
// where a stopped process is started again (LRM 9.7).
//
// A leaf watching part of its cell names the part by the select the source
// wrote, whose indices are lowered here; `lowerer` is the lowering that owns
// those expressions.
template <typename Lowerer>
auto BuildWaitStmt(
    mir::Block& target_block, const WalkFrame& frame, Lowerer& lowerer,
    std::span<const ObservedLeaf> leaves, support::BuiltinFn entry)
    -> diag::Result<mir::Stmt>;

// The wait of a construct the standard makes sensitive to the variables it
// reads, where a change to any of them is the event (LRM 9.2.2.2.1). Being
// reached is the whole condition, so its leaves share the one observation that
// says so.
template <typename Lowerer>
auto BuildValueChangeWaitStmt(
    mir::Block& target_block, const WalkFrame& frame, Lowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list,
    support::BuiltinFn entry) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
