#pragma once

#include <functional>
#include <span>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

class ProcessLowerer;
class UnitLowerer;

// A place a wait is reached at that no source text names -- where a force
// ends, where a continuous driver is reestablished (LRM 10.6.2) -- stated as an
// expression built in the block of the frame it is handed, since only the wait
// knows which block it is built in.
using StatedPlace = std::function<diag::Result<mir::ExprId>(const WalkFrame&)>;

// One leaf of a wait: what is watched -- storage the source names, or a place
// the lowering states -- and what decides whether reaching it is an event for
// the wait. The leaves watching for one event name one observation between
// them, since what is being watched for is one thing.
struct ObservedLeaf {
  std::variant<hir::SensitivityEntry, StatedPlace> watched;
  mir::LocalId observation;
};

// Each wait builder below is templated over the lowering the wait is built in
// -- a procedural body's or a scope's own -- except an implicit list's, which
// only a procedure has. Only a leaf that is a route asks it for the scope that
// route is counted from, so a body sitting inside no scope, a class method of
// a package, builds a wait over the leaves it can have.

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
// interface holds -- where the handle names something -- every object where a
// read follows no chain, and each cell and the bits written of it. Each call is
// made so it reports into the same report.
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

// Every construct that waits for something to happen on storage converges on
// one wait holding one trigger per leaf -- `@*` (LRM 9.4.2.2), `@(...)` (LRM
// 9.4.2), `@e` (LRM 15.5.2), an implicit list (LRM 9.2.2.2.1), and a
// continuous assignment -- differing only in what decides that reaching a leaf
// is an event for them, and built where everything it names stops changing.
//
// Lowering picks the observable-pointer expression per leaf so a backend
// forwards one stored expression rather than re-deriving the shape from the
// leaf's type: a plain field becomes an `AddressOf(FieldAccess(...))`, and a
// borrowed-pointer slot (a cross-unit reference sealed in the resolve phase, or
// another sealed pointer) is the bare `FieldAccess`.

// The block a wait whose stop is in `stop_block` is built in, where `cells` are
// what it watches and `evaluated` the expressions of `exprs` its observations
// evaluate where a change happens: the outermost block of the body, which
// lasts for the body's whole run, or `stop_block` where anything among them
// names something the body declares, which exists only from its declaration on
// (LRM 6.21).
[[nodiscard]] auto WaitStorageBlock(
    const WalkFrame& frame, mir::Block& stop_block,
    std::span<const hir::SensitivityEntry> cells,
    const base::Arena<hir::Expr, hir::ExprId>& exprs,
    std::span<const hir::ExprId> evaluated) -> mir::Block&;

// The wait on `leaves` and the stop at it: reaching a leaf is a candidacy, and
// the leaf's observation says whether it is an event. The wait is built in
// `storage_block`, which already declares the leaves' observations, and the
// returned stop at it is for `stop_block`.
template <typename Lowerer>
auto BuildWaitOnStmt(
    mir::Block& storage_block, mir::Block& stop_block, const WalkFrame& frame,
    Lowerer& lowerer, std::span<const ObservedLeaf> leaves)
    -> diag::Result<mir::Stmt>;

// The wait of a construct the standard makes sensitive to the variables it
// reads, where a change to any of them is the event (LRM 9.4.2.2, 10.3, 10.6).
// Being reached is the whole condition, so its leaves share the one
// observation that says so. It is reached at each of `also` in the same way:
// what ends a force for the force's own evaluation, and what reestablishes a
// continuous driver for that driver (LRM 10.6.2). The stop is for `stop_block`.
template <typename Lowerer>
auto BuildValueChangeWaitStmt(
    mir::Block& stop_block, const WalkFrame& frame, Lowerer& lowerer,
    std::span<const hir::SensitivityEntry> sensitivity_list,
    std::span<const StatedPlace> also) -> diag::Result<mir::Stmt>;

// The wait on an implicit list, collected once into a report ahead of the
// procedure's first run (LRM 9.2.2.2.1): everything `reads` names and writes
// is recorded, each call reporting what its function reads and writes without
// running, the report is settled as the list, and the wait is built on it.
// Every statement lands in the block `frame` is writing; the answer is the
// local holding the wait.
auto HoldImplicitList(
    const WalkFrame& frame, ProcessLowerer& lowerer, const hir::Reads& reads)
    -> diag::Result<mir::LocalId>;

}  // namespace lyra::lowering::hir_to_mir
