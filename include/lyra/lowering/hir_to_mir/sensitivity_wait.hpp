#pragma once

#include <optional>
#include <span>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

class ClosureBuilder;
class ProcessLowerer;
class UnitLowerer;

// What a leaf watching for an event to be triggered watches: the
// synchronization object the event held in the local `handle` names, read
// where the wait is built (LRM 15.5.2, 15.5.5.1).
struct HeldEvent {
  mir::LocalId handle;
};

// One leaf of a wait: the storage watched, or the object an event names, and
// what decides whether reaching it is an event for the wait. The leaves
// watching for one event name one observation between them, since what is
// being watched for is one thing.
struct ObservedLeaf {
  std::variant<hir::SensitivityEntry, HeldEvent> watched;
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
    mir::Builder block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::ValueTarget& cell) -> mir::ExprId;

// What an evaluation reaches through `object` -- a class handle, or the running
// method's own object -- to be read through: the same value, stated as reached
// in the report the evaluation states the places it reaches in, where it is
// handed one (LRM 9.4.2). One event source covers every property of an object,
// so the object is what is stated.
[[nodiscard]] auto ReportedObject(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::ExprId object)
    -> mir::ExprId;

// The same for `place`, a pointer to a variable an evaluation reaches through a
// virtual interface (LRM 25.9).
[[nodiscard]] auto ReportedPlace(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::ExprId place)
    -> mir::ExprId;

// Whether the local `report` holds a report at all, as a machine boolean added
// to `block`'s expressions: a body is handed one by an evaluation that states
// what it reaches, and null by any other call.
[[nodiscard]] auto BuildReportHanded(
    const mir::CompilationUnit& unit, mir::Builder block, mir::LocalId report)
    -> mir::ExprId;

// The report a call or a construction made in the block `frame` is writing
// hands on, as an argument added to that block: the one the evaluation being
// lowered states what it reaches in, and null where there is none.
[[nodiscard]] auto BuildReportHandedOn(
    const mir::CompilationUnit& unit, const WalkFrame& frame) -> mir::ExprId;

// The frame `closure`'s body is lowered in, where the closure runs within an
// evaluation that states what it reaches in the report the local `report` of
// the body `outer` is writing holds -- the evaluation's own closure, or an
// array method's `with` clause the evaluation calls -- and so states what it
// reaches there too: the closure captures the pointer, and its body reports
// through its own copy. With no report, the closure's own frame.
[[nodiscard]] auto ClosureFrameReportingTo(
    UnitLowerer& unit_lowerer, const WalkFrame& outer, ClosureBuilder& closure,
    std::optional<mir::LocalId> report) -> WalkFrame;

// States each of `cells` as read in the report `report` holds a pointer to,
// appending to the block `frame` is writing.
template <typename Lowerer>
auto ReportCells(
    Lowerer& lowerer, const WalkFrame& frame, mir::LocalId report,
    std::span<const hir::SensitivityEntry> cells) -> diag::Result<void>;

// States into the report `report` holds a pointer to what an implicit list
// takes of `reads` (LRM 9.2.2.2.1), appending to the block `frame` is writing:
// each cell and the bits read of it, each cell and the bits written of it, and
// each call, made so it states its own into the same report without running.
template <typename Lowerer>
auto ReportReads(
    Lowerer& lowerer, const WalkFrame& frame, const hir::Reads& reads,
    mir::LocalId report) -> diag::Result<void>;

// `entry(arguments)` acting on the report `report` holds a pointer to,
// answering `type`, added to `block`'s expressions.
[[nodiscard]] auto BuildReportCall(
    const mir::CompilationUnit& unit, mir::Builder block, mir::LocalId report,
    support::BuiltinFn entry, std::vector<mir::ExprId> arguments,
    mir::TypeId type) -> mir::ExprId;

// Materialises an observation into a local of `block`, so every leaf watching
// for one event names one value rather than one each. `entry` says which of the
// four forms (LRM 9.4.2, 9.4.2.3, 15.5) it is, because an absent half has no
// value that could stand in for it.
[[nodiscard]] auto DeclareObservation(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::Builder block, support::BuiltinFn entry,
    std::vector<mir::ExprId> arguments) -> mir::LocalId;

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
// what it watches, `evaluated` the expressions of `exprs` its observations
// evaluate where a change happens, and `evaluates_where_reached` whether it
// evaluates anything once, where it is reached: the outermost block of the
// body, which lasts for the body's whole run, or `stop_block` where anything it
// watches or evaluates at a change names something the body declares, which
// exists only from its declaration on (LRM 6.21), and where it evaluates
// anything where it is reached, which it does each time it is.
[[nodiscard]] auto WaitStorageBlock(
    const WalkFrame& frame, mir::Builder stop_block,
    std::span<const hir::SensitivityEntry> cells,
    const base::Arena<hir::Expr, hir::ExprId>& exprs,
    std::span<const hir::ExprId> evaluated, bool evaluates_where_reached)
    -> mir::Builder;

// One trigger per leaf, as the machine array a wait is built on, built in
// `target_block`. A leaf watching part of its cell names the part by the
// select the source wrote, whose indices are constants and are lowered here;
// `lowerer` is the lowering that owns those expressions.
template <typename Lowerer>
auto BuildTriggerArray(
    mir::Builder target_block, const WalkFrame& frame, Lowerer& lowerer,
    std::span<const ObservedLeaf> leaves) -> diag::Result<mir::ExprId>;

// The wait on `leaves` and the stop at it: reaching a leaf is a candidacy, and
// the leaf's observation says whether it is an event. The wait is built in
// `storage_block`, which already declares the leaves' observations, and the
// returned stop at it is for `stop_block`.
template <typename Lowerer>
auto BuildWaitOnStmt(
    mir::Builder storage_block, mir::Builder stop_block, const WalkFrame& frame,
    Lowerer& lowerer, std::span<const ObservedLeaf> leaves)
    -> diag::Result<mir::Stmt>;

// The leaves of a wait sensitive to the variables `sensitivity` names, where a
// change to any of them is the event: being reached is the whole condition, so
// they share one observation saying so, declared in `block`.
[[nodiscard]] auto BuildValueChangeLeaves(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::Builder block, std::span<const hir::SensitivityEntry> sensitivity)
    -> std::vector<ObservedLeaf>;

// The wait of a construct the standard makes sensitive to the variables it
// reads, where a change to any of them is the event (LRM 9.4.2.2, 10.3, 10.6).
// Being reached is the whole condition, so its leaves share the one
// observation that says so. The stop is for `stop_block`.
template <typename Lowerer>
auto BuildValueChangeWaitStmt(
    mir::Builder stop_block, const WalkFrame& frame, Lowerer& lowerer,
    std::span<const hir::SensitivityEntry> sensitivity_list)
    -> diag::Result<mir::Stmt>;

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
