#pragma once

#include <optional>

#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Materialises an already-lowered expression into a fresh local in `wrapper`:
// declares the local with its type's default initialiser, then assigns
// `expr_id` to it. The decl and assign are split (rather than a single
// var-decl-with-init) so the cpp backend's packed-init gap does not bite when
// the type unifies to a packed-explicit form. Snapshotting a case's selector
// once is what lets each label be compared with one evaluation of it (LRM
// 12.5). With an `origin` the local gets a cross-body identity so a
// closure can forward it; without one it is anonymous (used only within
// `wrapper`).
auto SnapshotExprToLocal(
    const UnitLowerer& unit_lowerer, WalkFrame frame, mir::Block& wrapper,
    mir::TypeId type, mir::ExprId expr_id,
    std::optional<BindingOriginId> origin = std::nullopt) -> mir::LocalId;

// A local of the body `frame` is lowering, declared in its block and starting
// out as `init`.
[[nodiscard]] auto DeclareLocal(const WalkFrame& frame, mir::ExprId init)
    -> mir::LocalId;

// `local = value`, appended to `block` as a statement; `value` is already of
// the local's type.
void AssignLocal(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId local,
    mir::ExprId value);

// `value` as a node that may stand at several places. A node that computes is
// evaluated at every place that reaches it, so one is evaluated here, once, as
// the initializer of a local of the enclosing body, and the result reads that
// local; a node that evaluates nothing already may stand at several and is
// handed back. The value is what the local starts out as, so one of a type no
// declaration holds -- a pointer, a machine integer -- is held the same way.
[[nodiscard]] auto EvaluatedOnce(const WalkFrame& frame, mir::ExprId value)
    -> mir::ExprId;

// Freezes an outer expression's value into a closure's environment: hoists it
// into a temp local of the block `outer_frame` is lowering, which ends with
// that block (with a fresh synthesized origin), and
// forwards that local into `closure` as a captured environment element,
// returning the body-side read. This is how a deferred body (a non-blocking
// assignment, a postponed `$display`) keeps a value computed at construction
// time: every value goes through the same path, and the closure layer only ever
// forwards an ordinary local -- the snapshot is not a special capture form. A
// constant has no moment of evaluation to keep, so it is not captured: the
// body states the constant itself.
auto SnapshotValueIntoClosure(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_expr) -> mir::ExprId;

// The same snapshot, answering the body-side binding as the place it is, for a
// snapshot that is itself something a path is rooted at -- a reference the
// body writes through.
auto SnapshotPlaceIntoClosure(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_expr) -> mir::ExprId;

// The same snapshot, answering the body-side binding itself, for a body that
// names it at several places -- state the closure keeps from one call to the
// next, which it reads and then writes, or a reference it reaches through
// more than once.
auto SnapshotBindingIntoClosure(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_expr) -> BodyBindingRef;

}  // namespace lyra::lowering::hir_to_mir
