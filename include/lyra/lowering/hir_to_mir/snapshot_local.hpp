#pragma once

#include <optional>
#include <string>

#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Materialises an already-lowered expression into a fresh local in `wrapper`:
// declares the local with its type's default initialiser, then assigns
// `expr_id` to it. The decl and assign are split (rather than a single
// var-decl-with-init) so the cpp backend's packed-init gap does not bite when
// the type unifies to a packed-explicit form. Cascade lowerings use this to
// snapshot a selector or predicate once before an if-then-else chain reads it
// repeatedly. With an `origin` the local gets a cross-body identity so a
// closure can forward it; without one it is anonymous (used only within
// `wrapper`).
auto SnapshotExprToLocal(
    const ModuleLowerer& module, WalkFrame frame, mir::Block& wrapper,
    std::string name, mir::TypeId type, mir::ExprId expr_id,
    std::optional<BindingOriginId> origin = std::nullopt) -> mir::LocalId;

// Freezes an outer expression's value into a closure's environment: hoists it
// into a temp local of the outer body (with a fresh synthesized origin) and
// forwards that local into `closure` as a captured environment element,
// returning the body-side read. This is how a deferred body (a non-blocking
// assignment, a postponed `$display`) keeps a value computed at construction
// time: every value goes through the same path, and the closure layer only ever
// forwards an ordinary local -- the snapshot is not a special capture form.
auto SnapshotIntoClosure(
    ModuleLowerer& module, const WalkFrame& outer_frame,
    ClosureBuilder& closure, mir::ExprId outer_expr, std::string name)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
