#pragma once

#include <string>

#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Materialises an already-lowered expression into a fresh zero-hop local in
// `wrapper`: declares the local with its type's default initialiser, then
// assigns `expr_id` to it. The decl and assign are split (rather than a single
// var-decl-with-init) so the cpp backend's packed-init gap does not bite when
// the type unifies to a packed-explicit form. Cascade lowerings use this to
// snapshot a selector or predicate once before an if-then-else chain reads it
// repeatedly.
auto SnapshotExprToLocal(
    const ModuleLowerer& module, WalkFrame frame, mir::Block& wrapper,
    std::string name, mir::TypeId type, mir::ExprId expr_id) -> mir::LocalId;

}  // namespace lyra::lowering::hir_to_mir
