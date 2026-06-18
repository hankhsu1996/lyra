#pragma once

#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

// Builds a read of the current body's `self` binding (mir.md invariant 11): a
// `ProceduralVarRef` whose hop count is the distance from the reading site back
// to where `self` was declared. The caller adds the returned expression to the
// scope it is composing. This is the one place the self-read shape lives --
// every member access, cross-unit deref, closure self-capture, and runtime-
// effect engine handle starts from it.
auto BuildSelfRefExpr(const WalkFrame& frame, mir::TypeId self_ptr_type)
    -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
