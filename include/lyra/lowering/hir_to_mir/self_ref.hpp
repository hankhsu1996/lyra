#pragma once

#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
struct ProceduralScope;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// Builds a read of the current body's `self` binding (mir.md invariant 11): a
// `ProceduralVarRef` whose hop count is the distance from the reading site back
// to where `self` was declared. The caller adds the returned expression to the
// scope it is composing. This is the one place the self-read shape lives --
// every member access, cross-unit deref, closure self-capture, and runtime-
// effect engine handle starts from it.
auto BuildSelfRefExpr(const WalkFrame& frame, mir::TypeId self_ptr_type)
    -> mir::Expr;

// Constructs a reference to the cell `cell` denotes (LRM 13.5.2): adds a
// reference-construction `CallExpr` to `scope` whose result type is a
// `RefType` over `pointee`, and returns its id. The body that holds the
// resulting reference reads / writes the live cell through it. Used for a
// `ref` / `const ref` actual and for a by-reference closure capture.
auto BuildReferenceArg(
    mir::CompilationUnit& unit, mir::ProceduralScope& scope, mir::ExprId cell,
    mir::TypeId pointee) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
