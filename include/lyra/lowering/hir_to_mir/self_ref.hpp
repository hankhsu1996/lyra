#pragma once

#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
struct Block;
struct MemberRef;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// Makes a read of the current body's `self` binding: a `ProceduralVarRef`
// whose hop count is the distance from the reading site back to where `self`
// was declared. The caller adds the returned expression to the scope it is
// composing. This is the one place the self-read shape lives -- every
// member access, cross-unit deref, closure self-capture, and runtime-effect
// engine handle starts from it.
auto MakeSelfRefExpr(const WalkFrame& frame, mir::TypeId self_ptr_type)
    -> mir::Expr;

// Builds a read of a structural var through the current body's `self`:
// `MemberAccess(self, member)`. The result type is the var's declared MIR
// storage type, read from the constructed scope at `member.hops` (a wrapper
// type for observable storage, the value type otherwise) -- a fact that lives
// only in the MIR scope, not in HIR, so it is read here rather than passed
// down. Serves both a structural-var reference and a static-lifetime local
// promoted to a per-instance structural var (LRM 13.3.1), which reach their
// storage identically.
auto BuildStructuralMemberAccessExpr(
    const WalkFrame& frame, const mir::MemberRef& member) -> mir::Expr;

// Constructs a reference to the cell `cell` denotes (LRM 13.5.2): adds a
// reference-construction `CallExpr` to `block` whose result type is a
// `RefType` over `pointee`, and returns its id. The body that holds the
// resulting reference reads / writes the live cell through it. Used for a
// `ref` / `const ref` actual and for a by-reference closure capture.
auto BuildReferenceArg(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId cell,
    mir::TypeId pointee) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
