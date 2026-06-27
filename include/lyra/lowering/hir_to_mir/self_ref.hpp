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

// The receiver object that owns something at `hops` enclosing-class levels up.
// At hops 0 it is the current body's `self`; above that the thing lives in an
// enclosing class whose runtime object is this scope's ancestor, reached by
// navigating up the object tree `hops` times through the runtime
// `Scope::Parent()` handle and casting to the enclosing class. The cast is
// sound because the reference is intra-unit -- the unit owns the enclosing
// class's layout. Shared by a structural member access and a call to a
// subroutine declared in an enclosing scope.
auto BuildEnclosingScopeReceiver(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops) -> mir::ExprId;

// Builds a read of a structural var through the current body's `self`:
// `MemberAccess(self, member)`. The result type is the var's declared MIR
// storage type, read from the constructed scope at `member.hops` (a wrapper
// type for observable storage, the value type otherwise) -- a fact that lives
// only in the MIR scope, not in HIR, so it is read here rather than passed
// down. Serves both a structural-var reference and a static-lifetime local
// promoted to a per-instance structural var (LRM 13.3.1), which reach their
// storage identically.
auto BuildStructuralMemberAccessExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops, mir::MemberId var) -> mir::Expr;

// Builds a read of a constructor-bound structural param (a generate `genvar`,
// LRM 27.4) at `hops` enclosing-class levels up: the same enclosing-scope
// receiver as a member access, with the param named as a field on it. The
// result type is the param's declared type, read from the scope at `hops`.
auto BuildStructuralParamAccessExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops, mir::ParamId param) -> mir::Expr;

// Constructs a reference to the cell `cell` denotes (LRM 13.5.2): adds a
// reference-construction `CallExpr` to `block` whose result type is a
// `RefType` over `pointee`, and returns its id. The body that holds the
// resulting reference reads / writes the live cell through it. Used for a
// `ref` / `const ref` actual and for a by-reference closure capture.
auto BuildReferenceArg(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId cell,
    mir::TypeId pointee) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
