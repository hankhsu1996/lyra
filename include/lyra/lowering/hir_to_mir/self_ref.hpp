#pragma once

#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// A body once the parameters it takes ahead of its formals are bound: the
// parameters in order, and the frame its statements lower under.
struct BoundImplicitParameters {
  std::vector<mir::LocalId> params;
  WalkFrame frame;
};

// Binds what a body of `owner` of the given form takes ahead of its formals,
// each under its own identity so a closure inside the body reaches it by
// capture: the object for an instance member or a constructor, then, where the
// class belongs to an instance, that instance for a body no object records it
// for -- a type-associated one, or a constructor, which enters its base before
// its object records anything (LRM 8.7). A body handed the instance reaches it
// through that parameter; any other keeps the way `frame` reaches it.
auto BindImplicitParameters(
    const WalkFrame& frame, const ClassShape& owner, CallableForm form)
    -> BoundImplicitParameters;

// Makes a read of the current body's `self` binding: a direct `LocalRef` in a
// directly-invoked body, or a field access over the closure receiver when
// `self` was captured. The receiver is an ordinary binding resolved through the
// same capture machinery as any other; its read appends any closure-receiver
// operand to `frame.current_block`, and the caller adds the returned expression
// to that same block. The read is typed as `self_ptr_type`, the borrowed
// pointer to the enclosing object. This is the one place the self-read shape
// lives -- every field access, cross-unit deref, closure self-capture, and
// runtime-effect engine handle starts from it.
auto MakeSelfRefExpr(const WalkFrame& frame, mir::TypeId self_ptr_type)
    -> mir::Expr;

// The object `reaches` designates, as a place -- `*reaches` -- added to
// `block`, or the place itself where `reaches` is its address. A member is
// reached on an object, so this is what a field access or a call is made on
// wherever a handle, a pointer, or a write in progress reaches it.
auto BuildObjectDeref(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId reaches)
    -> mir::ExprId;

// `object`, a pointer to an object, as `pointer`, a pointer to a class that
// object is an object of: the operand unchanged when it already is one,
// otherwise converted -- what C++ writes as a conversion between pointers to a
// class and its base. Every object of the design hierarchy is a scope, so what
// holds objects of several classes holds each as the scope it is, and a use
// naming the class views it as that class.
[[nodiscard]] auto ObjectAs(
    mir::Block& block, mir::ExprId object, mir::TypeId pointer) -> mir::ExprId;

// The receiver object that owns something at `hops` enclosing-class levels up.
// At hops 0 it is the current body's `self`; above that the thing lives in an
// enclosing class whose runtime object is this scope's ancestor, reached by
// navigating up the object tree `hops` times through the runtime
// `Scope::Parent()` handle and casting to the enclosing class. The cast is
// sound because the reference is intra-unit -- the unit owns the enclosing
// class's layout. Shared by a structural field access and a call to a
// subroutine declared in an enclosing scope.
auto BuildEnclosingScopeReceiver(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops) -> mir::ExprId;

// The instance a construction or type-associated call hands a class that
// belongs to one, reached from where `frame` stands -- a climb, or the end of a
// route `lowerer` holds -- and handed over as the scope every instance is,
// which is how the class takes it.
template <typename Lowerer>
auto BuildImplicitInstanceArgument(
    const Lowerer& lowerer, const WalkFrame& frame,
    const hir::DeclaringInstanceReach& reach) -> mir::ExprId {
  const auto& unit = lowerer.Owner().Unit();
  const mir::ExprId reached = std::visit(
      Overloaded{
          [&](hir::StructuralHops hops) {
            return BuildEnclosingScopeReceiver(
                frame, unit, mir::EnclosingHops{hops.value});
          },
          [&](const hir::RoutedObjectRef& routed) {
            return lowerer.RouteEnd(frame, routed.id);
          }},
      reach);
  return frame.current_block->exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = reached},
          .type = unit.builtins.scope_ptr});
}

// Builds a read of a structural var through the current body's `self`:
// `(*self).field`. The result type is the var's declared MIR
// storage type, read from the enclosing scope reached by `hops` (a wrapper
// type for observable storage, the value type otherwise) -- a fact that lives
// only in the MIR scope, not in HIR, so it is read here rather than passed
// down. Serves both a structural-var reference and a static-lifetime local
// promoted to a per-instance structural var (LRM 13.3.1), which reach their
// storage identically.
auto BuildStructuralFieldAccessExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops, mir::FieldId var) -> mir::Expr;

// The same read of `field`, a member of the scope's object that a class it
// extends may declare -- what the scope's unit published of it -- so the
// member is named by that class and its type is handed in rather than read
// off the scope's own class.
auto BuildStructuralFieldAccessExpr(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops, const mir::ClassFieldTarget& field,
    mir::TypeId field_type) -> mir::Expr;

// A reference to the cell `cell` denotes (LRM 13.5.2): a reference
// construction added to `block`, whose result type is a `RefType` over
// `pointee` -- or `cell` itself where it already is a reference. The body that
// holds the resulting reference reads / writes the live cell through it. Used
// for a `ref` / `const ref` actual and for a by-reference closure capture.
auto BuildReferenceArg(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId cell,
    mir::TypeId pointee) -> mir::ExprId;

// Binds a reference-typed lvalue (LRM 23.3.3.2 / 13.5.2): stores `reference`,
// a reference value, into `ref_lvalue`. This is the one canonical
// `Ref<T>`-member alias store -- a `ref` port binds the child's reference
// member, and any future reference member fills the same way. It is the
// lvalue-store placement only; a `ref` formal's call-argument placement and a
// closure's field-init placement share the reference-value construction but not
// this store. Returns the store expression for the caller to sequence.
auto BindReferenceSlot(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId ref_lvalue,
    mir::ExprId reference) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
