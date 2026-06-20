#pragma once

#include <memory>
#include <vector>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/procedural_var.hpp"

namespace lyra::mir {

struct ProceduralScope;

// Binds the body's `binding` var to the value of `value` at closure
// construction. Snapshot or alias is decided by the binding var's type, not by
// a separate capture kind (closure.md): a value-typed binding owns a snapshot;
// a `RefType` binding aliases an enclosing cell (`value` constructs the
// reference, LRM 6.21), so the body's reads / writes route through the live
// cell. The receiver `self` is always `captures[0]`.
struct Capture {
  ExprId value{};
  ProceduralVarId binding{};
};

// A per-invocation closure parameter (LRM 7.12.4 with-clause `item` / `index`).
// `binding` is a procedural-var slot in the closure body holding the argument
// value; body reads go through `ProceduralVarRef{binding}`, the same mechanism
// captures use. A capture is filled once at closure construction; a parameter
// receives a fresh value on each invocation.
struct Parameter {
  ProceduralVarId binding{};
};

// A callable value with captured state, per-invocation parameters, and a
// procedural body.
//
// Closures are synthesized exclusively by HIR-to-MIR lowering. No SystemVerilog
// source construct produces one directly; the capture list, parameter list,
// and body statements are all compiler-generated.
//
// Invariants enforced by lowering (violations are compiler-bug class):
//   1. Each capture's binding and each parameter's binding is a
//      ProceduralVarId valid in body.vars and is unique across the union of
//      the two lists.
//   2. A capture whose binding is a value type is read-only inside body (a
//      snapshot). A capture whose binding is a `RefType` may be read and
//      written -- it aliases the enclosing storage through a `Ref<T>`. A
//      parameter binding is read-only; the value is supplied per invocation.
//   3. ProceduralVarRef inside body uses hops bounded by body's own scope
//      nesting and does not escape into the enclosing process scope. Outer
//      procedural state reaches body only through captures and parameters.
//   4. The closure's result type is the closure expression's own type; a
//      coroutine closure (a fork branch, LRM 9.3.2) carries the coroutine type,
//      a synchronous closure its value-result type. The body is rendered from
//      its own statements alone -- the render reads none of the body's interior
//      to discover the signature, and nothing about the closure's use. A
//      backend realizes the closure from its captures, parameters, result type,
//      and body: a synchronous closure as a capture-clause lambda, a coroutine
//      closure as a stateless lambda whose captures pass as frame-copied
//      parameters (so nothing dangles when the spawned coroutine outlives the
//      referencing site). How the closure is invoked -- called at a fork site,
//      submitted as a deferred effect, iterated by a with-clause -- lives at
//      the referencing site, not on the closure.
//
// StructuralVarRef inside body may carry hops that reach module-scope storage
// directly, the same way C++ lambdas may reference globals without capture.
struct ClosureExpr {
  std::vector<Capture> captures;
  std::vector<Parameter> params;
  std::unique_ptr<ProceduralScope> body;

  ClosureExpr();
  ~ClosureExpr();
  ClosureExpr(const ClosureExpr&);
  auto operator=(const ClosureExpr&) -> ClosureExpr&;
  ClosureExpr(ClosureExpr&&) noexcept;
  auto operator=(ClosureExpr&&) noexcept -> ClosureExpr&;
};

}  // namespace lyra::mir
