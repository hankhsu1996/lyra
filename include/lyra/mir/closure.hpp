#pragma once

#include <memory>
#include <variant>
#include <vector>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/procedural_var.hpp"

namespace lyra::mir {

struct ProceduralScope;

struct ByValueCapture {
  ExprId value{};
  ProceduralVarId binding{};
};

// A by-reference capture binds the body's `binding` var to an enclosing-scope
// lvalue (`target`). The spawned body reaches it through a frame-copied
// reference parameter; the enclosing storage outlives the body (LRM 6.21), so
// reads and writes share the one live location. Used by fork branches that
// touch their enclosing process's variables and by synchronous IIFEs (e.g.
// `$sscanf` / `$fscanf`) whose body writes back to its output args.
struct ByReferenceCapture {
  ExprId target{};
  ProceduralVarId binding{};
};

using Capture = std::variant<ByValueCapture, ByReferenceCapture>;

// A per-invocation closure parameter (LRM 7.12.4 with-clause `item` /
// `index`). `binding` is a procedural-var slot in the closure body that
// holds the per-call argument value; body reads go through
// `ProceduralVarRef{binding}` -- the same mechanism captures use. The
// difference between a `Capture` and a `Parameter` is when the binding is
// filled: captures snapshot once at closure creation, parameters receive a
// new value on each invocation.
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
//   2. A ByValueCapture binding is read-only inside body. A ByReferenceCapture
//      binding may be read and written -- it aliases the enclosing storage.
//      A parameter binding is read-only inside body; the value is supplied
//      by the caller per invocation.
//   3. ProceduralVarRef inside body uses hops bounded by body's own scope
//      nesting and does not escape into the enclosing process scope. Outer
//      procedural state reaches body only through captures.
//   4. A closure with non-empty `params` is invoked many times (each call
//      supplies new parameter values) and renders as a lambda whose `(...)`
//      clause lists the parameters and whose body ends with a `ReturnStmt`
//      carrying the result expression. An empty-`params` closure is invoked
//      once (fork branch / deferred IIFE) and renders as a captures-as-args
//      IIFE.
//   5. `is_coroutine` declares whether the body may suspend (LRM 9.4 timing
//      controls / event waits) -- equivalent to asking "is the body lowered
//      as a C++20 coroutine vs a plain lambda". A fork branch (LRM 9.3.2)
//      spawns its body as a concurrent coroutine; all other closure shapes
//      (NBA submit / `$strobe` / `$sscanf` IIFE / with-clause iterator /
//      deferred check) run their body synchronously without suspending.
//      HIR-to-MIR encodes the value at construction; the backend renders
//      the body purely by reading it -- no surrounding context lookup.
//
// StructuralVarRef inside body may carry hops that reach module-scope storage
// directly, the same way C++ lambdas may reference globals without capture.
struct ClosureExpr {
  std::vector<Capture> captures;
  std::vector<Parameter> params;
  std::unique_ptr<ProceduralScope> body;
  bool is_coroutine = false;

  ClosureExpr();
  ~ClosureExpr();
  ClosureExpr(const ClosureExpr&);
  auto operator=(const ClosureExpr&) -> ClosureExpr&;
  ClosureExpr(ClosureExpr&&) noexcept;
  auto operator=(ClosureExpr&&) noexcept -> ClosureExpr&;
};

}  // namespace lyra::mir
