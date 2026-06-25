#pragma once

#include <memory>
#include <vector>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"

namespace lyra::mir {

struct CallableCode;

// One field of a closure's bound environment: at construction the body's
// `param` (a parameter of the closure's `code`, identified by `LocalId`) is
// bound to the value of `value`. Snapshot or alias is decided by the parameter
// var's type, not by a separate capture kind: a value-typed parameter owns a
// snapshot; a `RefType` parameter aliases an enclosing cell (`value` constructs
// the reference, LRM 6.21), so the body's reads / writes route through the live
// cell. The receiver `self` is the binding for `code.params[0]` -- an ordinary
// environment field, not a privileged slot.
struct EnvBinding {
  LocalId param{};
  ExprId value{};
};

// A callable value: callable code plus a bound environment. A closure binds a
// prefix of its `code`'s parameters (`self` followed by any captured locals)
// into `environment`; the remaining parameters (a with-clause `item` / `index`,
// LRM 7.12.4) are supplied per invocation by the referencing site.
//
// Closures are synthesized exclusively by HIR-to-MIR lowering. No SystemVerilog
// source construct produces one directly; the environment, the code's
// parameters, and the body statements are all compiler-generated.
//
// Invariants enforced by lowering (violations are compiler-bug class):
//   1. Each environment binding's `param` is one of `code.params`, each bound
//      at most once. The bound parameters are a prefix of `code.params`; the
//      unbound suffix is supplied per invocation.
//   2. A bound parameter whose type is a value type is read-only inside the
//      body (a snapshot). A bound parameter whose type is a `RefType` may be
//      read and written -- it aliases the enclosing storage through a `Ref<T>`.
//      An unbound (per-invocation) parameter is read-only; the value is
//      supplied at each call.
//   3. LocalRef inside the body uses hops bounded by the body's own scope
//      nesting and does not escape into the enclosing process scope. Outer
//      local state reaches the body only through the bound environment and the
//      per-invocation parameters.
//   4. The value's result type is `code.result_type`, the closure expression's
//      own type; a coroutine closure (a fork branch, LRM 9.3.2) carries the
//      coroutine type, a synchronous closure its value-result type. The body is
//      rendered from its own statements alone -- the render reads none of the
//      body's interior to discover the signature, and nothing about the
//      closure's use. A backend realizes the value from its code and
//      environment: a synchronous closure as a capture-clause lambda whose
//      captures are the bound parameters, a coroutine closure as a stateless
//      lambda whose captures pass as frame-copied parameters (so nothing
//      dangles when the spawned coroutine outlives the referencing site). How
//      the closure is invoked -- called at a fork site, submitted as a deferred
//      effect, iterated by a with-clause -- lives at the referencing site, not
//      on the value.
//
// MemberRef inside body may carry hops that reach module-scope storage
// directly, the same way C++ lambdas may reference globals without capture.
struct ClosureExpr {
  std::unique_ptr<CallableCode> code;
  std::vector<EnvBinding> environment;

  ClosureExpr();
  ~ClosureExpr();
  ClosureExpr(const ClosureExpr&);
  auto operator=(const ClosureExpr&) -> ClosureExpr&;
  ClosureExpr(ClosureExpr&&) noexcept;
  auto operator=(ClosureExpr&&) noexcept -> ClosureExpr&;
};

}  // namespace lyra::mir
