#pragma once

#include <memory>
#include <vector>

#include "lyra/mir/capture_id.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {

struct CallableCode;

// One initializer of a closure value's environment: at construction the code's
// capture field `target` (a `CaptureId` in the code's `captures` arena) is set
// to the value of `source`, an expression evaluated in the constructing body.
// Snapshot or alias is decided by the capture field's type, not a separate
// kind: a value-typed field owns a snapshot; a reference-typed field aliases an
// enclosing cell (`source` constructs the reference, LRM 6.21), so the body's
// reads / writes route through the live cell. The receiver `self`, when a
// closure binds it, is an ordinary capture initializer, not a privileged slot.
struct CaptureInit {
  CaptureId target{};
  ExprId source{};
};

// A callable value: callable code plus its bound environment. The environment
// is the list of capture initializers, one per field the code's `captures`
// arena declares; the code's per-invocation `params` (a with-clause `item` /
// `index`, LRM 7.12.4) are supplied by the referencing site at each call.
//
// Closures are synthesized exclusively by HIR-to-MIR lowering. No SystemVerilog
// source construct produces one directly; the environment, the code's captures
// and parameters, and the body statements are all compiler-generated.
//
// A reference inside the body is a body-local binding -- a `LocalRef` (an
// activation local / parameter) or a `CaptureRef` (an environment field). Outer
// state reaches the body only as a captured field; the body never climbs out of
// its own callable to an enclosing one.
//
// A backend realizes the value from its code and environment: a synchronous
// closure as a capture-clause lambda whose captures are the bound fields, a
// coroutine closure (a fork branch, LRM 9.3.2) as a stateless lambda whose
// fields pass as frame-copied parameters so nothing dangles when the spawned
// coroutine outlives the referencing site. The result type is
// `code.result_type`; how the closure is invoked -- called at a fork site,
// submitted as a deferred effect, iterated by a with-clause -- lives at the
// referencing site, not on the value.
struct ClosureExpr {
  std::unique_ptr<CallableCode> code;
  std::vector<CaptureInit> capture_inits;

  ClosureExpr();
  ~ClosureExpr();
  ClosureExpr(const ClosureExpr&);
  auto operator=(const ClosureExpr&) -> ClosureExpr&;
  ClosureExpr(ClosureExpr&&) noexcept;
  auto operator=(ClosureExpr&&) noexcept -> ClosureExpr&;
};

}  // namespace lyra::mir
