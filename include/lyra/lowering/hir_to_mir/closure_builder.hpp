#pragma once

#include <string_view>
#include <vector>

#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// A closure under construction. A closure is a callable value: callable code (a
// signature, a result type, a body) plus a bound environment. The builder owns
// the code and a child binding context whose parent is the enclosing callable,
// so a reference to an enclosing binding while the body is lowered through
// `Frame()` becomes a captured environment field, forwarded one boundary at a
// time. `self` is not a builder concept: a body that reaches its enclosing
// object's members resolves the receiver through the same capture machinery as
// any binding, so the builder never special-cases it.
//
// `coroutine` selects a fork-branch body (LRM 9.3.2): its `return`s render as
// `co_return` and the terminal is `BuildCoroutine`. `policy` is the capture
// policy -- a fork branch passes the origins of its own block-item declarations
// as the snapshot set (those snapshot, deeper-enclosing bindings alias); a
// synchronous body passes the default (every forwarded binding aliases).
//
// A closure is a value; how it is invoked -- an immediately-invoked call, a
// fork spawn, a deferred submit, a with-clause iteration -- is the referencing
// site's concern, not the builder's. Non-movable: `Frame()` and `Bindings()`
// hand out references into the owned code and binding context.
class ClosureBuilder {
 public:
  ClosureBuilder(
      mir::CompilationUnit& unit, const WalkFrame& enclosing,
      bool coroutine = false, CapturePolicy policy = {});

  ClosureBuilder(const ClosureBuilder&) = delete;
  auto operator=(const ClosureBuilder&) -> ClosureBuilder& = delete;
  ClosureBuilder(ClosureBuilder&&) = delete;
  auto operator=(ClosureBuilder&&) -> ClosureBuilder& = delete;
  ~ClosureBuilder() = default;

  // The frame to lower the body through: the body block is current and the
  // child binding context resolves enclosing references as captures.
  [[nodiscard]] auto Frame() const -> const WalkFrame& {
    return frame_;
  }
  // The body block to append statements / exprs into.
  [[nodiscard]] auto Body() -> mir::Block& {
    return code_.body;
  }
  // The body's binding context, for a caller that materializes a binding by
  // hand (a receiver for a deferred check, an explicit carrier).
  [[nodiscard]] auto Bindings() -> CallableBindings& {
    return bindings_;
  }

  // Declares a per-invocation parameter (LRM 7.12.4 with-clause) with its
  // cross-body origin, so a nested clause can capture it. Allocates the body
  // slot and records the parameter; returns its binding.
  auto AddParam(BindingOriginId origin, std::string_view name, mir::TypeId type)
      -> mir::LocalId;

  // Declares a per-invocation parameter with no cross-body identity: the body
  // names it directly and no nested closure can capture it (a built-in array
  // reduction whose body is the bare element, with no with-clause).
  auto AddParamAnonymous(std::string_view name, mir::TypeId type)
      -> mir::LocalId;

  // Snapshots a computed outer-scope expression into the body by value:
  // captures it as an environment field and returns the body-side read. An
  // integer literal clones verbatim so the body still sees a literal (no field
  // is made). Used by closures that build their environment by hand (the
  // non-blocking-assignment place / operand snapshots).
  auto CaptureByValue(mir::ExprId outer_id, std::string_view name)
      -> mir::ExprId;

  // Closes the body with `return result`, assembles the environment, and yields
  // the closure value typed as the result expression. Single-use.
  [[nodiscard]] auto Build(mir::ExprId result) -> mir::Expr;
  // Closes a fork-branch body with `co_return` and yields the coroutine-typed
  // closure value. Single-use.
  [[nodiscard]] auto BuildCoroutine() -> mir::Expr;
  // Yields a void-typed closure whose body is already self-contained (no result
  // expression). Single-use.
  [[nodiscard]] auto BuildVoid() -> mir::Expr;

 private:
  auto Finish(mir::TypeId result_type) -> mir::Expr;

  mir::CompilationUnit* unit_;
  mir::Block* outer_;
  mir::CallableCode code_;
  CallableBindings bindings_;
  WalkFrame frame_;
  std::vector<mir::LocalId> invocation_params_;
};

// Builds the immediately-invoked call of `closure` (the IIFE shape): adds the
// closure expression to `block` and returns a `CallExpr` over an `Indirect`
// callee referencing it, with no arguments. The call's type is the closure's
// result type.
auto BuildClosureCallExpr(mir::Block& block, mir::Expr closure) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
