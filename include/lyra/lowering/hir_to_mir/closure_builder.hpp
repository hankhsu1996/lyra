#pragma once

#include <string_view>
#include <vector>

#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// A closure under construction: a per-closure-site anonymous concrete callable
// value (captured fields plus one invoke body) that a referencing site
// constructs. The builder mints the closure identity up front, owns the closure
// being built and a child binding context whose parent is the enclosing
// callable, so a reference to an enclosing binding while the body is lowered
// through `Frame()` becomes a captured field, forwarded one boundary at a time.
// `self` is not a builder concept: a body that reaches its enclosing object's
// fields resolves the receiver through the same capture machinery as any
// binding, so the builder never special-cases it.
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
// hand out references into the owned closure and binding context.
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
    return closure_decl_.invoke.body;
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

  // Closes the body with `return result`, finalizes the closure, and yields the
  // closure value typed as its closure type. Single-use.
  [[nodiscard]] auto Build(mir::ExprId result) -> mir::Expr;
  // Closes a fork-branch body with `co_return` and yields the closure value.
  // Single-use.
  [[nodiscard]] auto BuildCoroutine() -> mir::Expr;
  // Yields a closure whose body is already self-contained (no result
  // expression). Single-use.
  [[nodiscard]] auto BuildVoid() -> mir::Expr;

 private:
  auto Finish(mir::TypeId result_type) -> mir::Expr;

  mir::CompilationUnit* unit_;
  mir::Block* outer_;
  mir::ClosureDecl closure_decl_;
  mir::ClosureId closure_id_;
  CallableBindings bindings_;
  WalkFrame frame_;
  std::vector<mir::LocalId> invocation_params_;
};

// Builds the immediately-invoked call of `closure` (the IIFE shape): adds the
// closure expression to `block` and returns a `CallExpr` over an `Indirect`
// callee referencing it, with no arguments. The call's result type comes from
// the closure's invoke.
auto BuildClosureCallExpr(
    mir::CompilationUnit& unit, mir::Block& block, mir::Expr closure)
    -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
