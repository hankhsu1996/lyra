#pragma once

#include "lyra/lowering/hir_to_mir/capture_sink.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// A synchronous closure under construction (closure.md). The constructor opens
// a fresh body scope with `self` as captures[0] and installs a capture sink, so
// a read of an enclosing variable while lowering the body becomes a capture
// (LRM 6.21). The caller lowers the body through `Frame()` into `Body()`, then
// `Build()` closes it with the result `return` and yields the closure value.
//
// A closure is a value; how it is invoked -- an immediately-invoked call
// (`BuildClosureCallExpr`), a fork spawn, a deferred submit -- is the caller's
// concern, not the builder's (mir.md). Non-movable: `Frame()` hands out a frame
// that points into the owned body scope and capture sink.
class ClosureBuilder {
 public:
  ClosureBuilder(
      mir::CompilationUnit& unit, const WalkFrame& enclosing,
      mir::TypeId result_type);

  ClosureBuilder(const ClosureBuilder&) = delete;
  auto operator=(const ClosureBuilder&) -> ClosureBuilder& = delete;
  ClosureBuilder(ClosureBuilder&&) = delete;
  auto operator=(ClosureBuilder&&) -> ClosureBuilder& = delete;
  ~ClosureBuilder() = default;

  // The frame to lower the body through: the body scope is current, `self` is
  // bound, and the capture sink intercepts enclosing-variable reads.
  [[nodiscard]] auto Frame() const -> const WalkFrame& {
    return frame_;
  }
  // The body scope to append statements / locals into.
  [[nodiscard]] auto Body() -> mir::ProceduralScope& {
    return body_;
  }

  // Closes the body with `return result`, assembles the captures (self plus the
  // sink's), and yields the closure value. Single-use.
  [[nodiscard]] auto Build(mir::ExprId result) -> mir::Expr;

 private:
  mir::CompilationUnit* unit_;
  mir::ProceduralScope* outer_;
  mir::TypeId result_type_;
  mir::ProceduralScope body_;
  mir::ProceduralVarId self_binding_{};
  mir::ExprId outer_self_read_{};
  CaptureSink sink_;
  WalkFrame frame_;
};

// Builds the immediately-invoked call of `closure` (the IIFE shape): adds the
// closure expression to `scope` and returns a `CallExpr` over a `ClosureRef` to
// it, with no arguments. The call's type is the closure's result type.
auto BuildClosureCallExpr(mir::ProceduralScope& scope, mir::Expr closure)
    -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
