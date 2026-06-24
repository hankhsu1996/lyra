#pragma once

#include <optional>
#include <string_view>
#include <vector>

#include "lyra/lowering/hir_to_mir/block_depth.hpp"
#include "lyra/lowering/hir_to_mir/capture_sink.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {
class CompilationUnit;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// A closure under construction. The constructor opens a fresh body scope
// with `self` as captures[0] and installs a capture sink, so a read of an
// enclosing variable while lowering the body through `Frame()` becomes a
// capture (LRM 6.21). A site that authors the body by hand instead of
// lowering HIR snapshots specific outer expressions with `CaptureByValue`;
// the two capture paths coexist, and `self` is always first.
//
// `coroutine` selects a fork-branch body (LRM 9.3.2): its `return`s render
// as `co_return` and the terminal is `BuildCoroutine`. `by_value_depth` is
// the sink snapshot boundary -- a capture declared at that depth is
// snapshotted by value, any deeper-enclosing capture aliases the live cell;
// pass it for a fork branch (block-item locals snapshot) and leave it empty
// for a synchronous body (every sink capture aliases).
//
// A closure is a value; how it is invoked -- an immediately-invoked call, a
// fork spawn, a deferred submit, a with-clause iteration -- is the caller's
// concern, not the builder's. Non-movable: `Frame()` hands out a frame that
// points into the owned body scope and sink.
class ClosureBuilder {
 public:
  ClosureBuilder(
      mir::CompilationUnit& unit, const WalkFrame& enclosing,
      bool coroutine = false,
      std::optional<BlockDepth> by_value_depth = std::nullopt);

  ClosureBuilder(const ClosureBuilder&) = delete;
  auto operator=(const ClosureBuilder&) -> ClosureBuilder& = delete;
  ClosureBuilder(ClosureBuilder&&) = delete;
  auto operator=(ClosureBuilder&&) -> ClosureBuilder& = delete;
  ~ClosureBuilder() = default;

  // The frame to lower the body through: the body scope is current, `self` is
  // bound, and the sink intercepts enclosing-variable reads. A caller that
  // needs a parameter to play a frame-level role (an `item.index` index)
  // decorates this frame at its own lowering call site.
  [[nodiscard]] auto Frame() const -> const WalkFrame& {
    return frame_;
  }
  // The body scope to append statements / locals into.
  [[nodiscard]] auto Body() -> mir::Block& {
    return body_;
  }
  // The `self` receiver binding (captures[0]), for a hand-authored body that
  // builds its own `self` reads.
  [[nodiscard]] auto SelfBinding() const -> mir::LocalId {
    return self_binding_;
  }

  // Declares a per-invocation parameter (LRM 7.12.4 with-clause). Allocates the
  // body slot and records the parameter; the caller maps any HIR iterator to
  // the returned binding before lowering the body. Any role a parameter plays
  // in the body -- a with-clause iterator, the `item.index` index -- is the
  // caller's to wire onto the frame it lowers through, not the builder's.
  auto AddParam(std::string_view name, mir::TypeId type) -> mir::LocalId;

  // Snapshots an outer-scope expression into the body by value: allocates a
  // body binding, records a by-value capture of `outer_id`, and returns the
  // body-side read. An integer literal clones verbatim so the body still sees a
  // literal (constant-extracting passes depend on that shape) and no capture is
  // made.
  auto CaptureByValue(mir::ExprId outer_id, std::string_view name)
      -> mir::ExprId;

  // Closes the body with `return result`, assembles the captures, and yields
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
  mir::Block body_;
  mir::LocalId self_binding_{};
  CaptureSink sink_;
  WalkFrame frame_;
  std::vector<mir::Capture> captures_;
  std::vector<mir::Parameter> params_;
};

// Builds the immediately-invoked call of `closure` (the IIFE shape): adds the
// closure expression to `block` and returns a `CallExpr` over a `ClosureRef` to
// it, with no arguments. The call's type is the closure's result type.
auto BuildClosureCallExpr(mir::Block& block, mir::Expr closure) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
