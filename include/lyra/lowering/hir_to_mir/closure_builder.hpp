#pragma once

#include <vector>

#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/callable_bounds.hpp"

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
// fields resolves the receiver through its binding context, as it resolves any
// binding, so the builder never special-cases it.
//
// A body's call protocol is its result type, fixed when the closure is
// finished: a fork branch (LRM 9.3.2) finishes with a coroutine result, a
// synchronous body with its value's type. `policy` is the capture policy -- a
// fork branch passes the origins of its own block-item declarations as the
// snapshot set (those snapshot, deeper-enclosing bindings alias); a synchronous
// body passes the default (every forwarded binding aliases). `kind` is what
// the body does to the closure it is entered on, as rustc's closure kinds say,
// fixed here because every captured read goes through that receiver: a body
// started by its one call takes the box over (`FnOnce`), one that writes what
// it keeps borrows it mutably (`FnMut`), and any other borrows it to read
// (`Fn`).
//
// Whatever the protocol, the closure is a value of its closure type, and the
// protocol is what calling it answers. What the site then does with it --
// keep it in a box for the runtime, lend it for one call, or call it to start
// its body -- is the site's concern, not the builder's. A body that runs where
// it is written is no callable value at all and takes the block-expression
// shape instead.
// Non-movable: `Frame()` and `Bindings()` hand out references into the owned
// closure and binding context.
class ClosureBuilder {
 public:
  ClosureBuilder(
      mir::CompilationUnit& unit, const WalkFrame& enclosing,
      support::ClosureKind kind, CapturePolicy policy = {});

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
    return closure_decl_.invoke.Body();
  }
  // The body's binding context, for a caller that materializes a binding by
  // hand (a receiver for a deferred check, an explicit carrier).
  [[nodiscard]] auto Bindings() -> CallableBindings& {
    return bindings_;
  }

  // A parameter whoever calls the closure hands it on each call. It has no
  // cross-body identity: the body reaches it directly and no nested closure
  // captures it.
  auto AddParam(mir::TypeId type) -> mir::LocalId;

  // Closes the body with `return result`, finalizes the closure, and yields the
  // closure value typed as its closure type. Single-use.
  [[nodiscard]] auto Build(mir::ExprId result) -> mir::Expr;
  // Yields a closure whose body is already self-contained (no result
  // expression). Single-use.
  [[nodiscard]] auto BuildVoid() -> mir::Expr;
  // Closes the body with `co_return`, makes the closure in a box of its own
  // in the block its captures were read in, and calls it: the call hands the
  // box to the body, which owns it from there, so the body can outlive the
  // statement starting it (rustc's `call_once`). It answers the coroutine.
  // Single-use.
  [[nodiscard]] auto BuildCallOnce() -> mir::Expr;

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

// `closure`, a synchronous closure value, added to `block` as a library
// compiled before the program keeps it: made in a box of its own, since it
// outlives the statement building it and so never lies in that statement's
// frame, as Rust's thread spawn keeps the closure it is handed in a `Box`. A
// cast makes the box one of a closure of no known type, called with what its
// body takes and answering what its body answers, as the library asks of it.
// `block` is the one the closure's captures were read in.
auto BuildKeptClosure(
    mir::CompilationUnit& unit, mir::Block& block, mir::Expr closure)
    -> mir::ExprId;

// The closure lying at `closure`, a place in `block`, borrowed by a library
// compiled before the program for as long as the statement borrowing it runs:
// a reference to a closure of no known type, called as its body is, as Rust
// hands `sort_by` a `&dyn Fn`. The body reads what it captured, so it is
// called only while the closure lies where it was made.
auto BuildBorrowedClosure(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId closure)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
