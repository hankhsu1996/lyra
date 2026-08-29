#pragma once

#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// A run of steps under construction: a child scope of the enclosing block,
// lowered through the enclosing callable's own binding context. A construct
// reaches for one when its evaluation takes several steps -- an SV subroutine
// call that writes back to its actuals, a system function that both yields a
// value and has an effect -- and finishes it as the expression those steps
// yield, or as the statement they amount to when they yield nothing.
//
// Nothing crosses a callable boundary, so a local declared inside is an
// ordinary local of the enclosing body and a reference out of it resolves with
// no capture. The steps do not return, either: a run sequences and yields, with
// no control-flow effect, so a construct whose answer depends on a test states
// that answer as a value the steps settle, never as an exit from the middle.
//
// Non-movable: `Frame()` and `Body()` hand out references into the block being
// built.
class BlockBuilder {
 public:
  explicit BlockBuilder(const WalkFrame& enclosing);

  BlockBuilder(const BlockBuilder&) = delete;
  auto operator=(const BlockBuilder&) -> BlockBuilder& = delete;
  BlockBuilder(BlockBuilder&&) = delete;
  auto operator=(BlockBuilder&&) -> BlockBuilder& = delete;
  ~BlockBuilder() = default;

  // The frame to lower the steps through: the block is current, and bindings
  // are the enclosing callable's.
  [[nodiscard]] auto Frame() const -> const WalkFrame& {
    return frame_;
  }
  [[nodiscard]] auto Body() -> mir::Block& {
    return body_;
  }
  // The enclosing callable's binding context, for a caller that declares a
  // local of its own.
  [[nodiscard]] auto Bindings() const -> CallableBindings& {
    return *frame_.bindings;
  }

  // Attaches the block to the enclosing one and yields the expression, typed as
  // the value it ends with. Single-use.
  [[nodiscard]] auto Build(mir::ExprId value) -> mir::Expr;
  // The same for steps that settle no value, which are a statement rather than
  // an expression: only a value standing where the grammar admits one needs the
  // expression form. Single-use.
  [[nodiscard]] auto BuildStatement() -> mir::Stmt;

 private:
  [[nodiscard]] auto Attach() -> mir::BlockId;

  mir::Block* outer_;
  mir::Block body_;
  WalkFrame frame_;
};

}  // namespace lyra::lowering::hir_to_mir
