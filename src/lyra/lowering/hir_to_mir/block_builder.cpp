#include "lyra/lowering/hir_to_mir/block_builder.hpp"

#include <optional>
#include <utility>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

BlockBuilder::BlockBuilder(const WalkFrame& enclosing)
    : outer_(enclosing.current_block),
      body_{},
      frame_(enclosing.WithBlock(&body_)) {
}

auto BlockBuilder::DeclareLocal(mir::TypeId type, mir::ExprId init)
    -> mir::LocalId {
  const mir::LocalId local = frame_.bindings->DeclareAnonymous(type);
  Body().AppendStmt(mir::LocalDeclStmt{.target = local, .init = init});
  return local;
}

auto BlockBuilder::Build(mir::ExprId value) -> mir::Expr {
  // A block expression yields a value, so a place it ends with is read there.
  if (mir::IsPlace(body_.exprs.Get(value).data)) {
    value = Body().Read(value);
  }
  const mir::TypeId type = body_.exprs.Get(value).type;
  return mir::Expr{
      .data = mir::BlockExpr{.scope = Attach(), .value = value}, .type = type};
}

auto BlockBuilder::BuildStatement() -> mir::Stmt {
  return mir::Stmt{
      .label = std::nullopt, .data = mir::BlockStmt{.scope = Attach()}};
}

auto BlockBuilder::Attach() -> mir::BlockId {
  return outer_->child_scopes.Add(std::move(body_));
}

}  // namespace lyra::lowering::hir_to_mir
