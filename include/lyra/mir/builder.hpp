#pragma once

#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// Where the nodes being built go, and the piece of source they are lowered
// from: every node added through it takes that span, unless it already names
// one, as LLVM's `IRBuilder` gives every instruction it inserts its current
// debug location. A node built outside the lowering of any source says no
// place. It holds the block by address, so it is copied freely and never
// outlives the block.
class Builder {
 public:
  Builder(Block& block, diag::SourceSpan span) : block_(&block), span_(span) {
  }

  // Another block, building from the same piece of source.
  [[nodiscard]] auto Into(Block& block) const -> Builder {
    return {block, span_};
  }

  [[nodiscard]] auto Body() const -> Block& {
    return *block_;
  }

  auto Add(Expr expr) -> ExprId {
    if (expr.span == diag::SourceSpan{}) {
      expr.span = span_;
    }
    return block_->exprs.Add(std::move(expr));
  }
  [[nodiscard]] auto Get(ExprId id) const -> const Expr& {
    return block_->exprs.Get(id);
  }
  // A block nested in this one, consumed.
  auto AddScope(Block scope) -> BlockId {
    return block_->child_scopes.Add(std::move(scope));
  }

  // The value the place `place` holds, read where it stands.
  auto Read(ExprId place) -> ExprId {
    return Add(MakeCopyExpr(place, Get(place).type));
  }

  // `id` where a value is wanted: read, where it names storage that holds a
  // value, and itself otherwise -- clang's `DefaultLvalueConversion`, for an
  // operand that may be either. Storage with no value to read stands as itself.
  auto AsValue(const TypePool& types, ExprId id) -> ExprId {
    return IsReadablePlace(types, Get(id)) ? Read(id) : id;
  }

  // The same for an expression not yet added: the read of it where it names
  // storage holding a value, and it as it stands otherwise.
  auto AsValue(const TypePool& types, Expr expr) -> Expr {
    if (!IsReadablePlace(types, expr)) {
      return expr;
    }
    const TypeId type = expr.type;
    return MakeCopyExpr(Add(std::move(expr)), type);
  }

  // The place the local `local`, of type `type`, is.
  auto LocalPlace(LocalId local, TypeId type) -> ExprId {
    return Add(MakeLocalRefExpr(local, type));
  }

  // The value the local `local`, of type `type`, holds.
  auto ReadLocal(LocalId local, TypeId type) -> ExprId {
    return Read(LocalPlace(local, type));
  }

  // What `*pointer` opens `pointer` as: a value, read where it names storage,
  // as clang reads the operand of builtin `*p`, and an owner as the object
  // C++ `*unique_ptr` enters `operator*` on.
  auto DerefOperand(const TypePool& types, ExprId pointer) -> ExprId {
    return types.Get(Get(pointer).type).OwnsPointee() ? pointer
                                                      : AsValue(types, pointer);
  }

  // The storage the capability wrapper `wrapper` represents, holding a
  // `value`: the dereference of the reference the wrapper answers when asked
  // for what it holds, the ask added here and the dereference left to add.
  auto WrapperContents(const TypePool& types, ExprId wrapper, TypeId value)
      -> Expr {
    return MakeDerefExpr(
        Add(MakeCellGetCallExpr(
            wrapper,
            types.Intern(
                Type{ReferenceType{
                    .referent = value, .mutability = Mutability::kReadOnly}}))),
        value);
  }

  // Where `wrapper` keeps what its storage held in the Preponed region of the
  // current time slot (LRM 16.5.1), reached the same way: the dereference of
  // the reference the wrapper answers to that value.
  auto SampledWrapperContents(
      const TypePool& types, ExprId wrapper, TypeId value) -> Expr {
    return MakeDerefExpr(
        Add(MakeCellSampledGetCallExpr(
            wrapper,
            types.Intern(
                Type{ReferenceType{
                    .referent = value, .mutability = Mutability::kReadOnly}}))),
        value);
  }

  // `*pointer`, the storage `pointer` reaches.
  auto Deref(const TypePool& types, ExprId pointer, TypeId referent_type)
      -> ExprId {
    return Add(MakeDerefExpr(DerefOperand(types, pointer), referent_type));
  }

  // A statement, staged and run after those already appended, so a
  // synthesized statement cannot be added to the block yet left out of what
  // it runs.
  auto AppendStmt(Stmt stmt) -> StmtId {
    if (stmt.span == diag::SourceSpan{}) {
      stmt.span = span_;
    }
    const StmtId id = block_->stmts.Add(std::move(stmt));
    block_->root_stmts.push_back(id);
    return id;
  }
  auto AppendStmt(StmtData data) -> StmtId {
    return AppendStmt(Stmt{.label = std::nullopt, .data = std::move(data)});
  }

  // Append an `if (cond) <then_body>` statement, registering `then_body` as a
  // child scope of this block and consuming it.
  auto AppendIfThen(ExprId cond, Block then_body) -> StmtId {
    const BlockId then_scope = AddScope(std::move(then_body));
    return AppendStmt(
        IfStmt{.condition = cond, .then_scope = then_scope, .else_scope = {}});
  }

  // Append `body` paired with a `cleanup` that runs on every way out of it,
  // registering both as child scopes of this block and consuming them. This is
  // the second half of an extent whose first half -- whatever entering it does
  // -- the caller has already appended here.
  auto AppendFinally(Block body, Block cleanup) -> StmtId {
    const BlockId body_id = AddScope(std::move(body));
    const BlockId cleanup_id = AddScope(std::move(cleanup));
    return AppendStmt(FinallyStmt{.body = body_id, .cleanup = cleanup_id});
  }

 private:
  Block* block_;
  diag::SourceSpan span_;
};

// What lowering the source at `span` answered: a node made there names that
// piece of source, unless a lowering inside it already named a narrower one,
// and a refusal naming no place of its own stands there too.
template <typename Node>
auto LoweredFrom(diag::Result<Node> lowered, diag::SourceSpan span)
    -> diag::Result<Node> {
  if (!lowered) {
    return diag::PlacedAt(std::move(lowered), span);
  }
  if (lowered->span == diag::SourceSpan{}) {
    lowered->span = span;
  }
  return lowered;
}

}  // namespace lyra::mir
