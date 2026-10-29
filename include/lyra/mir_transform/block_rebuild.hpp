#pragma once

#include <functional>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir_transform {

struct RebuildRules;

// A block built anew from another, a node at a time. Every node the new block
// holds is added to its own pools, which name it, so a node carried over is
// named afresh and its operands by their new names; a node nothing reaches from
// the statements asked for is not carried over at all. Arenas are never edited,
// so this is how a rewrite of MIR changes a body: it builds the one that
// replaces it.
class BlockRebuild {
 public:
  BlockRebuild(const mir::Block& from, const RebuildRules& rules);

  // The new name of `old`, a node of the block rebuilt from, carrying it over
  // the first time it is asked for.
  auto Expr(mir::ExprId old) -> mir::ExprId;
  // The same for a node an evaluation of its own begins at, as the rules
  // answer it.
  auto Evaluation(mir::ExprId old) -> mir::ExprId;
  auto Stmt(mir::StmtId old) -> mir::StmtId;
  auto Stmts(std::span<const mir::StmtId> old) -> std::vector<mir::StmtId>;

  // `stmt`, built by the statement rule while it answers a statement of the
  // block rebuilt from, to run just before that statement in the same block,
  // so what it declares the statement can name, as rustc places a
  // temporary's `StorageLive` among the statements of the block whose
  // statement uses it.
  void Precede(mir::StmtId stmt);

  // A node the rewrite builds, which the block it is built in names.
  auto Add(mir::Expr expr) -> mir::ExprId;
  auto Add(mir::StmtData data) -> mir::StmtId;
  auto Add(mir::Block block) -> mir::BlockId;

  // `data`, a node of the block rebuilt from, with each of its operands
  // replaced by what `take` answers for it, a node of the block being built.
  // An arm of a choice and the value of a block expression begin evaluations
  // of their own, which the rules answer rather than `take`.
  auto WithOperandsTaken(
      const mir::ExprData& data,
      const std::function<mir::ExprId(mir::ExprId)>& take) -> mir::ExprData;
  // `data`, a statement of the block rebuilt from, with its expressions and
  // blocks carried over as the rules answer them, the statement rule aside, so
  // a rule building something around a statement can carry the statement
  // inside it.
  auto WithOperandsCarried(const mir::StmtData& data) -> mir::StmtData;

  [[nodiscard]] auto From() const -> const mir::Block&;
  // The block being built, which names every node added so far, so a builder
  // reading the operands of a node it builds reads them here.
  [[nodiscard]] auto To() const -> const mir::Block&;

  // The block, running `root_stmts`.
  [[nodiscard]] auto Finish(
      std::vector<mir::StmtId> root_stmts) && -> mir::Block;

 private:
  auto Child(mir::BlockId old) -> mir::BlockId;
  auto BlockExprSteps(mir::BlockId old, mir::ExprId value)
      -> std::pair<mir::BlockId, mir::ExprId>;
  auto WithOperandsCarried(const mir::ExprData& data) -> mir::ExprData;

  const mir::Block* from_;
  const RebuildRules* rules_;
  mir::Block to_;
  std::vector<std::optional<mir::ExprId>> carried_exprs_;
  std::vector<std::optional<mir::StmtId>> carried_stmts_;
  // What runs before the statement being answered, placed when it is.
  std::vector<mir::StmtId> preceding_;
};

// What a rewrite says about the nodes of a block being rebuilt. Each is asked
// of every node the rebuild reaches, in every block nested in it, by the id it
// has in the block rebuilt from, since what a node may become can depend on
// what takes it. It answers the node standing in its place -- built from new
// ids, through the rebuild -- or nothing, for the node to be carried over as
// it is. A node that is one of its operands is answered by naming that
// operand, a node of the same block, which then stands in its place, as LLVM's
// InstSimplify answers with a value that already exists. `type` answers the
// type each node standing there is stated at, carried over or not.
struct RebuildRules {
  std::function<std::optional<mir::ExprId>(const mir::Block&, mir::ExprId)>
      stands_for = [](const mir::Block&, mir::ExprId) { return std::nullopt; };
  std::function<std::optional<mir::Expr>(BlockRebuild&, mir::ExprId)> expr =
      [](BlockRebuild&, mir::ExprId) { return std::nullopt; };
  std::function<std::optional<mir::StmtData>(BlockRebuild&, const mir::Stmt&)>
      stmt = [](BlockRebuild&, const mir::Stmt&) { return std::nullopt; };
  // Asked of each node an evaluation of its own begins at, answering what
  // stands there, so a rewrite can give that evaluation steps of its own: a
  // statement's expression, which is a full-expression; an arm of a choice,
  // which runs only when chosen; and the value a block expression ends with,
  // which runs after its steps.
  std::function<mir::ExprId(BlockRebuild&, mir::ExprId)> evaluation =
      [](BlockRebuild& rebuild, mir::ExprId old) { return rebuild.Expr(old); };
  // Asked of each statement's expression whose value the statement discards --
  // an expression statement's, a loop's step, a loop's initialization that
  // declares nothing -- answering what stands there, which may be a value made
  // somewhere the statement discards with it. Each begins an evaluation of its
  // own, as the rule above answers it, unless a rewrite says otherwise.
  std::function<mir::ExprId(BlockRebuild&, mir::ExprId)> discarded =
      [](BlockRebuild& rebuild, mir::ExprId old) {
        return rebuild.Evaluation(old);
      };
  std::function<mir::TypeId(mir::TypeId)> type = [](mir::TypeId type) {
    return type;
  };
};

// `code` with its body rebuilt under `rules` from its statements. Code with no
// body has nothing to rebuild and is carried over as it is.
[[nodiscard]] auto RebuildCode(
    const mir::CallableCode& code, const RebuildRules& rules)
    -> mir::CallableCode;

}  // namespace lyra::mir_transform
