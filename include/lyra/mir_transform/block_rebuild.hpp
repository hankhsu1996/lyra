#pragma once

#include <functional>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/mir/block_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::mir_transform {

class BlockRebuild;

// What a rewrite says about the nodes of a block being rebuilt. Each is asked
// of every node the rebuild reaches, in every block nested in it, by the id it
// has in the block rebuilt from, since what a node may become can depend on
// what takes it. It answers the node standing in its place -- built from new
// ids, through the rebuild -- or nothing, for the node to be carried over as
// it is. A node that is one of its operands is answered by naming that
// operand, a node of the same block, which then stands in its place, as LLVM's
// InstSimplify answers with a value that already exists.
struct RebuildRules {
  std::function<std::optional<mir::ExprId>(const mir::Block&, mir::ExprId)>
      stands_for = [](const mir::Block&, mir::ExprId) { return std::nullopt; };
  std::function<std::optional<mir::Expr>(BlockRebuild&, mir::ExprId)> expr =
      [](BlockRebuild&, mir::ExprId) { return std::nullopt; };
  std::function<std::optional<mir::StmtData>(BlockRebuild&, const mir::Stmt&)>
      stmt = [](BlockRebuild&, const mir::Stmt&) { return std::nullopt; };
};

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
  auto Stmt(mir::StmtId old) -> mir::StmtId;
  auto Stmts(std::span<const mir::StmtId> old) -> std::vector<mir::StmtId>;

  // A node the rewrite builds, which the block it is built in names.
  auto Add(mir::Expr expr) -> mir::ExprId;
  auto Add(mir::StmtData data) -> mir::StmtId;
  auto Add(mir::Block block) -> mir::BlockId;

  [[nodiscard]] auto From() const -> const mir::Block&;

  // The block, running `root_stmts`.
  [[nodiscard]] auto Finish(
      std::vector<mir::StmtId> root_stmts) && -> mir::Block;

 private:
  auto Child(mir::BlockId old) -> mir::BlockId;
  auto BlockExprSteps(mir::BlockId old, mir::ExprId value)
      -> std::pair<mir::BlockId, mir::ExprId>;
  auto WithOperandsCarried(const mir::ExprData& data) -> mir::ExprData;
  auto WithOperandsCarried(const mir::StmtData& data) -> mir::StmtData;

  const mir::Block* from_;
  const RebuildRules* rules_;
  mir::Block to_;
  std::vector<std::optional<mir::ExprId>> carried_exprs_;
  std::vector<std::optional<mir::StmtId>> carried_stmts_;
};

}  // namespace lyra::mir_transform
