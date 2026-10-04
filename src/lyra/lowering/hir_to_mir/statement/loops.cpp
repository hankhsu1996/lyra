#include "lyra/lowering/hir_to_mir/statement/loops.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// An expression a loop evaluates once per iteration -- its condition, its step
// -- lowered through steps of its own. Whatever lowering it binds is then bound
// each time it is evaluated: a statement placed in the block the loop stands in
// would run once, ahead of the loop, however many iterations follow. `finish`
// settles the value the steps yield, in their block.
template <typename Finish>
auto LowerPerIteration(
    ProcessLowerer& process, const WalkFrame& frame, hir::ExprId expr,
    const Finish& finish) -> diag::Result<mir::Expr> {
  BlockBuilder steps(frame);
  auto lowered =
      process.LowerExpr(process.HirBody().exprs.Get(expr), steps.Frame());
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  mir::Block& body = steps.Body();
  const mir::ExprId value = finish(body, body.exprs.Add(*std::move(lowered)));
  return steps.Build(value);
}

// A loop's condition, as the machine boolean the loop tests.
auto LowerLoopCondition(
    ProcessLowerer& process, const WalkFrame& frame, hir::ExprId condition)
    -> diag::Result<mir::Expr> {
  return LowerPerIteration(
      process, frame, condition, [&](mir::Block& body, mir::ExprId value) {
        return ReduceToCondition(process.Owner().Unit(), body, value);
      });
}

// The body of a loop built as one loop, which a `break` written in it leaves
// by standing inside it (LRM 12.8).
auto LowerLoopBody(
    ProcessLowerer& process, const WalkFrame& frame, hir::StmtId body)
    -> diag::Result<mir::Block> {
  return LowerStmtIntoChildScope(
      process, frame.WithBreakLeaving(std::nullopt), body);
}

}  // namespace

auto LowerForStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForStmt& f) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  auto& block = *frame.current_block;

  std::vector<mir::ForInit> mir_init;
  mir_init.reserve(f.init.size());
  for (const hir::ExprId init : f.init) {
    auto expr_or = process.LowerExpr(hir_proc.exprs.Get(init), frame);
    if (!expr_or) return std::unexpected(std::move(expr_or.error()));
    mir_init.emplace_back(
        mir::ForInitExpr{.expr = block.exprs.Add(*std::move(expr_or))});
  }

  std::optional<mir::ExprId> cond_id;
  if (f.condition.has_value()) {
    auto cond_or = LowerLoopCondition(process, frame, *f.condition);
    if (!cond_or) {
      return std::unexpected(std::move(cond_or.error()));
    }
    cond_id = block.exprs.Add(*std::move(cond_or));
  }

  std::vector<mir::ExprId> step_ids;
  step_ids.reserve(f.step.size());
  for (const hir::ExprId step_hid : f.step) {
    auto step_or = LowerPerIteration(
        process, frame, step_hid,
        [](mir::Block&, mir::ExprId value) { return value; });
    if (!step_or) {
      return std::unexpected(std::move(step_or.error()));
    }
    step_ids.push_back(block.exprs.Add(*std::move(step_or)));
  }

  auto body_or = LowerLoopBody(process, frame, f.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  const mir::BlockId body_scope_id =
      frame.current_block->child_scopes.Add(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ForStmt{
          .init = std::move(mir_init),
          .condition = cond_id,
          .step = std::move(step_ids),
          .scope = body_scope_id}};
}

auto LowerWhileStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::WhileStmt& w) -> diag::Result<mir::Stmt> {
  auto cond_or = LowerLoopCondition(process, frame, w.condition);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id =
      frame.current_block->exprs.Add(*std::move(cond_or));

  auto body_or = LowerLoopBody(process, frame, w.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  const mir::BlockId body_scope_id =
      frame.current_block->child_scopes.Add(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::WhileStmt{.condition = cond_id, .scope = body_scope_id}};
}

auto LowerDoWhileStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::DoWhileStmt& d) -> diag::Result<mir::Stmt> {
  auto body_or = LowerLoopBody(process, frame, d.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  auto cond_or = LowerLoopCondition(process, frame, d.condition);
  if (!cond_or) {
    return std::unexpected(std::move(cond_or.error()));
  }
  const mir::ExprId cond_id =
      frame.current_block->exprs.Add(*std::move(cond_or));

  const mir::BlockId body_scope_id =
      frame.current_block->child_scopes.Add(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::DoWhileStmt{.condition = cond_id, .scope = body_scope_id}};
}

namespace {

// `for (<init>; index <goes_on_while> bound; index = index <advance> 1)` over
// `body_scope`, for an `int` index. `read` answers with a read of the index
// and `write` with the expression that gives it a value, each a fresh node at
// every call, so the one builder serves an index the loop's own header
// declares and one that already stands in storage of its own.
template <typename Read, typename Write>
auto BuildIndexLoopStmt(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ForInit init,
    const Read& read, const Write& write, mir::BinaryOp goes_on_while,
    mir::ExprId bound, mir::BinaryOp advance, mir::BlockId body_scope,
    std::optional<mir::LoopLabelId> break_label) -> mir::Stmt {
  const mir::ExprId goes_on = block.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{.op = goes_on_while, .lhs = read(), .rhs = bound},
          .type = unit.builtins.bit1});
  const mir::ExprId cond_id = ReduceToCondition(unit, block, goes_on);

  const mir::ExprId next = block.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = advance,
                  .lhs = read(),
                  .rhs = BuildIntLiteral(unit, block, 1)},
          .type = unit.builtins.int_type});

  std::vector<mir::ForInit> for_init;
  for_init.push_back(std::move(init));
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ForStmt{
          .init = std::move(for_init),
          .condition = cond_id,
          .step = {write(next)},
          .scope = body_scope,
          .break_label = break_label}};
}

// The same loop over an index that stands in storage of its own, which the
// loop reaches through `index` and starts at `first`.
auto BuildStoredIndexLoopStmt(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& index,
    mir::ExprId first, mir::BinaryOp goes_on_while, mir::ExprId bound,
    mir::BinaryOp advance, mir::BlockId body_scope,
    std::optional<mir::LoopLabelId> break_label) -> mir::Stmt {
  const mir::TypeId int_type = unit.builtins.int_type;
  const auto read = [&] { return PathValue(unit, block, index); };
  const auto write = [&](mir::ExprId value) {
    return block.exprs.Add(
        BuildStoreExpr(unit, block, index, value, std::nullopt, int_type));
  };
  return BuildIndexLoopStmt(
      unit, block, mir::ForInit{mir::ForInitExpr{.expr = write(first)}}, read,
      write, goes_on_while, bound, advance, body_scope, break_label);
}

// The count a counting loop runs to, read once into a local of `block` ahead
// of the loop.
auto SampledCount(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::Block& block,
    mir::ExprId count) -> mir::ExprId {
  const mir::TypeId int_type = unit.builtins.int_type;
  if (block.exprs.Get(count).type != int_type) {
    count = block.exprs.Add(BuildValueConversion(unit, block, count, int_type));
  }
  const mir::LocalId count_var = frame.bindings->DeclareAnonymous(int_type);
  block.AppendStmt(mir::LocalDeclStmt{.target = count_var, .init = count});
  return block.exprs.Add(mir::MakeLocalRefExpr(count_var, int_type));
}

}  // namespace

auto BuildCountingLoopStmt(
    const mir::CompilationUnit& unit, WalkFrame frame, mir::Block& block,
    mir::ExprId count, mir::LocalId position, mir::BlockId body_scope,
    std::optional<mir::LoopLabelId> break_label) -> mir::Stmt {
  const mir::TypeId int_type = unit.builtins.int_type;
  const mir::ExprId bound = SampledCount(unit, frame, block, count);
  const auto read = [&] {
    return block.exprs.Add(mir::MakeLocalRefExpr(position, int_type));
  };
  const auto write = [&](mir::ExprId value) {
    return block.exprs.Add(
        mir::Expr{
            .data = mir::AssignExpr{.target = read(), .value = value},
            .type = int_type});
  };
  return BuildIndexLoopStmt(
      unit, block,
      mir::ForInit{mir::ForInitDecl{
          .induction_var = position, .init = BuildIntLiteral(unit, block, 0)}},
      read, write, mir::BinaryOp::kLessThan, bound, mir::BinaryOp::kAdd,
      body_scope, break_label);
}

auto BuildCountingLoopStmt(
    mir::CompilationUnit& unit, WalkFrame frame, mir::Block& block,
    mir::ExprId count, const AccessPath& position, mir::BlockId body_scope,
    std::optional<mir::LoopLabelId> break_label) -> mir::Stmt {
  const mir::ExprId bound = SampledCount(unit, frame, block, count);
  return BuildStoredIndexLoopStmt(
      unit, block, position, BuildIntLiteral(unit, block, 0),
      mir::BinaryOp::kLessThan, bound, mir::BinaryOp::kAdd, body_scope,
      break_label);
}

auto BuildRangeLoopStmt(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& index,
    std::int64_t left, std::int64_t right, mir::BlockId body_scope,
    std::optional<mir::LoopLabelId> break_label) -> mir::Stmt {
  const bool ascending = left <= right;
  return BuildStoredIndexLoopStmt(
      unit, block, index, BuildIntLiteral(unit, block, left),
      ascending ? mir::BinaryOp::kLessEqual : mir::BinaryOp::kGreaterEqual,
      BuildIntLiteral(unit, block, right),
      ascending ? mir::BinaryOp::kAdd : mir::BinaryOp::kSub, body_scope,
      break_label);
}

auto BuildRepeatLoopStmt(
    const mir::CompilationUnit& unit, WalkFrame frame, mir::Block& block,
    mir::ExprId count, mir::BlockId body_scope) -> mir::Stmt {
  const mir::LocalId position =
      frame.bindings->DeclareAnonymous(unit.builtins.int_type);
  return BuildCountingLoopStmt(unit, frame, block, count, position, body_scope);
}

auto LowerRepeatStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::RepeatStmt& r) -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  auto count_or =
      process.LowerExpr(process.HirBody().exprs.Get(r.count), wrapper_frame);
  if (!count_or) {
    return std::unexpected(std::move(count_or.error()));
  }
  const mir::ExprId count_id = wrapper.exprs.Add(*std::move(count_or));

  auto body_or = LowerLoopBody(process, wrapper_frame, r.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }
  const mir::BlockId body_scope_id =
      wrapper.child_scopes.Add(std::move(*body_or));

  wrapper.AppendStmt(
      BuildRepeatLoopStmt(unit, frame, wrapper, count_id, body_scope_id));

  const mir::BlockId wrapper_scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

auto LowerForeverStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ForeverStmt& f) -> diag::Result<mir::Stmt> {
  auto body_or = LowerLoopBody(process, frame, f.body);
  if (!body_or) {
    return std::unexpected(std::move(body_or.error()));
  }

  const mir::BlockId body_scope_id =
      frame.current_block->child_scopes.Add(std::move(*body_or));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id}};
}

}  // namespace lyra::lowering::hir_to_mir
