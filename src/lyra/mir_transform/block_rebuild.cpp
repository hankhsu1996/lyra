#include "lyra/mir_transform/block_rebuild.hpp"

#include <functional>
#include <optional>
#include <span>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::mir_transform {

BlockRebuild::BlockRebuild(const mir::Block& from, const RebuildRules& rules)
    : from_(&from),
      rules_(&rules),
      carried_exprs_(from.exprs.size()),
      carried_stmts_(from.stmts.size()) {
}

auto BlockRebuild::Expr(mir::ExprId old) -> mir::ExprId {
  std::optional<mir::ExprId>& carried = carried_exprs_.at(old.value);
  if (carried.has_value()) {
    return *carried;
  }
  const mir::Expr& expr = from_->exprs.Get(old);
  if (const std::optional<mir::ExprId> operand =
          rules_->stands_for(*from_, old)) {
    const mir::ExprId id = Expr(*operand);
    carried_exprs_.at(old.value) = id;
    return id;
  }
  std::optional<mir::Expr> replaced = rules_->expr(*this, old);
  mir::Expr standing =
      replaced.has_value()
          ? *std::move(replaced)
          : mir::Expr{
                .data = WithOperandsCarried(expr.data), .type = expr.type};
  standing.type = rules_->type(standing.type);
  const mir::ExprId id = Add(std::move(standing));
  carried_exprs_.at(old.value) = id;
  return id;
}

auto BlockRebuild::Evaluation(mir::ExprId old) -> mir::ExprId {
  return rules_->evaluation(*this, old);
}

auto BlockRebuild::Stmt(mir::StmtId old) -> mir::StmtId {
  std::optional<mir::StmtId>& carried = carried_stmts_.at(old.value);
  if (carried.has_value()) {
    return *carried;
  }
  const mir::Stmt& stmt = from_->stmts.Get(old);
  std::optional<mir::StmtData> replaced = rules_->stmt(*this, stmt);
  const mir::StmtId id = to_.stmts.Add(
      mir::Stmt{
          .label = stmt.label,
          .data = replaced.has_value() ? *std::move(replaced)
                                       : WithOperandsCarried(stmt.data)});
  carried_stmts_.at(old.value) = id;
  return id;
}

auto BlockRebuild::Stmts(std::span<const mir::StmtId> old)
    -> std::vector<mir::StmtId> {
  std::vector<mir::StmtId> ids;
  ids.reserve(old.size());
  for (const mir::StmtId id : old) {
    ids.push_back(Stmt(id));
  }
  return ids;
}

auto BlockRebuild::Add(mir::Expr expr) -> mir::ExprId {
  return to_.exprs.Add(std::move(expr));
}

auto BlockRebuild::Add(mir::StmtData data) -> mir::StmtId {
  return to_.stmts.Add(
      mir::Stmt{.label = std::nullopt, .data = std::move(data)});
}

auto BlockRebuild::Add(mir::Block block) -> mir::BlockId {
  return to_.child_scopes.Add(std::move(block));
}

auto BlockRebuild::From() const -> const mir::Block& {
  return *from_;
}

auto BlockRebuild::To() const -> const mir::Block& {
  return to_;
}

auto BlockRebuild::Finish(
    std::vector<mir::StmtId> root_stmts) && -> mir::Block {
  to_.root_stmts = std::move(root_stmts);
  return std::move(to_);
}

auto BlockRebuild::Child(mir::BlockId old) -> mir::BlockId {
  const mir::Block& child = from_->child_scopes.Get(old);
  BlockRebuild rebuild(child, *rules_);
  std::vector<mir::StmtId> stmts = rebuild.Stmts(child.root_stmts);
  return Add(std::move(rebuild).Finish(std::move(stmts)));
}

// A block expression's value is a node of its steps' block, which the
// statements there need not reach, so it is carried over with them.
auto BlockRebuild::BlockExprSteps(mir::BlockId old, mir::ExprId value)
    -> std::pair<mir::BlockId, mir::ExprId> {
  const mir::Block& child = from_->child_scopes.Get(old);
  BlockRebuild rebuild(child, *rules_);
  std::vector<mir::StmtId> stmts = rebuild.Stmts(child.root_stmts);
  const mir::ExprId carried = rebuild.Evaluation(value);
  return {Add(std::move(rebuild).Finish(std::move(stmts))), carried};
}

auto BlockRebuild::WithOperandsCarried(const mir::ExprData& data)
    -> mir::ExprData {
  return WithOperandsTaken(data, [this](mir::ExprId old) { return Expr(old); });
}

// A block expression's steps and the arms of a choice each begin an
// evaluation of their own; every other operand is taken where it stands.
auto BlockRebuild::WithOperandsTaken(
    const mir::ExprData& data,
    const std::function<mir::ExprId(mir::ExprId)>& take) -> mir::ExprData {
  mir::ExprData taken = data;
  if (auto* block = std::get_if<mir::BlockExpr>(&taken)) {
    std::tie(block->scope, block->value) =
        BlockExprSteps(block->scope, block->value);
    return taken;
  }
  if (auto* choice = std::get_if<mir::ConditionalExpr>(&taken)) {
    choice->condition = take(choice->condition);
    choice->then_value = Evaluation(choice->then_value);
    choice->else_value = Evaluation(choice->else_value);
    return taken;
  }
  mir::ForEachOperand(taken, [&](mir::ExprId& operand, mir::OperandUse) {
    operand = take(operand);
  });
  return taken;
}

auto BlockRebuild::WithOperandsCarried(const mir::StmtData& data)
    -> mir::StmtData {
  return std::visit(
      Overloaded{
          [](const mir::EmptyStmt& s) -> mir::StmtData { return s; },
          [&](mir::LocalDeclStmt s) -> mir::StmtData {
            s.init = Evaluation(s.init);
            return s;
          },
          [&](mir::ExprStmt s) -> mir::StmtData {
            s.expr = Evaluation(s.expr);
            return s;
          },
          [&](mir::BlockStmt s) -> mir::StmtData {
            s.scope = Child(s.scope);
            return s;
          },
          [&](mir::TryStmt s) -> mir::StmtData {
            s.body = Child(s.body);
            s.handler = Child(s.handler);
            return s;
          },
          [&](mir::RaiseStmt s) -> mir::StmtData {
            s.effect = Evaluation(s.effect);
            return s;
          },
          [&](mir::FinallyStmt s) -> mir::StmtData {
            s.body = Child(s.body);
            s.cleanup = Child(s.cleanup);
            return s;
          },
          [&](mir::IfStmt s) -> mir::StmtData {
            s.condition = Evaluation(s.condition);
            s.then_scope = Child(s.then_scope);
            if (s.else_scope.has_value()) {
              s.else_scope = Child(*s.else_scope);
            }
            return s;
          },
          [&](mir::SwitchStmt s) -> mir::StmtData {
            s.discriminant = Evaluation(s.discriminant);
            for (mir::SwitchCase& selected : s.cases) {
              selected.body = Child(selected.body);
            }
            if (s.default_body.has_value()) {
              s.default_body = Child(*s.default_body);
            }
            return s;
          },
          [&](mir::ForStmt s) -> mir::StmtData {
            for (mir::ForInit& init : s.init) {
              std::visit(
                  Overloaded{
                      [&](mir::ForInitDecl& decl) {
                        decl.init = Evaluation(decl.init);
                      },
                      [&](mir::ForInitExpr& expr) {
                        expr.expr = Evaluation(expr.expr);
                      }},
                  init);
            }
            if (s.condition.has_value()) {
              s.condition = Evaluation(*s.condition);
            }
            for (mir::ExprId& step : s.step) {
              step = Evaluation(step);
            }
            s.scope = Child(s.scope);
            return s;
          },
          [&](mir::WhileStmt s) -> mir::StmtData {
            s.condition = Evaluation(s.condition);
            s.scope = Child(s.scope);
            return s;
          },
          [&](mir::DoWhileStmt s) -> mir::StmtData {
            s.condition = Evaluation(s.condition);
            s.scope = Child(s.scope);
            return s;
          },
          [](const mir::BreakStmt& s) -> mir::StmtData { return s; },
          [](const mir::ContinueStmt& s) -> mir::StmtData { return s; },
          [&](mir::ReturnStmt s) -> mir::StmtData {
            if (s.value.has_value()) {
              s.value = Evaluation(*s.value);
            }
            return s;
          }},
      data);
}

auto RebuildCode(const mir::CallableCode& code, const RebuildRules& rules)
    -> mir::CallableCode {
  std::optional<mir::Block> body;
  if (code.body.has_value()) {
    BlockRebuild rebuild(*code.body, rules);
    std::vector<mir::StmtId> stmts = rebuild.Stmts(code.body->root_stmts);
    body = std::move(rebuild).Finish(std::move(stmts));
  }
  return mir::CallableCode{
      .params = code.params,
      .receiver = code.receiver,
      .result_type = code.result_type,
      .locals = code.locals,
      .named_locals = code.named_locals,
      .body = std::move(body)};
}

}  // namespace lyra::mir_transform
