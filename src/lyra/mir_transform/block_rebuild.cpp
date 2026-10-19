#include "lyra/mir_transform/block_rebuild.hpp"

#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
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
  const mir::ExprId carried = rebuild.Expr(value);
  return {Add(std::move(rebuild).Finish(std::move(stmts))), carried};
}

auto BlockRebuild::WithOperandsCarried(const mir::ExprData& data)
    -> mir::ExprData {
  const auto each = [&](std::vector<mir::ExprId> ids) {
    for (mir::ExprId& id : ids) {
      id = Expr(id);
    }
    return ids;
  };
  return std::visit(
      Overloaded{
          [](const mir::StringLiteral& e) -> mir::ExprData { return e; },
          [](const mir::NullLiteral& e) -> mir::ExprData { return e; },
          [](const mir::MachineBoolLiteral& e) -> mir::ExprData { return e; },
          [](const mir::MachineIntLiteral& e) -> mir::ExprData { return e; },
          [](const mir::MachineFloatLiteral& e) -> mir::ExprData { return e; },
          [](const mir::ReferenceExpr& e) -> mir::ExprData { return e; },
          [&](mir::UnaryExpr e) -> mir::ExprData {
            e.operand = Expr(e.operand);
            return e;
          },
          [&](mir::BinaryExpr e) -> mir::ExprData {
            e.lhs = Expr(e.lhs);
            e.rhs = Expr(e.rhs);
            return e;
          },
          [&](mir::CastExpr e) -> mir::ExprData {
            e.operand = Expr(e.operand);
            return e;
          },
          [&](mir::ConditionalExpr e) -> mir::ExprData {
            e.condition = Expr(e.condition);
            e.then_value = Expr(e.then_value);
            e.else_value = Expr(e.else_value);
            return e;
          },
          [&](mir::BlockExpr e) -> mir::ExprData {
            std::tie(e.scope, e.value) = BlockExprSteps(e.scope, e.value);
            return e;
          },
          [&](mir::AssignExpr e) -> mir::ExprData {
            e.target = Expr(e.target);
            e.value = Expr(e.value);
            return e;
          },
          [&](mir::CallExpr e) -> mir::ExprData {
            std::visit(
                Overloaded{
                    [&](mir::Direct& callee) {
                      if (callee.receiver.has_value()) {
                        callee.receiver = Expr(*callee.receiver);
                      }
                    },
                    [&](mir::Indirect& callee) {
                      callee.code = Expr(callee.code);
                    },
                    [](mir::Construct&) {},
                    [&](mir::Virtual& callee) {
                      callee.receiver = Expr(callee.receiver);
                    }},
                e.callee);
            e.arguments = each(std::move(e.arguments));
            return e;
          },
          [&](mir::NewExpr e) -> mir::ExprData {
            e.initializer = Expr(e.initializer);
            return e;
          },
          [&](mir::DerefExpr e) -> mir::ExprData {
            e.pointer = Expr(e.pointer);
            return e;
          },
          [&](mir::AddressOfExpr e) -> mir::ExprData {
            e.operand = Expr(e.operand);
            return e;
          },
          [&](mir::BorrowExpr e) -> mir::ExprData {
            e.operand = Expr(e.operand);
            return e;
          },
          [&](mir::MoveExpr e) -> mir::ExprData {
            e.operand = Expr(e.operand);
            return e;
          },
          [&](mir::CopyExpr e) -> mir::ExprData {
            e.place = Expr(e.place);
            return e;
          },
          [&](mir::DiscriminantExpr e) -> mir::ExprData {
            e.tagged = Expr(e.tagged);
            return e;
          },
          [&](mir::FieldAccessExpr e) -> mir::ExprData {
            e.receiver = Expr(e.receiver);
            return e;
          },
          [&](mir::ClosureExpr e) -> mir::ExprData {
            for (mir::FieldInit& init : e.field_inits) {
              init.value = Expr(init.value);
            }
            return e;
          },
          [&](mir::CompositeExpr e) -> mir::ExprData {
            e.parts = each(std::move(e.parts));
            return e;
          },
          [&](mir::UnionExpr e) -> mir::ExprData {
            e.value = Expr(e.value);
            return e;
          },
          [&](mir::AwaitExpr e) -> mir::ExprData {
            e.execution = Expr(e.execution);
            return e;
          },
          [&](mir::WaitExpr e) -> mir::ExprData {
            e.park = Expr(e.park);
            return e;
          },
          [&](mir::VectorGetExpr e) -> mir::ExprData {
            e.vector = Expr(e.vector);
            e.index = Expr(e.index);
            return e;
          }},
      data);
}

auto BlockRebuild::WithOperandsCarried(const mir::StmtData& data)
    -> mir::StmtData {
  return std::visit(
      Overloaded{
          [](const mir::EmptyStmt& s) -> mir::StmtData { return s; },
          [&](mir::LocalDeclStmt s) -> mir::StmtData {
            s.init = Expr(s.init);
            return s;
          },
          [&](mir::ExprStmt s) -> mir::StmtData {
            s.expr = Expr(s.expr);
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
            s.effect = Expr(s.effect);
            return s;
          },
          [&](mir::FinallyStmt s) -> mir::StmtData {
            s.body = Child(s.body);
            s.cleanup = Child(s.cleanup);
            return s;
          },
          [&](mir::IfStmt s) -> mir::StmtData {
            s.condition = Expr(s.condition);
            s.then_scope = Child(s.then_scope);
            if (s.else_scope.has_value()) {
              s.else_scope = Child(*s.else_scope);
            }
            return s;
          },
          [&](mir::ForStmt s) -> mir::StmtData {
            for (mir::ForInit& init : s.init) {
              std::visit(
                  Overloaded{
                      [&](mir::ForInitDecl& decl) {
                        decl.init = Expr(decl.init);
                      },
                      [&](mir::ForInitExpr& expr) {
                        expr.expr = Expr(expr.expr);
                      }},
                  init);
            }
            if (s.condition.has_value()) {
              s.condition = Expr(*s.condition);
            }
            for (mir::ExprId& step : s.step) {
              step = Expr(step);
            }
            s.scope = Child(s.scope);
            return s;
          },
          [&](mir::WhileStmt s) -> mir::StmtData {
            s.condition = Expr(s.condition);
            s.scope = Child(s.scope);
            return s;
          },
          [&](mir::DoWhileStmt s) -> mir::StmtData {
            s.condition = Expr(s.condition);
            s.scope = Child(s.scope);
            return s;
          },
          [](const mir::BreakStmt& s) -> mir::StmtData { return s; },
          [](const mir::ContinueStmt& s) -> mir::StmtData { return s; },
          [&](mir::ReturnStmt s) -> mir::StmtData {
            if (s.value.has_value()) {
              s.value = Expr(*s.value);
            }
            return s;
          }},
      data);
}

auto RebuildCode(
    const mir::CallableCode& code, std::span<const mir::ExprId> outside_roots,
    const RebuildRules& rules) -> mir::SourcedCode {
  std::vector<mir::ExprId> roots(outside_roots.begin(), outside_roots.end());
  std::optional<mir::Block> body;
  if (code.body.has_value()) {
    BlockRebuild rebuild(*code.body, rules);
    std::vector<mir::StmtId> stmts = rebuild.Stmts(code.body->root_stmts);
    for (mir::ExprId& root : roots) {
      root = rebuild.Expr(root);
    }
    body = std::move(rebuild).Finish(std::move(stmts));
  }
  return mir::SourcedCode{
      .code =
          mir::CallableCode{
              .params = code.params,
              .receiver = code.receiver,
              .result_type = code.result_type,
              .locals = code.locals,
              .named_locals = code.named_locals,
              .body = std::move(body)},
      .outside_roots = std::move(roots)};
}

}  // namespace lyra::mir_transform
