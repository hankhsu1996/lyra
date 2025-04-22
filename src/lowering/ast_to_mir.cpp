#include "lowering/ast_to_mir.hpp"

#include <fmt/core.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "mir/expression.hpp"
#include "mir/module.hpp"
#include "mir/statement.hpp"

namespace volans::lowering {

namespace {

auto LowerExpr(const slang::ast::Expression& expr)
    -> std::shared_ptr<mir::Expression> {
  using Kind = mir::Expression::Kind;

  auto result = std::make_shared<mir::Expression>();

  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral:
      result->kind = Kind::kLiteralInt;
      result->value =
          expr.as<slang::ast::IntegerLiteral>().getValue().as<int>().value();
      break;
    case slang::ast::ExpressionKind::StringLiteral:
      result->kind = Kind::kLiteralString;
      result->value =
          std::string(expr.as<slang::ast::StringLiteral>().getValue());
      break;
    case slang::ast::ExpressionKind::NamedValue:
      result->kind = Kind::kIdentifier;
      result->value =
          std::string(expr.as<slang::ast::NamedValueExpression>().symbol.name);
      break;
    case slang::ast::ExpressionKind::BinaryOp:
      if (expr.as<slang::ast::BinaryExpression>().op ==
          slang::ast::BinaryOperator::Add) {
        result->kind = Kind::kAdd;
        const auto& bin = expr.as<slang::ast::BinaryExpression>();
        result->value =
            std::make_pair(LowerExpr(bin.left()), LowerExpr(bin.right()));
      }
      break;
    default:
      throw std::runtime_error(fmt::format(
          "Unsupported expression kind {} in LowerExpr",
          slang::ast::toString(expr.kind)));
  }

  return result;
}

auto LowerStmt(const slang::ast::Statement& stmt)
    -> std::vector<std::shared_ptr<mir::Statement>> {
  using Kind = slang::ast::StatementKind;
  std::vector<std::shared_ptr<mir::Statement>> result;

  switch (stmt.kind) {
    case Kind::List: {
      const auto& list = stmt.as<slang::ast::StatementList>();
      for (const auto* s : list.list) {
        auto stmts = LowerStmt(*s);
        result.insert(result.end(), stmts.begin(), stmts.end());
      }
      break;
    }

    case Kind::Block: {
      const auto& block = stmt.as<slang::ast::BlockStatement>();
      auto inner_stmts = LowerStmt(block.body);
      result.insert(result.end(), inner_stmts.begin(), inner_stmts.end());
      break;
    }

    case Kind::ExpressionStatement: {
      const auto& expr_stmt = stmt.as<slang::ast::ExpressionStatement>();
      const auto& assign =
          expr_stmt.expr.as<slang::ast::AssignmentExpression>();

      auto s = std::make_shared<mir::Statement>();
      s->kind = mir::Statement::Kind::kAssign;
      s->target =
          assign.left().as<slang::ast::NamedValueExpression>().symbol.name;
      s->value = LowerExpr(assign.right());

      result.push_back(std::move(s));
      break;
    }

    default:
      throw std::runtime_error(fmt::format(
          "Unsupported statement kind {} in LowerStmt",
          slang::ast::toString(stmt.kind)));
  }

  return result;
}

}  // namespace

auto AstToMir(slang::ast::Compilation& compilation) -> mir::Module {
  mir::Module mod;

  const auto& root = compilation.getRoot();

  for (const auto& member : root.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      const auto& instance = member.as<slang::ast::InstanceSymbol>();
      mod.name = std::string(instance.name);
      const auto& body = instance.body;

      for (const auto& sym : body.members()) {
        if (sym.kind == slang::ast::SymbolKind::Variable) {
          const auto& var = sym.as<slang::ast::VariableSymbol>();
          mir::Variable v;
          v.name = var.name;
          v.type = mir::Type::kInt;
          mod.variables.push_back(v);
        }

        else if (sym.kind == slang::ast::SymbolKind::ProceduralBlock) {
          const auto& proc = sym.as<slang::ast::ProceduralBlockSymbol>();
          if (proc.procedureKind == slang::ast::ProceduralBlockKind::Initial) {
            auto p = std::make_shared<mir::Process>();
            p->kind = mir::ProcessKind::kInitial;

            const auto& stmt = proc.getBody();
            auto stmts = LowerStmt(stmt);
            p->body.insert(p->body.end(), stmts.begin(), stmts.end());

            mod.processes.push_back(std::move(p));
          }
        }
      }

      break;
    }
  }

  return mod;
}

}  // namespace volans::lowering
