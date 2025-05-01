#pragma once

#include <unordered_set>
#include <vector>

#include <fmt/format.h>
#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Symbol.h>

namespace slang::ast {
class Statement;
}

namespace lyra::lowering {

struct SensitivityCollector
    : public slang::ast::ASTVisitor<SensitivityCollector, true, true> {
  std::reference_wrapper<std::unordered_set<const slang::ast::Symbol*>> signals;

  explicit SensitivityCollector(
      std::unordered_set<const slang::ast::Symbol*>& signals)
      : signals(signals) {
  }

  void handle(const slang::ast::NamedValueExpression& expr) const {
    signals.get().insert(&expr.symbol);
  }

  void handle(const slang::ast::AssignmentExpression& expr) {
    if (expr.kind == slang::ast::ExpressionKind::Assignment) {
      const auto& assignment_expr = expr.as<slang::ast::AssignmentExpression>();
      assignment_expr.right().visit(*this);
    }
  }

  void handle(const slang::ast::BinaryExpression& expr) {
    visitDefault(expr);
  }

  void handle(const slang::ast::CallExpression& expr) {
    if (expr.isSystemCall()) {
      visitDefault(expr);
    } else {
      throw std::runtime_error(fmt::format(
          "Unsupported subroutine call {} in CollectSensitivityList",
          expr.getSubroutineName()));
    }
  }

  static void handle(const slang::ast::Expression& expr) {
    throw std::runtime_error(fmt::format(
        "Unsupported expression kind {} in CollectSensitivityList",
        slang::ast::toString(expr.kind)));
  }
};

// Collects all named signals (identifiers) that appear on the RHS
// of the given statement's expressions.
auto CollectSensitivityList(const slang::ast::Statement& statement)
    -> std::vector<const slang::ast::Symbol*>;

}  // namespace lyra::lowering
