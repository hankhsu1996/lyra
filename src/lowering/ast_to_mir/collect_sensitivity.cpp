#include "lowering/ast_to_mir/collect_sensitivity.hpp"

namespace lyra::lowering {

auto CollectSensitivityList(const slang::ast::Statement& statement)
    -> std::vector<const slang::ast::Symbol*> {
  std::unordered_set<const slang::ast::Symbol*> signals;

  auto visit_statement = [&](const slang::ast::Statement& statement,
                             auto&& self) -> void {
    using Kind = slang::ast::StatementKind;

    switch (statement.kind) {
      case Kind::List: {
        const auto& statement_list = statement.as<slang::ast::StatementList>();
        for (const auto* statement : statement_list.list) {
          self(*statement, self);
        }
        break;
      }
      case Kind::Block: {
        const auto& block_statement =
            statement.as<slang::ast::BlockStatement>();
        self(block_statement.body, self);
        break;
      }
      case Kind::ExpressionStatement: {
        const auto& expression_statement =
            statement.as<slang::ast::ExpressionStatement>();
        SensitivityCollector collector(signals);
        expression_statement.expr.visit(collector);
        break;
      }
      default:
        throw std::runtime_error(fmt::format(
            "Unsupported statement kind {} in CollectSensitivityList",
            slang::ast::toString(statement.kind)));
    }
  };

  visit_statement(statement, visit_statement);

  return {signals.begin(), signals.end()};
}

}  // namespace lyra::lowering
