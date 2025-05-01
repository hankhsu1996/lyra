#include "lowering/mir_to_lir/statement.hpp"

#include <stdexcept>

#include "lowering/mir_to_lir/expression.hpp"
#include "lowering/mir_to_lir/lir_builder.hpp"
#include "mir/statement.hpp"

namespace lyra::lowering {

auto LowerStatement(const mir::Statement& statement, LirBuilder& builder)
    -> void {
  switch (statement.kind) {
    case mir::Statement::Kind::kAssign: {
      const auto& assign = mir::As<mir::AssignStatement>(statement);

      const auto& target = assign.target;
      if (target.empty()) {
        throw std::runtime_error("AssignStatement has empty target");
      }

      const auto& expression = assign.value;
      if (!expression) {
        throw std::runtime_error("AssignStatement has null expression");
      }

      // Lower the expression and get its result value
      auto result_value = LowerExpression(*expression, builder);

      // Store the result to the target signal
      builder.AddInstruction(
          lir::InstructionKind::kStoreSignal, "",
          {lir::Value::MakeSignal(target), result_value});
      break;
    }

    case mir::Statement::Kind::kDelay: {
      const auto& delay = mir::As<mir::DelayStatement>(statement);
      builder.AddInstruction(
          lir::InstructionKind::kDelay, "",
          {lir::Value::MakeLiteralInt(delay.delay_amount)});
      break;
    }

    case mir::Statement::Kind::kBlock: {
      const auto& block = mir::As<mir::BlockStatement>(statement);
      for (const auto& stmt : block.statements) {
        if (stmt) {
          LowerStatement(*stmt, builder);
        }
      }
      break;
    }

    case mir::Statement::Kind::kIf: {
      throw std::runtime_error("If statement lowering not implemented yet");
      break;
    }

    case mir::Statement::Kind::kExpression: {
      const auto& expression_statement =
          mir::As<mir::ExpressionStatement>(statement);
      if (!expression_statement.expression) {
        throw std::runtime_error("ExpressionStatement has null expression");
      }

      // Lower the expression, which may produce instructions
      LowerExpression(*expression_statement.expression, builder);
      break;
    }

    default:
      throw std::runtime_error("Unsupported statement kind");
  }
}

}  // namespace lyra::lowering
