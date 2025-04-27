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

    default:
      throw std::runtime_error("Unsupported statement kind");
  }
}

}  // namespace lyra::lowering
