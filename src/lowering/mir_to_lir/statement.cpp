#include "lowering/mir_to_lir/statement.hpp"

#include <stdexcept>

#include "lowering/mir_to_lir/expression.hpp"
#include "lowering/mir_to_lir/lir_builder.hpp"
#include "mir/statement.hpp"

namespace lyra::lowering {

auto LowerStatement(const mir::Statement& statement, LirBuilder& builder)
    -> void {
  if (statement.kind != mir::Statement::Kind::kAssign) {
    // Skip non-assignment statements for now
    return;
  }

  const auto& target = statement.target;
  if (target.empty()) {
    throw std::runtime_error("Statement has empty target");
  }

  const auto& expression = statement.value;
  if (!expression) {
    throw std::runtime_error("Statement has null expression");
  }

  // Lower the expression and get its result value
  auto result_value = LowerExpression(*expression, builder);

  // Store the result to the target signal
  builder.AddInstruction(
      lir::InstructionKind::kStoreSignal, "",
      {lir::Value::MakeSignal(target), result_value});
}
}  // namespace lyra::lowering
