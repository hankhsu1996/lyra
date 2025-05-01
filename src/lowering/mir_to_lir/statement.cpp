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

    case mir::Statement::Kind::kConditional: {
      const auto& if_stmt = mir::As<mir::ConditionalStatement>(statement);

      if (!if_stmt.condition) {
        throw std::runtime_error("If statement has null condition");
      }

      if (!if_stmt.then_branch) {
        throw std::runtime_error("If statement has null then branch");
      }

      // Generate unique labels for the various blocks
      static int if_counter = 0;
      std::string if_id = std::to_string(if_counter++);

      std::string then_label = "if." + if_id + ".then";
      std::string else_label = "if." + if_id + ".else";
      std::string end_label = "if." + if_id + ".end";

      // Evaluate the condition
      auto condition_value = LowerExpression(*if_stmt.condition, builder);

      // Create a branch instruction
      if (if_stmt.else_branch) {
        // Branch to either then or else based on condition
        builder.AddInstruction(
            lir::InstructionKind::kBranch, "",
            {condition_value, lir::Value::MakeLiteralString(then_label),
             lir::Value::MakeLiteralString(else_label)});

        // Create then block
        builder.StartBlock(then_label);
        LowerStatement(*if_stmt.then_branch, builder);
        builder.AddInstruction(
            lir::InstructionKind::kJump, "",
            {lir::Value::MakeLiteralString(end_label)});
        builder.EndBlock();

        // Create else block
        builder.StartBlock(else_label);
        LowerStatement(*if_stmt.else_branch, builder);
        // Fall through to end block
        builder.AddInstruction(
            lir::InstructionKind::kJump, "",
            {lir::Value::MakeLiteralString(end_label)});
        builder.EndBlock();
      } else {
        // No else branch - create a simpler branch structure
        builder.AddInstruction(
            lir::InstructionKind::kBranch, "",
            {condition_value, lir::Value::MakeLiteralString(then_label),
             lir::Value::MakeLiteralString(end_label)});

        // Create then block
        builder.StartBlock(then_label);
        LowerStatement(*if_stmt.then_branch, builder);
        // Fall through to end block
        builder.AddInstruction(
            lir::InstructionKind::kJump, "",
            {lir::Value::MakeLiteralString(end_label)});
        builder.EndBlock();
      }

      // Create end block - control rejoins here
      builder.StartBlock(end_label);

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
      throw std::runtime_error(fmt::format(
          "Unsupported statement kind {} in MIR to LIR LowerStatement",
          statement.kind));
  }
}

}  // namespace lyra::lowering
