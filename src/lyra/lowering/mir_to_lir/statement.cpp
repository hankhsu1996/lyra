#include "lyra/lowering/mir_to_lir/statement.hpp"

#include <cassert>

#include "lyra/common/value_storage.hpp"
#include "lyra/lowering/mir_to_lir/expression.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering {

using Type = common::Type;
using Literal = common::Literal;
using ValueStorage = common::ValueStorage;

auto LowerStatement(const mir::Statement& statement, LirBuilder& builder)
    -> void {
  switch (statement.kind) {
    case mir::Statement::Kind::kAssign: {
      const auto& assign = mir::As<mir::AssignStatement>(statement);

      const auto& target = assign.target;

      const auto& expression = assign.value;
      assert(expression);

      // Lower the expression and get its result value
      auto result_value = LowerExpression(*expression, builder);

      // Store the result to the target variable
      auto instruction = lir::Instruction::Basic(
          lir::InstructionKind::kStoreVariable, "",
          {lir::Operand::Variable(target), result_value});
      builder.AddInstruction(std::move(instruction));
      break;
    }

    case mir::Statement::Kind::kExpression: {
      const auto& expression_statement =
          mir::As<mir::ExpressionStatement>(statement);

      // Use assertion for internal consistency check
      assert(expression_statement.expression);

      // Lower the expression, which may produce instructions
      LowerExpression(*expression_statement.expression, builder);
      break;
    }

    case mir::Statement::Kind::kWaitEvent: {
      const auto& wait = mir::As<mir::WaitEventStatement>(statement);

      // Lower to a LIR wait-event instruction using the trigger list
      auto instruction = lir::Instruction::WaitEvent(wait.triggers);
      builder.AddInstruction(std::move(instruction));

      break;
    }

    case mir::Statement::Kind::kDelay: {
      const auto& delay = mir::As<mir::DelayStatement>(statement);
      auto delay_amount = Literal::ULongInt(delay.delay_amount);
      auto instruction = lir::Instruction::Basic(
          lir::InstructionKind::kDelay, "",
          {lir::Operand::Literal(delay_amount)});
      builder.AddInstruction(std::move(instruction));
      break;
    }

    case mir::Statement::Kind::kConditional: {
      const auto& if_statement = mir::As<mir::ConditionalStatement>(statement);
      assert(if_statement.condition);
      assert(if_statement.then_branch);

      // Generate unique labels for the various blocks
      static int if_counter = 0;
      std::string if_id = std::to_string(if_counter++);

      std::string then_label = "if." + if_id + ".then";
      std::string else_label = "if." + if_id + ".else";
      std::string end_label = "if." + if_id + ".end";

      // Evaluate the condition
      auto condition_value = LowerExpression(*if_statement.condition, builder);

      // Create a branch instruction
      if (if_statement.else_branch) {
        // Branch to either then or else based on condition
        auto branch = lir::Instruction::Basic(
            lir::InstructionKind::kBranch, "",
            {condition_value, lir::Operand::Label(then_label),
             lir::Operand::Label(else_label)});
        builder.AddInstruction(std::move(branch));

        // Create then block
        builder.StartBlock(then_label);
        LowerStatement(*if_statement.then_branch, builder);
        auto jump_to_end = lir::Instruction::Basic(
            lir::InstructionKind::kJump, "", {lir::Operand::Label(end_label)});
        builder.AddInstruction(std::move(jump_to_end));
        builder.EndBlock();

        // Create else block
        builder.StartBlock(else_label);
        LowerStatement(*if_statement.else_branch, builder);
        // Fall through to end block
        auto fall_through_to_end = lir::Instruction::Basic(
            lir::InstructionKind::kJump, "", {lir::Operand::Label(end_label)});
        builder.AddInstruction(std::move(fall_through_to_end));
        builder.EndBlock();
      } else {
        // No else branch - create a simpler branch structure
        auto branch = lir::Instruction::Basic(
            lir::InstructionKind::kBranch, "",
            {condition_value, lir::Operand::Label(then_label),
             lir::Operand::Label(end_label)});
        builder.AddInstruction(std::move(branch));

        // Create then block
        builder.StartBlock(then_label);
        LowerStatement(*if_statement.then_branch, builder);
        // Fall through to end block
        auto fall_through_to_end = lir::Instruction::Basic(
            lir::InstructionKind::kJump, "", {lir::Operand::Label(end_label)});
        builder.AddInstruction(std::move(fall_through_to_end));
        builder.EndBlock();
      }

      // Create end block - control rejoins here
      builder.StartBlock(end_label);

      break;
    }

    case mir::Statement::Kind::kWhile: {
      const auto& while_statement = mir::As<mir::WhileStatement>(statement);

      // Use assertions for internal consistency checks
      assert(while_statement.condition);
      assert(while_statement.body);

      // Generate unique labels for the various blocks
      static int while_counter = 0;
      std::string while_id = std::to_string(while_counter++);

      std::string cond_label = "while." + while_id + ".cond";
      std::string body_label = "while." + while_id + ".body";
      std::string end_label = "while." + while_id + ".end";

      // Jump to condition block first
      auto jump_to_cond = lir::Instruction::Basic(
          lir::InstructionKind::kJump, "", {lir::Operand::Label(cond_label)});
      builder.AddInstruction(std::move(jump_to_cond));

      // Create condition block
      builder.StartBlock(cond_label);
      auto condition_value =
          LowerExpression(*while_statement.condition, builder);

      // Branch to either body or end based on condition
      auto branch = lir::Instruction::Basic(
          lir::InstructionKind::kBranch, "",
          {condition_value, lir::Operand::Label(body_label),
           lir::Operand::Label(end_label)});
      builder.AddInstruction(std::move(branch));
      builder.EndBlock();

      // Create body block
      builder.StartBlock(body_label);
      LowerStatement(*while_statement.body, builder);

      // Jump back to condition after executing body
      auto jump_back_to_cond = lir::Instruction::Basic(
          lir::InstructionKind::kJump, "", {lir::Operand::Label(cond_label)});
      builder.AddInstruction(std::move(jump_back_to_cond));
      builder.EndBlock();

      // Create end block - control continues here when loop is done
      builder.StartBlock(end_label);

      break;
    }

    case mir::Statement::Kind::kDoWhile: {
      const auto& do_while_statement =
          mir::As<mir::DoWhileStatement>(statement);

      // Use assertions for internal consistency checks
      assert(do_while_statement.condition);
      assert(do_while_statement.body);

      // Generate unique labels for the various blocks
      static int do_while_counter = 0;
      std::string do_while_id = std::to_string(do_while_counter++);

      std::string body_label = "do_while." + do_while_id + ".body";
      std::string cond_label = "do_while." + do_while_id + ".cond";
      std::string end_label = "do_while." + do_while_id + ".end";

      // Jump to body block first (do-while executes body at least once)
      auto jump_to_body = lir::Instruction::Basic(
          lir::InstructionKind::kJump, "", {lir::Operand::Label(body_label)});
      builder.AddInstruction(std::move(jump_to_body));

      // Create body block
      builder.StartBlock(body_label);
      LowerStatement(*do_while_statement.body, builder);

      // Jump to condition after executing body
      auto jump_to_cond = lir::Instruction::Basic(
          lir::InstructionKind::kJump, "", {lir::Operand::Label(cond_label)});
      builder.AddInstruction(std::move(jump_to_cond));
      builder.EndBlock();

      // Create condition block (evaluated after body execution)
      builder.StartBlock(cond_label);
      auto condition_value =
          LowerExpression(*do_while_statement.condition, builder);

      // Branch back to body if condition is true, otherwise go to end
      auto branch = lir::Instruction::Basic(
          lir::InstructionKind::kBranch, "",
          {condition_value, lir::Operand::Label(body_label),
           lir::Operand::Label(end_label)});
      builder.AddInstruction(std::move(branch));
      builder.EndBlock();

      // Create end block - control continues here when loop is done
      builder.StartBlock(end_label);

      break;
    }

    case mir::Statement::Kind::kBlock: {
      const auto& block = mir::As<mir::BlockStatement>(statement);
      for (const auto& statement : block.statements) {
        if (statement) {
          LowerStatement(*statement, builder);
        }
      }
      break;
    }
  }
}

}  // namespace lyra::lowering
