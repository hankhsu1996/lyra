#include "lyra/lowering/mir_to_lir/statement.hpp"

#include <cassert>

#include "lyra/common/value_storage.hpp"
#include "lyra/lowering/mir_to_lir/expression.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Literal = common::Literal;
using ValueStorage = common::ValueStorage;
using Operand = lir::Operand;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerStatement(const mir::Statement& statement, LirBuilder& builder)
    -> void {
  switch (statement.kind) {
    case mir::Statement::Kind::kVariableDeclaration: {
      const auto& declaration =
          mir::As<mir::VariableDeclarationStatement>(statement);

      builder.AddProcessVariable(declaration.variable);

      if (declaration.initializer) {
        auto result = LowerExpression(*declaration.initializer, builder);
        auto instruction = Instruction::StoreVariable(
            declaration.variable.symbol, result, false);
        builder.AddInstruction(std::move(instruction));
      }
      break;
    }

    case mir::Statement::Kind::kAssign: {
      const auto& assign = mir::As<mir::AssignStatement>(statement);
      const auto& target = assign.target;
      const auto& expression = assign.value;
      assert(expression);

      // Lower the expression and get its result value
      auto result_value = LowerExpression(*expression, builder);

      // Store the result to the target variable
      auto instruction =
          Instruction::StoreVariable(target, result_value, false);
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
      auto instruction = Instruction::WaitEvent(wait.triggers);
      builder.AddInstruction(std::move(instruction));

      break;
    }

    case mir::Statement::Kind::kDelay: {
      const auto& delay = mir::As<mir::DelayStatement>(statement);
      auto delay_amount = Literal::ULongInt(delay.delay_amount);
      auto delay_interned = builder.InternLiteral(delay_amount);
      auto instruction = Instruction::Delay(Operand::Literal(delay_interned));
      builder.AddInstruction(std::move(instruction));
      break;
    }

    case mir::Statement::Kind::kConditional: {
      const auto& if_statement = mir::As<mir::ConditionalStatement>(statement);
      assert(if_statement.condition);
      assert(if_statement.then_branch);

      // Generate unique labels for the various blocks
      auto then_label = builder.MakeLabel("if.then");
      auto else_label = builder.MakeLabel("if.else");
      auto end_label = builder.MakeLabel("if.end");

      // Evaluate the condition
      auto condition_value = LowerExpression(*if_statement.condition, builder);

      // Create a branch instruction
      if (if_statement.else_branch) {
        // Branch to either then or else based on condition
        auto branch =
            Instruction::Branch(condition_value, then_label, else_label);
        builder.AddInstruction(std::move(branch));

        // Create then block
        builder.StartBlock(then_label);
        LowerStatement(*if_statement.then_branch, builder);
        auto jump_to_end = Instruction::Jump(end_label);
        builder.AddInstruction(std::move(jump_to_end));
        builder.EndBlock();

        // Create else block
        builder.StartBlock(else_label);
        LowerStatement(*if_statement.else_branch, builder);
        // Fall through to end block
        auto fall_through_to_end = Instruction::Jump(end_label);
        builder.AddInstruction(std::move(fall_through_to_end));
        builder.EndBlock();
      } else {
        // No else branch - create a simpler branch structure
        auto branch =
            Instruction::Branch(condition_value, then_label, end_label);
        builder.AddInstruction(std::move(branch));

        // Create then block
        builder.StartBlock(then_label);
        LowerStatement(*if_statement.then_branch, builder);
        // Fall through to end block
        auto fall_through_to_end = Instruction::Jump(end_label);
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
      auto cond_label = builder.MakeLabel("while.cond");
      auto body_label = builder.MakeLabel("while.body");
      auto end_label = builder.MakeLabel("while.end");

      // Jump to condition block first
      auto jump_to_cond = Instruction::Jump(cond_label);
      builder.AddInstruction(std::move(jump_to_cond));

      // Create condition block
      builder.StartBlock(cond_label);
      auto condition_value =
          LowerExpression(*while_statement.condition, builder);

      // Branch to either body or end based on condition
      auto branch = Instruction::Branch(condition_value, body_label, end_label);
      builder.AddInstruction(std::move(branch));
      builder.EndBlock();

      // Create body block
      builder.StartBlock(body_label);
      LowerStatement(*while_statement.body, builder);

      // Jump back to condition after executing body
      auto jump_back_to_cond = Instruction::Jump(cond_label);
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
      auto body_label = builder.MakeLabel("do_while.body");
      auto cond_label = builder.MakeLabel("do_while.cond");
      auto end_label = builder.MakeLabel("do_while.end");

      // Jump to body block first (do-while executes body at least once)
      auto jump_to_body = Instruction::Jump(body_label);
      builder.AddInstruction(std::move(jump_to_body));

      // Create body block
      builder.StartBlock(body_label);
      LowerStatement(*do_while_statement.body, builder);

      // Jump to condition after executing body
      auto jump_to_cond = Instruction::Jump(cond_label);
      builder.AddInstruction(std::move(jump_to_cond));
      builder.EndBlock();

      // Create condition block (evaluated after body execution)
      builder.StartBlock(cond_label);
      auto condition_value =
          LowerExpression(*do_while_statement.condition, builder);

      // Branch back to body if condition is true, otherwise go to end
      auto branch = Instruction::Branch(condition_value, body_label, end_label);
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

}  // namespace lyra::lowering::mir_to_lir
