#include "lyra/lowering/mir_to_lir/statement.hpp"

#include <cassert>
#include <utility>

#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/expression.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Literal = common::Literal;
using Operand = lir::Operand;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerStatement(
    const mir::Statement& statement, LirBuilder& builder,
    LoweringContext& lowering_context) -> void {
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
        LowerStatement(*if_statement.then_branch, builder, lowering_context);
        auto jump_to_end = Instruction::Jump(end_label);
        builder.AddInstruction(std::move(jump_to_end));
        builder.EndBlock();

        // Create else block
        builder.StartBlock(else_label);
        LowerStatement(*if_statement.else_branch, builder, lowering_context);
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
        LowerStatement(*if_statement.then_branch, builder, lowering_context);
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
      assert(while_statement.condition);
      assert(while_statement.body);

      auto cond_label = builder.MakeLabel("while.cond");
      auto body_label = builder.MakeLabel("while.body");
      auto end_label = builder.MakeLabel("while.end");

      builder.AddInstruction(Instruction::Jump(cond_label));

      lowering_context.PushLoop({
          .continue_label = cond_label,
          .break_label = end_label,
      });

      builder.StartBlock(cond_label);
      auto condition_value =
          LowerExpression(*while_statement.condition, builder);
      builder.AddInstruction(
          Instruction::Branch(condition_value, body_label, end_label));
      builder.EndBlock();

      builder.StartBlock(body_label);
      LowerStatement(*while_statement.body, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(cond_label));
      builder.EndBlock();

      lowering_context.PopLoop();

      builder.StartBlock(end_label);
      break;
    }

    case mir::Statement::Kind::kDoWhile: {
      const auto& do_while_statement =
          mir::As<mir::DoWhileStatement>(statement);
      assert(do_while_statement.condition);
      assert(do_while_statement.body);

      auto body_label = builder.MakeLabel("do_while.body");
      auto cond_label = builder.MakeLabel("do_while.cond");
      auto end_label = builder.MakeLabel("do_while.end");

      builder.AddInstruction(Instruction::Jump(body_label));

      lowering_context.PushLoop({
          .continue_label = cond_label,
          .break_label = end_label,
      });

      builder.StartBlock(body_label);
      LowerStatement(*do_while_statement.body, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(cond_label));
      builder.EndBlock();

      lowering_context.PopLoop();

      builder.StartBlock(cond_label);
      auto condition_value =
          LowerExpression(*do_while_statement.condition, builder);
      builder.AddInstruction(
          Instruction::Branch(condition_value, body_label, end_label));
      builder.EndBlock();

      builder.StartBlock(end_label);
      break;
    }

    case mir::Statement::Kind::kFor: {
      const auto& for_statement = mir::As<mir::ForStatement>(statement);
      assert(for_statement.body);

      // Lower initializers first (variable declarations and init expressions)
      for (const auto& init : for_statement.initializers) {
        LowerStatement(*init, builder, lowering_context);
      }

      auto cond_label = builder.MakeLabel("for.cond");
      auto body_label = builder.MakeLabel("for.body");
      auto step_label = builder.MakeLabel("for.step");
      auto end_label = builder.MakeLabel("for.end");

      builder.AddInstruction(Instruction::Jump(cond_label));

      // continue jumps to step (to run step expressions before condition)
      lowering_context.PushLoop({
          .continue_label = step_label,
          .break_label = end_label,
      });

      // Condition block
      builder.StartBlock(cond_label);
      if (for_statement.condition) {
        auto condition_value =
            LowerExpression(*for_statement.condition, builder);
        builder.AddInstruction(
            Instruction::Branch(condition_value, body_label, end_label));
      } else {
        // No condition = infinite loop
        builder.AddInstruction(Instruction::Jump(body_label));
      }
      builder.EndBlock();

      // Body block
      builder.StartBlock(body_label);
      LowerStatement(*for_statement.body, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(step_label));
      builder.EndBlock();

      // Step block
      builder.StartBlock(step_label);
      for (const auto& step : for_statement.steps) {
        LowerExpression(*step, builder);
      }
      builder.AddInstruction(Instruction::Jump(cond_label));
      builder.EndBlock();

      lowering_context.PopLoop();

      builder.StartBlock(end_label);
      break;
    }

    case mir::Statement::Kind::kBreak: {
      assert(lowering_context.HasLoop());
      auto target = lowering_context.CurrentLoop().break_label;
      builder.AddInstruction(Instruction::Jump(target));
      break;
    }

    case mir::Statement::Kind::kContinue: {
      assert(lowering_context.HasLoop());
      auto target = lowering_context.CurrentLoop().continue_label;
      builder.AddInstruction(Instruction::Jump(target));
      break;
    }

    case mir::Statement::Kind::kBlock: {
      const auto& block = mir::As<mir::BlockStatement>(statement);
      for (const auto& statement : block.statements) {
        if (statement) {
          LowerStatement(*statement, builder, lowering_context);
        }
      }
      break;
    }
  }
}

}  // namespace lyra::lowering::mir_to_lir
