#include "lyra/lowering/mir_to_lir/statement.hpp"

#include <cassert>
#include <utility>

#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/expression.hpp"
#include "lyra/lowering/mir_to_lir/helpers.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
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

      if (target.IsHierarchical()) {
        // Hierarchical assignment: child.signal = value
        auto instruction = Instruction::StoreHierarchical(
            target.instance_path, target.target_symbol, result_value, false);
        builder.AddInstruction(std::move(instruction));
        break;
      }

      if (target.IsElementSelect()) {
        if (target.IsPacked()) {
          // Packed element assignment (possibly multi-dimensional)
          const auto& base_type = *target.base_type;
          size_t element_width =
              GetElementWidthAfterIndices(base_type, target.indices.size());
          auto composite_index =
              ComputeCompositeIndex(target.indices, base_type, builder);
          auto adjusted_index = AdjustForNonZeroLower(
              composite_index, base_type.GetElementLower(), builder);

          auto instruction = Instruction::StorePackedElement(
              target.symbol, adjusted_index, result_value, element_width);
          builder.AddInstruction(std::move(instruction));
        } else {
          // Unpacked array element assignment: array[index] = value
          assert(target.indices.size() == 1);
          auto index = LowerExpression(*target.indices[0], builder);
          auto instruction = Instruction::StoreUnpackedElement(
              target.symbol, index, result_value);
          builder.AddInstruction(std::move(instruction));
        }
        break;
      }

      // Store the result to the target variable
      auto instruction =
          Instruction::StoreVariable(target.symbol, result_value, false);
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
      // Scale delay based on module's timescale and global precision
      uint64_t scaled_delay =
          delay.delay_amount * lowering_context.DelayMultiplier();
      auto delay_amount = Literal::ULongInt(scaled_delay);
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

    case mir::Statement::Kind::kRepeat: {
      const auto& repeat_statement = mir::As<mir::RepeatStatement>(statement);
      assert(repeat_statement.count);
      assert(repeat_statement.body);

      // Evaluate count expression and store in a counter temp
      auto count_value = LowerExpression(*repeat_statement.count, builder);
      auto counter_temp = builder.AllocateTemp("repeat.counter", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(IK::kMove, counter_temp, count_value));

      // Create literals for comparison and decrement
      auto zero_literal = builder.InternLiteral(Literal::Int(0));
      auto one_literal = builder.InternLiteral(Literal::Int(1));

      auto cond_label = builder.MakeLabel("repeat.cond");
      auto body_label = builder.MakeLabel("repeat.body");
      auto step_label = builder.MakeLabel("repeat.step");
      auto end_label = builder.MakeLabel("repeat.end");

      builder.AddInstruction(Instruction::Jump(cond_label));

      // continue jumps to step (to decrement before checking condition)
      lowering_context.PushLoop({
          .continue_label = step_label,
          .break_label = end_label,
      });

      // Condition block: counter > 0
      builder.StartBlock(cond_label);
      auto zero_temp = builder.AllocateTemp("zero", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kLiteral, zero_temp, Operand::Literal(zero_literal)));
      auto cond_temp = builder.AllocateTemp("repeat.cond", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kBinaryGreaterThan, cond_temp,
              {Operand::Temp(counter_temp), Operand::Temp(zero_temp)}));
      builder.AddInstruction(
          Instruction::Branch(cond_temp, body_label, end_label));
      builder.EndBlock();

      // Body block
      builder.StartBlock(body_label);
      LowerStatement(*repeat_statement.body, builder, lowering_context);
      builder.AddInstruction(Instruction::Jump(step_label));
      builder.EndBlock();

      // Step block: counter = counter - 1
      builder.StartBlock(step_label);
      auto one_temp = builder.AllocateTemp("one", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kLiteral, one_temp, Operand::Literal(one_literal)));
      auto new_counter =
          builder.AllocateTemp("repeat.new_counter", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(
              IK::kBinarySubtract, new_counter,
              {Operand::Temp(counter_temp), Operand::Temp(one_temp)}));
      builder.AddInstruction(
          Instruction::Basic(IK::kMove, counter_temp, new_counter));
      builder.AddInstruction(Instruction::Jump(cond_label));
      builder.EndBlock();

      lowering_context.PopLoop();

      builder.StartBlock(end_label);
      break;
    }

    case mir::Statement::Kind::kCase: {
      const auto& case_stmt = mir::As<mir::CaseStatement>(statement);
      assert(case_stmt.condition);

      // Evaluate case condition once and store in a temp
      auto cond_value = LowerExpression(*case_stmt.condition, builder);
      auto cond_temp = builder.AllocateTemp("case.cond", Type::Int());
      builder.AddInstruction(
          Instruction::Basic(IK::kMove, cond_temp, cond_value));

      auto end_label = builder.MakeLabel("case.end");

      // Generate labels for each item body and next check
      std::vector<lir::LabelRef> item_labels;
      std::vector<lir::LabelRef> check_labels;
      for (size_t i = 0; i < case_stmt.items.size(); ++i) {
        item_labels.push_back(builder.MakeLabel("case.item"));
        check_labels.push_back(builder.MakeLabel("case.check"));
      }

      // Default or end label for when no item matches
      auto default_label = case_stmt.default_case
                               ? builder.MakeLabel("case.default")
                               : end_label;

      // Jump to first check
      if (!case_stmt.items.empty()) {
        builder.AddInstruction(Instruction::Jump(check_labels[0]));
      } else if (case_stmt.default_case) {
        builder.AddInstruction(Instruction::Jump(default_label));
      } else {
        builder.AddInstruction(Instruction::Jump(end_label));
      }

      // Generate check and body blocks for each item
      for (size_t i = 0; i < case_stmt.items.size(); ++i) {
        const auto& item = case_stmt.items[i];
        auto next_label = (i + 1 < case_stmt.items.size()) ? check_labels[i + 1]
                                                           : default_label;

        // Check block: compare condition against each expression
        builder.StartBlock(check_labels[i]);

        // Evaluate all expressions and OR together
        // For normal case: match = (cond == expr0) || (cond == expr1) || ...
        // For casez/casex: match = ((cond & mask0) == expr0) || ...
        lir::TempRef match_result{};
        for (size_t j = 0; j < item.expressions.size(); ++j) {
          auto expr_value = LowerExpression(*item.expressions[j], builder);
          int64_t mask = item.masks[j];

          lir::TempRef cmp_lhs{};
          if (mask == -1) {
            // Normal case - compare directly
            cmp_lhs = cond_temp;
          } else {
            // Wildcard case - apply mask to condition
            // Use unsigned literal to match condition type (slang converts to
            // unsigned)
            auto mask_literal = builder.InternLiteral(
                Literal::UInt(static_cast<uint32_t>(mask)));
            auto mask_temp = builder.AllocateTemp("case.mask", Type::UInt());
            builder.AddInstruction(
                Instruction::Basic(
                    IK::kLiteral, mask_temp, Operand::Literal(mask_literal)));
            auto masked_cond =
                builder.AllocateTemp("case.masked_cond", Type::UInt());
            builder.AddInstruction(
                Instruction::Basic(
                    IK::kBinaryBitwiseAnd, masked_cond,
                    std::vector<Operand>{
                        Operand::Temp(cond_temp), Operand::Temp(mask_temp)}));
            cmp_lhs = masked_cond;
          }

          auto cmp_temp = builder.AllocateTemp("case.cmp", Type::Int());
          builder.AddInstruction(
              Instruction::Basic(
                  IK::kBinaryEqual, cmp_temp,
                  std::vector<Operand>{
                      Operand::Temp(cmp_lhs), Operand::Temp(expr_value)}));

          if (j == 0) {
            match_result = cmp_temp;
          } else {
            auto or_temp = builder.AllocateTemp("case.or", Type::Int());
            builder.AddInstruction(
                Instruction::Basic(
                    IK::kBinaryLogicalOr, or_temp,
                    std::vector<Operand>{
                        Operand::Temp(match_result), Operand::Temp(cmp_temp)}));
            match_result = or_temp;
          }
        }

        builder.AddInstruction(
            Instruction::Branch(match_result, item_labels[i], next_label));
        builder.EndBlock();

        // Body block
        builder.StartBlock(item_labels[i]);
        if (item.statement) {
          LowerStatement(*item.statement, builder, lowering_context);
        }
        builder.AddInstruction(Instruction::Jump(end_label));
        builder.EndBlock();
      }

      // Default block (if present)
      if (case_stmt.default_case) {
        builder.StartBlock(default_label);
        LowerStatement(*case_stmt.default_case, builder, lowering_context);
        builder.AddInstruction(Instruction::Jump(end_label));
        builder.EndBlock();
      }

      // End block
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
