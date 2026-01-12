#include "lyra/lowering/mir_to_lir/statement/statement.hpp"

#include <cassert>
#include <cstddef>

#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/helpers.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/lowering/mir_to_lir/statement/internal.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_lir {

using Instruction = lir::Instruction;

auto LowerStatement(
    const mir::Statement& statement, LirBuilder& builder,
    LoweringContext& lowering_context) -> void {
  switch (statement.kind) {
    case mir::Statement::Kind::kVariableDeclaration: {
      const auto& declaration =
          mir::As<mir::VariableDeclarationStatement>(statement);

      // Register variable in the current context (process or function).
      // The builder handles context-specific behavior internally.
      builder.RegisterLocalVariable(declaration.variable);

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
        // Hierarchical assignment uses target_symbol directly (flat storage
        // model)
        auto instruction = Instruction::StoreVariable(
            target.target_symbol, result_value, false);
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

          // Compute bit_offset = adjusted_index * element_width
          auto bit_offset =
              builder.AllocateTemp("bit_offset", common::Type::Int());
          auto width_literal = builder.InternLiteral(
              common::Literal::Int(static_cast<int32_t>(element_width)));
          auto width_temp = builder.AllocateTemp("width", common::Type::Int());
          builder.AddInstruction(
              Instruction::Basic(
                  lir::InstructionKind::kLiteral, width_temp, width_literal));
          builder.AddInstruction(
              Instruction::Basic(
                  lir::InstructionKind::kBinaryMultiply, bit_offset,
                  {lir::Operand::Temp(adjusted_index),
                   lir::Operand::Temp(width_temp)}));

          auto slice_type = common::Type::IntegralUnsigned(
              static_cast<uint32_t>(element_width));
          auto instruction = Instruction::StorePackedBits(
              target.symbol, bit_offset, result_value, slice_type);
          builder.AddInstruction(std::move(instruction));
        } else {
          // Unpacked array element assignment: array[index] = value
          assert(target.indices.size() == 1);
          auto index = LowerExpression(*target.indices[0], builder);
          auto instruction = Instruction::StoreElement(
              lir::Operand::Variable(target.symbol), index, result_value);
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
      auto delay_amount = common::Literal::ULongInt(scaled_delay);
      auto delay_interned = builder.InternLiteral(delay_amount);
      auto instruction =
          Instruction::Delay(lir::Operand::Literal(delay_interned));
      builder.AddInstruction(std::move(instruction));
      break;
    }

    case mir::Statement::Kind::kConditional: {
      const auto& if_stmt = mir::As<mir::ConditionalStatement>(statement);
      LowerConditionalStatement(if_stmt, builder, lowering_context);
      break;
    }

    case mir::Statement::Kind::kWhile: {
      const auto& while_stmt = mir::As<mir::WhileStatement>(statement);
      LowerWhileLoop(while_stmt, builder, lowering_context);
      break;
    }

    case mir::Statement::Kind::kDoWhile: {
      const auto& do_while_stmt = mir::As<mir::DoWhileStatement>(statement);
      LowerDoWhileLoop(do_while_stmt, builder, lowering_context);
      break;
    }

    case mir::Statement::Kind::kFor: {
      const auto& for_stmt = mir::As<mir::ForStatement>(statement);
      LowerForLoop(for_stmt, builder, lowering_context);
      break;
    }

    case mir::Statement::Kind::kRepeat: {
      const auto& repeat_stmt = mir::As<mir::RepeatStatement>(statement);
      LowerRepeatLoop(repeat_stmt, builder, lowering_context);
      break;
    }

    case mir::Statement::Kind::kCase: {
      const auto& case_stmt = mir::As<mir::CaseStatement>(statement);
      LowerCaseStatement(case_stmt, builder, lowering_context);
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
      for (const auto& stmt : block.statements) {
        if (stmt) {
          LowerStatement(*stmt, builder, lowering_context);
        }
      }
      break;
    }

    case mir::Statement::Kind::kReturn: {
      const auto& ret = mir::As<mir::ReturnStatement>(statement);

      if (ret.value) {
        auto value_temp = LowerExpression(*ret.value, builder);
        builder.AddInstruction(Instruction::Return(value_temp));
      } else {
        builder.AddInstruction(Instruction::Return());
      }
      break;
    }
  }
}

}  // namespace lyra::lowering::mir_to_lir
