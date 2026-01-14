#include "lyra/lowering/mir_to_lir/statement/statement.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/helpers.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/lowering/mir_to_lir/statement/internal.hpp"
#include "lyra/mir/expression.hpp"
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
        // Store through pointer
        const auto* pointee =
            builder.GetContext()->InternType(declaration.variable.type);
        auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
        builder.AddInstruction(
            Instruction::ResolveVar(ptr, declaration.variable.symbol));
        builder.AddInstruction(Instruction::Store(ptr, result));
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
        // model). Use the result value's type for the pointer (types should
        // match after implicit conversion).
        const auto& value_type = expression->type;
        const auto* pointee = builder.GetContext()->InternType(value_type);
        auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
        builder.AddInstruction(
            Instruction::ResolveVar(ptr, target.target_symbol));
        builder.AddInstruction(Instruction::Store(ptr, result_value));
        break;
      }

      if (target.IsElementSelect()) {
        if (target.IsPacked()) {
          // Packed element assignment (possibly multi-dimensional)
          // ResolveVar -> compute bit offset -> StoreSlice directly
          const auto& base_type = *target.base_type;
          size_t element_width =
              GetElementWidthAfterIndices(base_type, target.indices.size());
          auto composite_index =
              ComputeCompositeIndex(target.indices, base_type, builder);
          auto adjusted_index = AdjustForNonZeroLower(
              composite_index, base_type.GetElementLower(), builder);

          // Get pointer to base variable
          const auto* base_pointee =
              builder.GetContext()->InternType(base_type);
          auto base_ptr =
              builder.AllocateTemp("ptr", common::Type::Pointer(base_pointee));
          builder.AddInstruction(
              Instruction::ResolveVar(base_ptr, target.symbol));

          // Compute bit_offset = adjusted_index * element_width
          auto bit_offset =
              builder.AllocateTemp("bit_offset", common::Type::Int());
          auto width_constant = builder.InternConstant(
              common::Constant::Int(static_cast<int32_t>(element_width)));
          auto width_temp = builder.AllocateTemp("width", common::Type::Int());
          builder.AddInstruction(
              Instruction::Constant(width_temp, width_constant));
          builder.AddInstruction(
              Instruction::Basic(
                  lir::InstructionKind::kBinaryMultiply, bit_offset,
                  {lir::Operand::Temp(adjusted_index),
                   lir::Operand::Temp(width_temp)}));

          // Direct StoreSlice: base_ptr[bit_offset +: width] = result_value
          builder.AddInstruction(
              Instruction::StoreSlice(
                  base_ptr, result_value, bit_offset, width_temp));
        } else {
          // Unpacked array element assignment: array[index] = value
          // Use pointer chain: ResolveVar -> ResolveIndex -> Store
          const auto& base_type = *target.base_type;

          // Build pointer chain
          const auto* arr_pointee = builder.GetContext()->InternType(base_type);
          auto ptr =
              builder.AllocateTemp("ptr", common::Type::Pointer(arr_pointee));
          builder.AddInstruction(Instruction::ResolveVar(ptr, target.symbol));

          const common::Type* current_type = &base_type;
          for (const auto& idx_expr : target.indices) {
            auto index = LowerExpression(*idx_expr, builder);
            const common::Type& elem_type = current_type->GetElementType();
            const auto* elem_pointee =
                builder.GetContext()->InternType(elem_type);
            auto elem_ptr = builder.AllocateTemp(
                "ptr_elem", common::Type::Pointer(elem_pointee));
            builder.AddInstruction(
                Instruction::ResolveIndex(elem_ptr, ptr, index));
            ptr = elem_ptr;
            current_type = &elem_type;
          }

          builder.AddInstruction(Instruction::Store(ptr, result_value));
        }
        break;
      }

      // Store the result to the target variable through pointer.
      // Use base_type if available, otherwise use the result value's type.
      const auto& var_type =
          target.base_type.has_value() ? *target.base_type : expression->type;
      const auto* pointee = builder.GetContext()->InternType(var_type);
      auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
      builder.AddInstruction(Instruction::ResolveVar(ptr, target.symbol));
      builder.AddInstruction(Instruction::Store(ptr, result_value));
      break;
    }

    case mir::Statement::Kind::kExpression: {
      const auto& expression_statement =
          mir::As<mir::ExpressionStatement>(statement);

      // Use assertion for internal consistency check
      assert(expression_statement.expression);

      const auto& expr = *expression_statement.expression;

      // Handle mutating method calls on variables (SSA style):
      // q.push_back(x) should store the result back to q
      if (expr.kind == mir::Expression::Kind::kMethodCall) {
        const auto& method_call = mir::As<mir::MethodCallExpression>(expr);
        const auto& receiver = *method_call.receiver;

        // Check if this is a mutating method that returns the modified
        // receiver. Note: pop_back/pop_front return the element, not the
        // receiver, so they are handled separately in assignment lowering.
        auto m = method_call.method;
        bool is_mutating =
            (m == mir::BuiltinMethod::kPushBack ||
             m == mir::BuiltinMethod::kPushFront ||
             m == mir::BuiltinMethod::kInsert ||
             m == mir::BuiltinMethod::kDelete);

        if (is_mutating &&
            receiver.kind == mir::Expression::Kind::kIdentifier) {
          const auto& ident = mir::As<mir::IdentifierExpression>(receiver);
          // Lower the method call
          auto result = LowerExpression(expr, builder);
          // Store result back to the receiver variable (SSA writeback)
          const auto* pointee = builder.GetContext()->InternType(ident.type);
          auto ptr =
              builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
          builder.AddInstruction(Instruction::ResolveVar(ptr, ident.symbol));
          builder.AddInstruction(Instruction::Store(ptr, result));
          break;
        }
      }

      // Lower the expression, which may produce instructions
      LowerExpression(expr, builder);
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
      auto delay_amount = common::Constant::ULongInt(scaled_delay);
      auto delay_interned = builder.InternConstant(delay_amount);
      auto instruction = Instruction::Delay(delay_interned);
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
