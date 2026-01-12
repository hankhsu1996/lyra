#include <cassert>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/lowering/mir_to_lir/statement/internal.hpp"
#include "lyra/lowering/mir_to_lir/statement/statement.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Constant = common::Constant;
using Operand = lir::Operand;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

void LowerWhileLoop(
    const mir::WhileStatement& while_statement, LirBuilder& builder,
    LoweringContext& lowering_context) {
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
  auto condition_value = LowerExpression(*while_statement.condition, builder);
  builder.AddInstruction(
      Instruction::Branch(condition_value, body_label, end_label));
  builder.EndBlock();

  builder.StartBlock(body_label);
  LowerStatement(*while_statement.body, builder, lowering_context);
  builder.AddInstruction(Instruction::Jump(cond_label));
  builder.EndBlock();

  lowering_context.PopLoop();

  builder.StartBlock(end_label);
}

void LowerDoWhileLoop(
    const mir::DoWhileStatement& do_while_statement, LirBuilder& builder,
    LoweringContext& lowering_context) {
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
}

void LowerForLoop(
    const mir::ForStatement& for_statement, LirBuilder& builder,
    LoweringContext& lowering_context) {
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
    auto condition_value = LowerExpression(*for_statement.condition, builder);
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
}

void LowerRepeatLoop(
    const mir::RepeatStatement& repeat_statement, LirBuilder& builder,
    LoweringContext& lowering_context) {
  assert(repeat_statement.count);
  assert(repeat_statement.body);

  // Evaluate count expression and store in a counter temp
  auto count_value = LowerExpression(*repeat_statement.count, builder);
  auto counter_temp = builder.AllocateTemp("repeat.counter", Type::Int());
  builder.AddInstruction(
      Instruction::Basic(IK::kMove, counter_temp, count_value));

  // Create constants for comparison and decrement
  auto zero_constant = builder.InternConstant(Constant::Int(0));
  auto one_constant = builder.InternConstant(Constant::Int(1));

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
          IK::kConstant, zero_temp, Operand::Constant(zero_constant)));
  auto cond_temp = builder.AllocateTemp("repeat.cond", Type::Int());
  builder.AddInstruction(
      Instruction::Basic(
          IK::kBinaryGreaterThan, cond_temp,
          {Operand::Temp(counter_temp), Operand::Temp(zero_temp)}));
  builder.AddInstruction(Instruction::Branch(cond_temp, body_label, end_label));
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
          IK::kConstant, one_temp, Operand::Constant(one_constant)));
  auto new_counter = builder.AllocateTemp("repeat.new_counter", Type::Int());
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
}

}  // namespace lyra::lowering::mir_to_lir
