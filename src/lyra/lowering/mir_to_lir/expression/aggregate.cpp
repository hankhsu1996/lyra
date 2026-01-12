#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/expression/internal.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::mir_to_lir {

using Literal = common::Literal;
using Operand = lir::Operand;
using TempRef = lir::TempRef;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerConcatenationExpression(
    const mir::ConcatenationExpression& concat, LirBuilder& builder)
    -> lir::TempRef {
  std::vector<TempRef> operand_temps;
  operand_temps.reserve(concat.operands.size());
  for (const auto& operand : concat.operands) {
    operand_temps.push_back(LowerExpression(*operand, builder));
  }
  auto result = builder.AllocateTemp("cat", concat.type);
  auto instruction =
      Instruction::Concatenation(result, std::move(operand_temps), concat.type);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerReplicationExpression(
    const mir::ReplicationExpression& rep, LirBuilder& builder)
    -> lir::TempRef {
  // Lower operand once (ensures single evaluation per LRM)
  auto operand_temp = LowerExpression(*rep.operand, builder);

  // Create vector with operand repeated count times
  std::vector<TempRef> operand_temps(rep.count, operand_temp);

  // Emit as concatenation instruction
  auto result = builder.AllocateTemp("rep", rep.type);
  auto instruction =
      Instruction::Concatenation(result, std::move(operand_temps), rep.type);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerNewArrayExpression(
    const mir::NewArrayExpression& new_arr, LirBuilder& builder)
    -> lir::TempRef {
  auto size = LowerExpression(*new_arr.size_expr, builder);
  auto result = builder.AllocateTemp("new_array", new_arr.type);

  if (new_arr.init_expr) {
    auto init = LowerExpression(*new_arr.init_expr, builder);
    builder.AddInstruction(
        Instruction::NewDynamicArray(result, size, new_arr.type, init));
  } else {
    builder.AddInstruction(
        Instruction::NewDynamicArray(result, size, new_arr.type));
  }
  return result;
}

auto LowerUnpackedStructLiteralExpression(
    const mir::UnpackedStructLiteralExpression& lit, LirBuilder& builder)
    -> lir::TempRef {
  // Create struct literal by storing field values into a new struct
  auto result = builder.AllocateTemp("struct_lit", lit.type);
  builder.AddInstruction(Instruction::CreateAggregate(result, lit.type));

  for (size_t i = 0; i < lit.field_values.size(); ++i) {
    auto field_value = LowerExpression(*lit.field_values[i], builder);

    // Emit literal for field index
    auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
    auto index_literal =
        builder.InternLiteral(Literal::Int(static_cast<int32_t>(i)));
    builder.AddInstruction(
        Instruction::Basic(IK::kLiteral, index_temp, index_literal));

    // Use unified StoreElement with temp operand
    builder.AddInstruction(
        Instruction::StoreElement(
            Operand::Temp(result), index_temp, field_value));
  }
  return result;
}

}  // namespace lyra::lowering::mir_to_lir
