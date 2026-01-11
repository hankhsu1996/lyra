#include <cstdint>

#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lowering/mir_to_lir/expression_internal.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::mir_to_lir {

using Literal = common::Literal;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerLiteralExpression(
    const mir::LiteralExpression& literal_expression, LirBuilder& builder)
    -> lir::TempRef {
  auto result = builder.AllocateTemp("lit", literal_expression.literal.type);
  auto literal = builder.InternLiteral(literal_expression.literal);
  auto instruction = Instruction::Basic(IK::kLiteral, result, literal);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerIdentifierExpression(
    const mir::IdentifierExpression& identifier, LirBuilder& builder)
    -> lir::TempRef {
  auto result = builder.AllocateTemp("load", identifier.type);
  auto instruction =
      Instruction::Basic(IK::kLoadVariable, result, identifier.symbol);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerEnumValueExpression(
    const mir::EnumValueExpression& enum_val, LirBuilder& builder)
    -> lir::TempRef {
  auto result = builder.AllocateTemp("enum", enum_val.type);

  // Respect the enum's base type signedness
  const auto& integral_data =
      std::get<common::IntegralData>(enum_val.type.data);
  auto width = enum_val.type.GetBitWidth();
  auto literal = integral_data.is_signed
                     ? builder.InternLiteral(
                           Literal::IntegralSigned(enum_val.value, width))
                     : builder.InternLiteral(
                           Literal::IntegralUnsigned(
                               static_cast<uint64_t>(enum_val.value), width));

  auto instruction = Instruction::Basic(IK::kLiteral, result, literal);
  builder.AddInstruction(std::move(instruction));
  return result;
}

}  // namespace lyra::lowering::mir_to_lir
