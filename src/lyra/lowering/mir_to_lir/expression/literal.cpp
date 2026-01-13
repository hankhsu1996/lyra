#include "lyra/lowering/ast_to_mir/literal.hpp"

#include <cstdint>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lowering/mir_to_lir/expression/internal.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::mir_to_lir {

using Constant = common::Constant;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerConstantExpression(
    const mir::ConstantExpression& constant_expression, LirBuilder& builder)
    -> lir::TempRef {
  auto result =
      builder.AllocateTemp("const", constant_expression.constant.type);
  auto constant = builder.InternConstant(constant_expression.constant);
  builder.AddInstruction(Instruction::Constant(result, constant));
  return result;
}

auto LowerIdentifierExpression(
    const mir::IdentifierExpression& identifier, LirBuilder& builder)
    -> lir::TempRef {
  // Check if the symbol has a constant value (for parameters).
  // This enables MIR→LIR to emit constant instruction without MIR having
  // a parameter_value field, keeping MIR clean.
  const auto& constant_opt =
      builder.GetSymbolTable().GetConstant(identifier.symbol);
  if (constant_opt.has_value()) {
    auto result = builder.AllocateTemp("param", identifier.type);
    auto interned = builder.InternConstant(*constant_opt);
    builder.AddInstruction(Instruction::Constant(result, interned));
    return result;
  }

  // Create pointer to variable, then load through it
  const auto* pointee = builder.GetContext()->InternType(identifier.type);
  auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
  builder.AddInstruction(Instruction::ResolveVar(ptr, identifier.symbol));

  auto result = builder.AllocateTemp("load", identifier.type);
  builder.AddInstruction(Instruction::Load(result, ptr));
  return result;
}

auto LowerEnumValueExpression(
    const mir::EnumValueExpression& enum_val, LirBuilder& builder)
    -> lir::TempRef {
  auto result = builder.AllocateTemp("enum", enum_val.type);

  // Respect the enum's base type signedness
  auto width = enum_val.type.GetBitWidth();
  bool is_signed = enum_val.type.IsSigned();
  auto constant = is_signed
                      ? builder.InternConstant(
                            Constant::IntegralSigned(enum_val.value, width))
                      : builder.InternConstant(
                            Constant::IntegralUnsigned(
                                static_cast<uint64_t>(enum_val.value), width));

  builder.AddInstruction(Instruction::Constant(result, constant));
  return result;
}

}  // namespace lyra::lowering::mir_to_lir
