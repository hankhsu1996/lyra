#include "lyra/lowering/ast_to_mir/literal.hpp"

#include <cstdint>
#include <stdexcept>
#include <utility>

#include <slang/ast/symbols/ParameterSymbols.h>

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
  // Parameter symbols (from constructor params) need to be resolved to their
  // constant values for the interpreter. In codegen, these are accessed as
  // class members, but the interpreter uses flat symbol tables.
  if (identifier.symbol->kind == slang::ast::SymbolKind::Parameter) {
    const auto& param = identifier.symbol->as<slang::ast::ParameterSymbol>();
    const auto& cv = param.getValue();
    auto constant_result = ast_to_mir::ConstantValueToConstant(cv);
    if (!constant_result) {
      // Struct-typed parameters can't be converted to simple constants.
      // This is a limitation of the interpreter for non-template param types.
      throw std::runtime_error(
          "Unsupported: unpacked struct/string port parameters not yet "
          "supported in interpreter. Use 'lyra run' (codegen) instead.");
    }
    auto result = builder.AllocateTemp("param", identifier.type);
    auto interned = builder.InternConstant(*constant_result);
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
