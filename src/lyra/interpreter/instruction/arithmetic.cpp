#include "lyra/interpreter/instruction/arithmetic.hpp"

#include <cassert>

#include "lyra/common/internal_error.hpp"
#include "lyra/interpreter/builtin_ops.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

auto HandleArithmeticOps(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  assert(instr.result.has_value());

  switch (instr.kind) {
    // Unary operations
    case lir::InstructionKind::kUnaryPlus:
      return ctx.EvalUnaryOp(
          instr.operands[0], instr.result.value(), UnaryPlus);

    case lir::InstructionKind::kUnaryMinus:
      return ctx.EvalUnaryOp(
          instr.operands[0], instr.result.value(), UnaryMinus);

    case lir::InstructionKind::kUnaryLogicalNot:
      return ctx.EvalUnaryOp(
          instr.operands[0], instr.result.value(), UnaryLogicalNot);

    case lir::InstructionKind::kUnaryBitwiseNot:
      return ctx.EvalUnaryOp(
          instr.operands[0], instr.result.value(), UnaryBitwiseNot);

    // Reduction operations
    case lir::InstructionKind::kReductionAnd:
      return ctx.EvalUnaryOp(
          instr.operands[0], instr.result.value(), ReductionAnd);

    case lir::InstructionKind::kReductionNand:
      return ctx.EvalUnaryOp(
          instr.operands[0], instr.result.value(), ReductionNand);

    case lir::InstructionKind::kReductionOr:
      return ctx.EvalUnaryOp(
          instr.operands[0], instr.result.value(), ReductionOr);

    case lir::InstructionKind::kReductionNor:
      return ctx.EvalUnaryOp(
          instr.operands[0], instr.result.value(), ReductionNor);

    case lir::InstructionKind::kReductionXor:
      return ctx.EvalUnaryOp(
          instr.operands[0], instr.result.value(), ReductionXor);

    case lir::InstructionKind::kReductionXnor:
      return ctx.EvalUnaryOp(
          instr.operands[0], instr.result.value(), ReductionXnor);

    // Binary operations
    case lir::InstructionKind::kBinaryAdd:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryAdd);

    case lir::InstructionKind::kBinarySubtract:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinarySubtract);

    case lir::InstructionKind::kBinaryMultiply:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryMultiply);

    case lir::InstructionKind::kBinaryDivide:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryDivide);

    case lir::InstructionKind::kBinaryModulo:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryModulo);

    case lir::InstructionKind::kBinaryEqual:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryEqual);

    case lir::InstructionKind::kBinaryNotEqual:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryNotEqual);

    case lir::InstructionKind::kBinaryLessThan:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryLessThan);

    case lir::InstructionKind::kBinaryLessThanEqual:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryLessThanEqual);

    case lir::InstructionKind::kBinaryGreaterThan:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryGreaterThan);

    case lir::InstructionKind::kBinaryGreaterThanEqual:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryGreaterThanEqual);

    case lir::InstructionKind::kBinaryPower:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryPower);

    case lir::InstructionKind::kBinaryBitwiseAnd:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryBitwiseAnd);

    case lir::InstructionKind::kBinaryBitwiseOr:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryBitwiseOr);

    case lir::InstructionKind::kBinaryBitwiseXor:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryBitwiseXor);

    case lir::InstructionKind::kBinaryBitwiseXnor:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryBitwiseXnor);

    case lir::InstructionKind::kBinaryLogicalAnd:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryLogicalAnd);

    case lir::InstructionKind::kBinaryLogicalOr:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryLogicalOr);

    case lir::InstructionKind::kBinaryLogicalShiftLeft:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryLogicalShiftLeft);

    case lir::InstructionKind::kBinaryLogicalShiftRight:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryLogicalShiftRight);

    case lir::InstructionKind::kBinaryArithmeticShiftLeft:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryArithmeticShiftLeft);

    case lir::InstructionKind::kBinaryArithmeticShiftRight:
      return ctx.EvalBinaryOp(
          instr.operands[0], instr.operands[1], instr.result.value(),
          BinaryArithmeticShiftRight);

    default:
      throw common::InternalError(
          "interpreter", "unhandled instruction kind in arithmetic handler");
  }
}

}  // namespace lyra::interpreter
