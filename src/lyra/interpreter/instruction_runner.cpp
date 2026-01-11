#include "lyra/interpreter/instruction_runner.hpp"

#include <memory>

#include "lyra/common/internal_error.hpp"
#include "lyra/interpreter/instruction/arithmetic.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction/control.hpp"
#include "lyra/interpreter/instruction/memory.hpp"
#include "lyra/interpreter/instruction/type.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

namespace {

/// Check if instruction kind is a memory operation.
auto IsMemoryOp(lir::InstructionKind kind) -> bool {
  switch (kind) {
    case lir::InstructionKind::kLiteral:
    case lir::InstructionKind::kLoadVariable:
    case lir::InstructionKind::kStoreVariable:
    case lir::InstructionKind::kStoreVariableNonBlocking:
    case lir::InstructionKind::kLoadElement:
    case lir::InstructionKind::kStoreElement:
    case lir::InstructionKind::kStoreElementNonBlocking:
    case lir::InstructionKind::kCreateAggregate:
    case lir::InstructionKind::kNewDynamicArray:
    case lir::InstructionKind::kLoadPackedBits:
    case lir::InstructionKind::kStorePackedBits:
    case lir::InstructionKind::kMove:
      return true;
    default:
      return false;
  }
}

/// Check if instruction kind is an arithmetic operation.
auto IsArithmeticOp(lir::InstructionKind kind) -> bool {
  switch (kind) {
    case lir::InstructionKind::kUnaryPlus:
    case lir::InstructionKind::kUnaryMinus:
    case lir::InstructionKind::kUnaryLogicalNot:
    case lir::InstructionKind::kUnaryBitwiseNot:
    case lir::InstructionKind::kReductionAnd:
    case lir::InstructionKind::kReductionNand:
    case lir::InstructionKind::kReductionOr:
    case lir::InstructionKind::kReductionNor:
    case lir::InstructionKind::kReductionXor:
    case lir::InstructionKind::kReductionXnor:
    case lir::InstructionKind::kBinaryAdd:
    case lir::InstructionKind::kBinarySubtract:
    case lir::InstructionKind::kBinaryMultiply:
    case lir::InstructionKind::kBinaryDivide:
    case lir::InstructionKind::kBinaryModulo:
    case lir::InstructionKind::kBinaryEqual:
    case lir::InstructionKind::kBinaryNotEqual:
    case lir::InstructionKind::kBinaryLessThan:
    case lir::InstructionKind::kBinaryLessThanEqual:
    case lir::InstructionKind::kBinaryGreaterThan:
    case lir::InstructionKind::kBinaryGreaterThanEqual:
    case lir::InstructionKind::kBinaryPower:
    case lir::InstructionKind::kBinaryBitwiseAnd:
    case lir::InstructionKind::kBinaryBitwiseOr:
    case lir::InstructionKind::kBinaryBitwiseXor:
    case lir::InstructionKind::kBinaryBitwiseXnor:
    case lir::InstructionKind::kBinaryLogicalAnd:
    case lir::InstructionKind::kBinaryLogicalOr:
    case lir::InstructionKind::kBinaryLogicalShiftLeft:
    case lir::InstructionKind::kBinaryLogicalShiftRight:
    case lir::InstructionKind::kBinaryArithmeticShiftLeft:
    case lir::InstructionKind::kBinaryArithmeticShiftRight:
      return true;
    default:
      return false;
  }
}

/// Check if instruction kind is a type operation.
auto IsTypeOp(lir::InstructionKind kind) -> bool {
  switch (kind) {
    case lir::InstructionKind::kConversion:
    case lir::InstructionKind::kConcatenation:
      return true;
    default:
      return false;
  }
}

/// Check if instruction kind is a control flow operation.
auto IsControlFlowOp(lir::InstructionKind kind) -> bool {
  switch (kind) {
    case lir::InstructionKind::kComplete:
    case lir::InstructionKind::kWaitEvent:
    case lir::InstructionKind::kDelay:
    case lir::InstructionKind::kSystemCall:
    case lir::InstructionKind::kMethodCall:
    case lir::InstructionKind::kJump:
    case lir::InstructionKind::kBranch:
    case lir::InstructionKind::kCall:
    case lir::InstructionKind::kReturn:
    case lir::InstructionKind::kLoadCapture:
    case lir::InstructionKind::kStoreCapture:
      return true;
    default:
      return false;
  }
}

}  // namespace

auto RunInstruction(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessFrame& frame, ProcessEffect& effect,
    const std::shared_ptr<InstanceContext>& instance_context)
    -> InstructionResult {
  // Use function-local temp table when inside a function
  auto& temp_table = frame.call_stack.empty()
                         ? frame.temp_table
                         : frame.call_stack.back().temp_table;

  InstructionContext ctx(
      simulation_context, frame, effect, temp_table, instance_context);

  // Dispatch to appropriate handler based on instruction kind
  if (IsMemoryOp(instr.kind)) {
    return HandleMemoryOps(instr, ctx);
  }

  if (IsArithmeticOp(instr.kind)) {
    return HandleArithmeticOps(instr, ctx);
  }

  if (IsTypeOp(instr.kind)) {
    return HandleTypeOps(instr, ctx);
  }

  if (IsControlFlowOp(instr.kind)) {
    return HandleControlFlowOps(instr, ctx);
  }

  throw common::InternalError(
      "interpreter", "unhandled instruction kind in dispatcher");
}

}  // namespace lyra::interpreter
