#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}

namespace lyra::interpreter {

class InstructionContext;

/// Handle unary, binary, and reduction arithmetic operations.
auto HandleArithmeticOps(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
