#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}

namespace lyra::interpreter {

class InstructionContext;

/// Handle type conversion and concatenation operations.
auto HandleTypeOps(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
