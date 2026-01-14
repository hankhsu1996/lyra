#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}

namespace lyra::interpreter {

class InstructionContext;

/// Handle control flow operations (jump, branch, call, return, wait, captures).
auto HandleControlFlowOps(
    const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
