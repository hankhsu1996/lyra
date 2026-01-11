#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}  // namespace lyra::lir

namespace lyra::interpreter {

class SystemCallContext;

/// Handle math system functions ($ln, $sqrt, $sin, $pow, etc.)
auto HandleMathCalls(const lir::Instruction& instr, SystemCallContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
