#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}  // namespace lyra::lir

namespace lyra::interpreter {

class InstructionContext;

// Executes a system call instruction ($display, $time, $finish, etc.)
// Returns the appropriate InstructionResult based on the system call.
auto RunSystemCall(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
