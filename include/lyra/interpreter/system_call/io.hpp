#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}  // namespace lyra::lir

namespace lyra::interpreter {

class SystemCallContext;

/// Handle memory I/O system tasks.
/// Includes: $readmemh, $readmemb, $writememh, $writememb
auto HandleMemIoCalls(const lir::Instruction& instr, SystemCallContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
