#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}  // namespace lyra::lir

namespace lyra::interpreter {

class InstructionContext;

/// Handle display and output system tasks.
/// Includes: $display*, $write*, $strobe*, $monitor*, $monitoron, $monitoroff
auto HandleDisplayCalls(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
