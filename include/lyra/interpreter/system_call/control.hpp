#pragma once

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}  // namespace lyra::lir

namespace lyra::interpreter {

class SystemCallContext;

/// Handle simulation control and severity tasks.
/// Includes: $finish, $stop, $exit, $fatal, $error, $warning, $info,
///           $test$plusargs, $value$plusargs
auto HandleControlCalls(const lir::Instruction& instr, SystemCallContext& ctx)
    -> InstructionResult;

}  // namespace lyra::interpreter
