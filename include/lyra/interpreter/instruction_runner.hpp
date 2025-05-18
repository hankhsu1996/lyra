#pragma once

#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

auto RunInstruction(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessContext& process_context, ProcessEffect& effect)
    -> InstructionResult;

}  // namespace lyra::interpreter
