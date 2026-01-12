#pragma once

#include "lyra/interpreter/hierarchy_context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

// hierarchy_context can be nullptr for flat (non-hierarchical) modules
auto RunInstruction(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessFrame& frame, ProcessEffect& effect,
    const std::shared_ptr<HierarchyContext>& hierarchy_context = nullptr)
    -> InstructionResult;

}  // namespace lyra::interpreter
