#pragma once

#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/module.hpp"

namespace lyra::interpreter {

// instance_context can be nullptr for flat (non-hierarchical) modules
// module: the LIR module containing function definitions for kCall
auto RunInstruction(
    const lir::Instruction& instr, const lir::Module& module,
    SimulationContext& simulation_context, ProcessContext& process_context,
    ProcessEffect& effect,
    const std::shared_ptr<InstanceContext>& instance_context = nullptr)
    -> InstructionResult;

}  // namespace lyra::interpreter
