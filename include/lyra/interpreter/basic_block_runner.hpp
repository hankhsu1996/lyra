#pragma once

#include "lyra/interpreter/basic_block_result.hpp"
#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/process_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/basic_block.hpp"
#include "lyra/lir/module.hpp"

namespace lyra::interpreter {

// instance_context can be nullptr for flat (non-hierarchical) modules
// module: the LIR module containing function definitions for kCall
auto RunBlock(
    const lir::BasicBlock& block, std::size_t start_instruction_index,
    const lir::Module& module, SimulationContext& simulation_context,
    ProcessContext& process_context, ProcessEffect& effect,
    const std::shared_ptr<InstanceContext>& instance_context = nullptr)
    -> BasicBlockResult;

}  // namespace lyra::interpreter
