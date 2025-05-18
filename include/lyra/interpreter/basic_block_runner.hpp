#pragma once

#include "lyra/interpreter/basic_block_result.hpp"
#include "lyra/interpreter/process_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/basic_block.hpp"

namespace lyra::interpreter {

auto RunBlock(
    const lir::BasicBlock& block, std::size_t start_instruction_index,
    SimulationContext& simulation_context, ProcessContext& process_context,
    ProcessEffect& effect) -> BasicBlockResult;

}  // namespace lyra::interpreter
