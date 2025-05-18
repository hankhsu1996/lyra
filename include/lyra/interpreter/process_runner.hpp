#pragma once

#include <memory>

#include "lyra/interpreter/process_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_result.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::interpreter {

// Execute an entire process, or until a delay/finish
auto RunProcess(
    const std::shared_ptr<lir::Process>& process, std::size_t block_index,
    std::size_t instruction_index, SimulationContext& simulation_context,
    ProcessContext& process_context, ProcessEffect& effect) -> ProcessResult;

}  // namespace lyra::interpreter
