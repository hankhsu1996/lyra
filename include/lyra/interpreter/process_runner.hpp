#pragma once

#include <memory>

#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/process_result.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::interpreter {

// Execute an entire process, or until a delay/finish
// instance_context can be nullptr for flat (non-hierarchical) modules
auto RunProcess(
    const std::shared_ptr<lir::Process>& process, std::size_t block_index,
    std::size_t instruction_index, SimulationContext& simulation_context,
    ProcessFrame& frame, ProcessEffect& effect,
    const std::shared_ptr<InstanceContext>& instance_context = nullptr)
    -> ProcessResult;

}  // namespace lyra::interpreter
