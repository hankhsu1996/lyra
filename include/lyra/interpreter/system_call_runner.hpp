#pragma once

#include <memory>

#include "lyra/interpreter/instruction_result.hpp"

namespace lyra::lir {
struct Instruction;
}  // namespace lyra::lir

namespace lyra::interpreter {

class HierarchyContext;
class ProcessEffect;
class ProcessFrame;
class SimulationContext;
class TempTable;

// Executes a system call instruction ($display, $time, $finish, etc.)
// Returns the appropriate InstructionResult based on the system call.
auto RunSystemCall(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessFrame& frame, ProcessEffect& effect, TempTable& temp_table,
    const std::shared_ptr<HierarchyContext>& hierarchy_context)
    -> InstructionResult;

}  // namespace lyra::interpreter
