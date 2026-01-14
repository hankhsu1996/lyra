#include "lyra/interpreter/instruction_runner.hpp"

#include <memory>

#include "lyra/interpreter/hierarchy_context.hpp"
#include "lyra/interpreter/instruction/arithmetic.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction/control.hpp"
#include "lyra/interpreter/instruction/memory.hpp"
#include "lyra/interpreter/instruction/type.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

auto RunInstruction(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessFrame& frame, ProcessEffect& effect,
    const std::shared_ptr<HierarchyContext>& hierarchy_context)
    -> InstructionResult {
  // Compute execution scope once at instruction start.
  // This eliminates repeated call_stack.empty() branching in GetTempType, etc.
  ExecutionScope scope =
      frame.call_stack.empty()
          ? ExecutionScope{&frame.process->temps, &frame.temp_table}
          : ExecutionScope{
                &frame.call_stack.back().function->temps,
                &frame.call_stack.back().temp_table};

  InstructionContext ctx(
      simulation_context, frame, effect, scope, hierarchy_context);

  // Dispatch to appropriate handler based on instruction category
  switch (lir::GetInstructionCategory(instr.kind)) {
    case lir::InstructionCategory::kMemory:
      return HandleMemoryOps(instr, ctx);
    case lir::InstructionCategory::kArithmetic:
      return HandleArithmeticOps(instr, ctx);
    case lir::InstructionCategory::kType:
      return HandleTypeOps(instr, ctx);
    case lir::InstructionCategory::kControl:
      return HandleControlFlowOps(instr, ctx);
  }
}

}  // namespace lyra::interpreter
