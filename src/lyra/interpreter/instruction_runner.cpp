#include "lyra/interpreter/instruction_runner.hpp"

#include <memory>

#include "lyra/interpreter/instruction/arithmetic.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction/control.hpp"
#include "lyra/interpreter/instruction/memory.hpp"
#include "lyra/interpreter/instruction/type.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

auto RunInstruction(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessFrame& frame, ProcessEffect& effect,
    const std::shared_ptr<InstanceContext>& instance_context)
    -> InstructionResult {
  // Use function-local temp table when inside a function
  auto& temp_table = frame.call_stack.empty()
                         ? frame.temp_table
                         : frame.call_stack.back().temp_table;

  InstructionContext ctx(
      simulation_context, frame, effect, temp_table, instance_context);

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
