#pragma once

#include <functional>

#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

class InstructionRunner {
 public:
  explicit InstructionRunner(SimulationContext& context);

  auto RunInstruction(const lir::Instruction& instr) -> InstructionResult;

 private:
  std::reference_wrapper<SimulationContext> ctx_;
};

}  // namespace lyra::interpreter
