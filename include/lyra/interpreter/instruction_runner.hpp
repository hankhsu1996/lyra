#pragma once

#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

class InstructionRunner {
 public:
  explicit InstructionRunner(SimulationContext& context) : context_(context) {
  }

  auto RunInstruction(const lir::Instruction& instr, ProcessEffect& effect)
      -> InstructionResult;

 private:
  std::reference_wrapper<SimulationContext> context_;
};

}  // namespace lyra::interpreter
