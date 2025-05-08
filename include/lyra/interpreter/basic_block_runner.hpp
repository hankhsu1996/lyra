#pragma once

#include "lyra/interpreter/basic_block_result.hpp"
#include "lyra/interpreter/instruction_runner.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/basic_block.hpp"

namespace lyra::interpreter {

class BasicBlockRunner {
 public:
  explicit BasicBlockRunner(SimulationContext& context)
      : instruction_runner_(context), context_(context) {
  }

  auto RunBlock(
      const lir::BasicBlock& block, std::size_t start_instruction_index,
      ProcessEffect& effect) -> BasicBlockResult;

 private:
  InstructionRunner instruction_runner_;
  std::reference_wrapper<SimulationContext> context_;
};

}  // namespace lyra::interpreter
