#pragma once

#include <cstddef>

#include "lyra/interpreter/basic_block_result.hpp"
#include "lyra/interpreter/execution_context.hpp"
#include "lyra/interpreter/instruction_runner.hpp"
#include "lyra/lir/basic_block.hpp"

namespace lyra::interpreter {

class BasicBlockRunner {
 public:
  explicit BasicBlockRunner(ExecutionContext& context);

  auto RunBlock(
      const lir::BasicBlock& block, std::size_t start_instruction_index)
      -> BasicBlockResult;

 private:
  InstructionRunner instruction_runner_;
  std::reference_wrapper<ExecutionContext> context_;
};

}  // namespace lyra::interpreter
