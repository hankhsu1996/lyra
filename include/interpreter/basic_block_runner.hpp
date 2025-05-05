#pragma once

#include <cstddef>

#include <lir/basic_block.hpp>

#include "interpreter/basic_block_result.hpp"
#include "interpreter/instruction_runner.hpp"
#include "runtime/execution_context.hpp"

namespace lyra::interpreter {

class BasicBlockRunner {
 public:
  explicit BasicBlockRunner(ExecutionContext& context);

  auto RunBlock(
      const lir::BasicBlock& block, std::size_t start_instruction_index)
      -> BasicBlockResult;

 private:
  InstructionRunner instruction_runner_;
  std::reference_wrapper<ExecutionContext> ctx_;
};

}  // namespace lyra::interpreter
