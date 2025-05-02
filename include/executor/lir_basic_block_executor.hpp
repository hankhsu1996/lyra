#pragma once

#include <cstddef>

#include <lir/basic_block.hpp>

#include "core/execution_context.hpp"
#include "executor/lir_basic_block_result.hpp"
#include "executor/lir_instruction_executor.hpp"

namespace lyra {

class LIRBasicBlockExecutor {
 public:
  explicit LIRBasicBlockExecutor(ExecutionContext& context);

  auto RunBlock(const lir::BasicBlock& block, std::size_t start_instr_index)
      -> LIRBasicBlockResult;

 private:
  lir::LIRInstructionExecutor instr_executor_;
  std::reference_wrapper<ExecutionContext> ctx_;
};

}  // namespace lyra
