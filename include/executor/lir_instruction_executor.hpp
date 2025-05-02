#pragma once

#include <functional>

#include <lir/instruction.hpp>

#include "core/execution_context.hpp"
#include "executor/lir_instruction_result.hpp"

namespace lyra::lir {

class LIRInstructionExecutor {
 public:
  explicit LIRInstructionExecutor(ExecutionContext& context);

  auto ExecuteInstruction(const Instruction& instr) -> LIRInstructionResult;

 private:
  std::reference_wrapper<ExecutionContext> ctx_;
};

}  // namespace lyra::lir
