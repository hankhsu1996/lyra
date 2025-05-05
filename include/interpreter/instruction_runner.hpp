#pragma once

#include <functional>

#include <lir/instruction.hpp>

#include "core/execution_context.hpp"
#include "interpreter/lir_instruction_result.hpp"

namespace lyra::interpreter {

class LIRInstructionExecutor {
 public:
  explicit LIRInstructionExecutor(ExecutionContext& context);

  auto ExecuteInstruction(const lir::Instruction& instr)
      -> LIRInstructionResult;

 private:
  std::reference_wrapper<ExecutionContext> ctx_;
};

}  // namespace lyra::interpreter
