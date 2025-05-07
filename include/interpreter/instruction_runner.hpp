#pragma once

#include <functional>

#include <lir/instruction.hpp>

#include "interpreter/execution_context.hpp"
#include "interpreter/instruction_result.hpp"

namespace lyra::interpreter {

class InstructionRunner {
 public:
  explicit InstructionRunner(ExecutionContext& context);

  auto RunInstruction(const lir::Instruction& instr) -> InstructionResult;

 private:
  std::reference_wrapper<ExecutionContext> ctx_;
};

}  // namespace lyra::interpreter
