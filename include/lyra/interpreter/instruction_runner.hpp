#pragma once

#include <functional>

#include "lyra/interpreter/execution_context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

class InstructionRunner {
 public:
  explicit InstructionRunner(ExecutionContext& context);

  auto RunInstruction(const lir::Instruction& instr) -> InstructionResult;

 private:
  std::reference_wrapper<ExecutionContext> ctx_;
};

}  // namespace lyra::interpreter
