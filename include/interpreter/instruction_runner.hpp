#pragma once

#include <functional>

#include <lir/instruction.hpp>

#include "interpreter/instruction_result.hpp"
#include "runtime/execution_context.hpp"

namespace lyra::interpreter {

class InstructionRunner {
 public:
  explicit InstructionRunner(ExecutionContext& context);

  auto ExecuteInstruction(const lir::Instruction& instr) -> InstructionResult;

 private:
  std::reference_wrapper<ExecutionContext> ctx_;
};

}  // namespace lyra::interpreter
