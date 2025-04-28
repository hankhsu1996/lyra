#pragma once

#include "core/execution_context.hpp"
#include "lir/module.hpp"

namespace lyra::lir {

class Executor {
 public:
  Executor(const Module& module, lyra::ExecutionContext& context);

  // Run initial processes and the first evaluation of always processes
  void RunInitial();

  // Run always processes (for combinational evaluation and triggered processes)
  void RunAlways();

 private:
  // Execute a single instruction
  void ExecuteInstruction(const Instruction& instr);

  std::reference_wrapper<const Module> module_;
  std::reference_wrapper<lyra::ExecutionContext> ctx_;
};

}  // namespace lyra::lir
