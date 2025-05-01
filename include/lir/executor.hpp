#pragma once

#include "core/execution_context.hpp"
#include "lir/module.hpp"

namespace lyra::lir {

struct ExecuteResult {
  enum class Action {
    kContinue,
    kDelay,
    kFinish,
  };

  Action action{};
  int64_t delay_amount = 0;

  static auto Continue() -> ExecuteResult {
    return ExecuteResult{.action = Action::kContinue};
  }

  static auto Delay(int64_t amount) -> ExecuteResult {
    return ExecuteResult{.action = Action::kDelay, .delay_amount = amount};
  }

  static auto Finish() -> ExecuteResult {
    return ExecuteResult{.action = Action::kFinish};
  }
};

class Executor {
 public:
  Executor(const Module& module, lyra::ExecutionContext& context);

  // Execute a single instruction
  auto ExecuteInstruction(const Instruction& instr) -> ExecuteResult;

 private:
  std::reference_wrapper<const Module> module_;
  std::reference_wrapper<lyra::ExecutionContext> ctx_;
};

}  // namespace lyra::lir
