#pragma once

#include <functional>
#include <memory>

#include "core/execution_context.hpp"
#include "interpreter/lir_basic_block_executor.hpp"
#include "interpreter/lir_process_result.hpp"
#include "lir/process.hpp"

namespace lyra::interpreter {

class LIRProcessInterpreter {
 public:
  explicit LIRProcessInterpreter(ExecutionContext& context);

  // Execute an entire process, or until a delay/finish
  auto RunProcess(
      const std::shared_ptr<lir::Process>& process, std::size_t block_index,
      std::size_t instruction_index) -> LIRProcessResult;

 private:
  LIRBasicBlockExecutor block_executor_;
  std::reference_wrapper<ExecutionContext> ctx_;
};

}  // namespace lyra::interpreter
