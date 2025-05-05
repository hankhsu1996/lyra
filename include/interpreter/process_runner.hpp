#pragma once

#include <functional>
#include <memory>

#include "interpreter/basic_block_runner.hpp"
#include "interpreter/process_result.hpp"
#include "lir/process.hpp"
#include "runtime/execution_context.hpp"

namespace lyra::interpreter {

class ProcessRunner {
 public:
  explicit ProcessRunner(ExecutionContext& context);

  // Execute an entire process, or until a delay/finish
  auto RunProcess(
      const std::shared_ptr<lir::Process>& process, std::size_t block_index,
      std::size_t instruction_index) -> ProcessResult;

 private:
  BasicBlockRunner block_runner_;
  std::reference_wrapper<ExecutionContext> ctx_;
};

}  // namespace lyra::interpreter
