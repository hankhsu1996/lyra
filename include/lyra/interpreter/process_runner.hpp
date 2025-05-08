#pragma once

#include <functional>
#include <memory>

#include "lyra/interpreter/basic_block_runner.hpp"
#include "lyra/interpreter/process_result.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::interpreter {

class ProcessRunner {
 public:
  explicit ProcessRunner(SimulationContext& context);

  // Execute an entire process, or until a delay/finish
  auto RunProcess(
      const std::shared_ptr<lir::Process>& process, std::size_t block_index,
      std::size_t instruction_index) -> ProcessResult;

 private:
  BasicBlockRunner block_runner_;
  std::reference_wrapper<SimulationContext> context_;
};

}  // namespace lyra::interpreter
