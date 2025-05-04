#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace lyra::interpreter {

// Result of running a Process execution
struct LIRProcessResult {
  enum class Kind { kComplete, kDelay, kFinish };

  Kind kind = Kind::kComplete;
  uint64_t delay_amount = 0;

  // For resuming after a delay
  std::size_t block_index = 0;
  std::size_t resume_instruction_index = 0;

  // Variables modified during process execution
  std::vector<std::string> modified_variables;

  static auto Complete(std::vector<std::string> modified_variables)
      -> LIRProcessResult {
    return LIRProcessResult{
        .kind = Kind::kComplete,
        .modified_variables = std::move(modified_variables)};
  }

  static auto Delay(
      uint64_t amount, std::size_t block_index, std::size_t instruction_index,
      std::vector<std::string> modified_variables) -> LIRProcessResult {
    return LIRProcessResult{
        .kind = Kind::kDelay,
        .delay_amount = amount,
        .block_index = block_index,
        .resume_instruction_index = instruction_index,
        .modified_variables = std::move(modified_variables)};
  }

  static auto Finish(std::vector<std::string> modified_variables)
      -> LIRProcessResult {
    return LIRProcessResult{
        .kind = Kind::kFinish,
        .modified_variables = std::move(modified_variables)};
  }
};

}  // namespace lyra::interpreter
