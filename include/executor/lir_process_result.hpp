#pragma once

#include <cstddef>  // For std::size_t
#include <cstdint>
#include <string>
#include <vector>

namespace lyra {

// Result of running a Process execution
struct LIRProcessResult {
  enum class Kind { kComplete, kDelay, kFinish };

  Kind kind = Kind::kComplete;
  int64_t delay_amount = 0;

  // For resuming after a delay
  std::size_t block_index = 0;
  std::size_t resume_instruction_index = 0;

  // Signals modified during process execution
  std::vector<std::string> modified_signals;

  static auto Complete(std::vector<std::string> modified_signals)
      -> LIRProcessResult {
    return LIRProcessResult{
        .kind = Kind::kComplete,
        .modified_signals = std::move(modified_signals)};
  }

  static auto Delay(
      int64_t amount, std::size_t block_idx, std::size_t instr_idx,
      std::vector<std::string> modified_signals) -> LIRProcessResult {
    return LIRProcessResult{
        .kind = Kind::kDelay,
        .delay_amount = amount,
        .block_index = block_idx,
        .resume_instruction_index = instr_idx,
        .modified_signals = std::move(modified_signals)};
  }

  static auto Finish(std::vector<std::string> modified_signals)
      -> LIRProcessResult {
    return LIRProcessResult{
        .kind = Kind::kFinish, .modified_signals = std::move(modified_signals)};
  }
};

}  // namespace lyra
