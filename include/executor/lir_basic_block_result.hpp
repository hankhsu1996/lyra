#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace lyra {

struct LIRBasicBlockResult {
  enum class Kind { kFallthrough, kDelay, kFinish, kJump };

  Kind kind = Kind::kFallthrough;

  // For Jump
  std::string target_label{};

  // For Delay
  int64_t delay_amount = 0;
  std::size_t resume_instruction_index = 0;

  // Signals modified during block execution
  std::vector<std::string> modified_signals;

  static auto Fallthrough(std::vector<std::string> modified_signals)
      -> LIRBasicBlockResult {
    return {
        .kind = Kind::kFallthrough,
        .modified_signals = std::move(modified_signals)};
  }

  static auto Delay(
      int64_t amount, std::size_t resume_at,
      std::vector<std::string> modified_signals) -> LIRBasicBlockResult {
    return {
        .kind = Kind::kDelay,
        .delay_amount = amount,
        .resume_instruction_index = resume_at,
        .modified_signals = std::move(modified_signals)};
  }

  static auto Finish(std::vector<std::string> modified_signals)
      -> LIRBasicBlockResult {
    return {
        .kind = Kind::kFinish, .modified_signals = std::move(modified_signals)};
  }

  static auto Jump(std::string label, std::vector<std::string> modified_signals)
      -> LIRBasicBlockResult {
    return {
        .kind = Kind::kJump,
        .target_label = std::move(label),
        .modified_signals = std::move(modified_signals)};
  }
};

}  // namespace lyra
