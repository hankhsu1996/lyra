#pragma once

#include <cstdint>
#include <optional>
#include <string>

namespace lyra::lir {

// Represents the result of executing a single LIR instruction
struct LIRInstructionResult {
  enum class Kind { kContinue, kDelay, kFinish, kJump };

  Kind kind{};

  // For Jump
  std::string target_label{};

  // For Delay
  int64_t delay_amount = 0;

  // The signal that was modified by this instruction (if any)
  std::optional<std::string> modified_signal{};

  static auto Continue(
      std::optional<std::string> modified_signal = std::nullopt)
      -> LIRInstructionResult {
    return LIRInstructionResult{
        .kind = Kind::kContinue, .modified_signal = std::move(modified_signal)};
  }

  static auto Delay(
      int64_t amount, std::optional<std::string> modified_signal = std::nullopt)
      -> LIRInstructionResult {
    return LIRInstructionResult{
        .kind = Kind::kDelay,
        .delay_amount = amount,
        .modified_signal = std::move(modified_signal)};
  }

  static auto Finish(std::optional<std::string> modified_signal = std::nullopt)
      -> LIRInstructionResult {
    return LIRInstructionResult{
        .kind = Kind::kFinish, .modified_signal = std::move(modified_signal)};
  }

  static auto Jump(
      std::string label,
      std::optional<std::string> modified_signal = std::nullopt)
      -> LIRInstructionResult {
    return LIRInstructionResult{
        .kind = Kind::kJump,
        .target_label = std::move(label),
        .modified_signal = std::move(modified_signal)};
  }
};

}  // namespace lyra::lir
