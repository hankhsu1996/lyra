#pragma once

#include <cstdint>
#include <optional>
#include <string>

namespace lyra::interpreter {

// Represents the result of executing a single LIR instruction
struct LIRInstructionResult {
  enum class Kind { kContinue, kDelay, kFinish, kJump };

  Kind kind{};

  // For Jump
  std::string target_label{};

  // For Delay
  uint64_t delay_amount = 0;

  // The variable that was modified by this instruction (if any)
  std::optional<std::string> modified_variable{};

  static auto Continue(
      std::optional<std::string> modified_variable = std::nullopt)
      -> LIRInstructionResult {
    return LIRInstructionResult{
        .kind = Kind::kContinue,
        .modified_variable = std::move(modified_variable)};
  }

  static auto Delay(
      uint64_t amount,
      std::optional<std::string> modified_variable = std::nullopt)
      -> LIRInstructionResult {
    return LIRInstructionResult{
        .kind = Kind::kDelay,
        .delay_amount = amount,
        .modified_variable = std::move(modified_variable)};
  }

  static auto Finish(
      std::optional<std::string> modified_variable = std::nullopt)
      -> LIRInstructionResult {
    return LIRInstructionResult{
        .kind = Kind::kFinish,
        .modified_variable = std::move(modified_variable)};
  }

  static auto Jump(
      std::string label,
      std::optional<std::string> modified_variable = std::nullopt)
      -> LIRInstructionResult {
    return LIRInstructionResult{
        .kind = Kind::kJump,
        .target_label = std::move(label),
        .modified_variable = std::move(modified_variable)};
  }
};

}  // namespace lyra::interpreter
