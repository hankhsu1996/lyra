#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "common/trigger.hpp"

namespace lyra::interpreter {

// Represents the result of executing a single LIR instruction
struct LIRInstructionResult {
  enum class Kind { kContinue, kDelay, kWaitEvent, kFinish, kJump };

  Kind kind{};

  // For Jump
  std::string target_label{};

  // For Delay
  uint64_t delay_amount = 0;

  // For WaitEvent
  std::vector<common::Trigger<std::string>> triggers{};

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

  static auto WaitEvent(
      std::vector<common::Trigger<std::string>> triggers,
      std::optional<std::string> modified_variable = std::nullopt)
      -> LIRInstructionResult {
    return LIRInstructionResult{
        .kind = Kind::kWaitEvent,
        .triggers = std::move(triggers),
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
