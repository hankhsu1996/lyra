#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "common/trigger.hpp"
#include "interpreter/actions.hpp"

namespace lyra::interpreter {

// Represents the result of executing a single LIR instruction
struct InstructionResult {
  enum class Kind { kComplete, kContinue, kDelay, kWaitEvent, kFinish, kJump };

  Kind kind{};

  // For Jump
  std::string target_label{};

  // For Delay
  uint64_t delay_amount = 0;

  // For WaitEvent
  std::vector<common::Trigger<std::string>> triggers{};

  std::optional<std::string> modified_variable = std::nullopt;
  std::optional<NbaAction> nba_action = std::nullopt;
  std::optional<PostponedAction> postponed_action = std::nullopt;

  static auto Complete() -> InstructionResult {
    return InstructionResult{.kind = Kind::kComplete};
  }

  static auto Continue(
      std::optional<std::string> modified_variable = std::nullopt)
      -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kContinue,
        .modified_variable = std::move(modified_variable)};
  }

  static auto NbaAction(NbaAction nba_action) -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kContinue, .nba_action = std::move(nba_action)};
  }

  static auto PostponedAction(PostponedAction postponed_action)
      -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kContinue,
        .postponed_action = std::move(postponed_action)};
  }

  static auto Delay(
      uint64_t amount,
      std::optional<std::string> modified_variable = std::nullopt)
      -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kDelay,
        .delay_amount = amount,
        .modified_variable = std::move(modified_variable)};
  }

  static auto WaitEvent(
      std::vector<common::Trigger<std::string>> triggers,
      std::optional<std::string> modified_variable = std::nullopt)
      -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kWaitEvent,
        .triggers = std::move(triggers),
        .modified_variable = std::move(modified_variable)};
  }

  static auto Finish(
      std::optional<std::string> modified_variable = std::nullopt)
      -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kFinish,
        .modified_variable = std::move(modified_variable)};
  }

  static auto Jump(
      std::string label,
      std::optional<std::string> modified_variable = std::nullopt)
      -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kJump,
        .target_label = std::move(label),
        .modified_variable = std::move(modified_variable)};
  }

  [[nodiscard]] auto Summary() const -> std::string {
    if (modified_variable) {
      return fmt::format("modified {}", *modified_variable);
    }

    switch (kind) {
      case Kind::kDelay:
        return fmt::format("Delay {}", delay_amount);

      case Kind::kWaitEvent:
        return "WaitEvent";

      case Kind::kFinish:
        return "Finish";

      case Kind::kJump:
        return fmt::format("Jump -> {}", target_label);

      case Kind::kComplete:
      case Kind::kContinue:
        return "";
    }

    return "";
  }
};

}  // namespace lyra::interpreter
