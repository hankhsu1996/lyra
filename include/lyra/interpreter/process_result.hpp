#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/ranges.h>

#include "lyra/common/trigger.hpp"
#include "lyra/interpreter/actions.hpp"

namespace lyra::interpreter {

// Result of running a Process execution
struct ProcessResult {
  enum class Kind { kComplete, kDelay, kWaitEvent, kFinish };

  Kind kind = Kind::kComplete;
  uint64_t delay_amount = 0;

  // For resuming after a delay or waiting for event
  std::size_t block_index = 0;
  std::size_t resume_instruction_index = 0;

  // For WaitEvent
  std::vector<common::Trigger<std::string>> triggers{};

  // Variables modified during process execution
  std::vector<std::string> modified_variables;

  // Non-blocking writes during process execution
  std::vector<NbaAction> nba_actions;
  std::vector<PostponedAction> postponed_actions;

  static auto Complete(
      std::vector<std::string> modified_variables,
      std::vector<NbaAction> nba_actions,
      std::vector<PostponedAction> postponed_actions) -> ProcessResult {
    return ProcessResult{
        .kind = Kind::kComplete,
        .modified_variables = std::move(modified_variables),
        .nba_actions = std::move(nba_actions),
        .postponed_actions = std::move(postponed_actions)};
  }

  static auto Delay(
      uint64_t amount, std::size_t block_index, std::size_t instruction_index,
      std::vector<std::string> modified_variables,
      std::vector<NbaAction> nba_actions,
      std::vector<PostponedAction> postponed_actions) -> ProcessResult {
    return ProcessResult{
        .kind = Kind::kDelay,
        .delay_amount = amount,
        .block_index = block_index,
        .resume_instruction_index = instruction_index,
        .modified_variables = std::move(modified_variables),
        .nba_actions = std::move(nba_actions),
        .postponed_actions = std::move(postponed_actions)};
  }

  static auto WaitEvent(
      std::vector<common::Trigger<std::string>> triggers,
      std::size_t block_index, std::size_t instruction_index,
      std::vector<std::string> modified_variables,
      std::vector<NbaAction> nba_actions,
      std::vector<PostponedAction> postponed_actions) -> ProcessResult {
    return ProcessResult{
        .kind = Kind::kWaitEvent,
        .block_index = block_index,
        .resume_instruction_index = instruction_index,
        .triggers = std::move(triggers),
        .modified_variables = std::move(modified_variables),
        .nba_actions = std::move(nba_actions),
        .postponed_actions = std::move(postponed_actions)};
  }

  static auto Finish(
      std::vector<std::string> modified_variables,
      std::vector<NbaAction> nba_actions,
      std::vector<PostponedAction> postponed_actions) -> ProcessResult {
    return ProcessResult{
        .kind = Kind::kFinish,
        .modified_variables = std::move(modified_variables),
        .nba_actions = std::move(nba_actions),
        .postponed_actions = std::move(postponed_actions)};
  }

  [[nodiscard]] auto Summary() const -> std::string {
    std::string base;

    switch (kind) {
      case Kind::kDelay:
        base = fmt::format("Delay for {} time units", delay_amount);
        break;

      case Kind::kWaitEvent: {
        std::vector<std::string> trigger_strs;
        for (const auto& trig : triggers) {
          trigger_strs.push_back(trig.ToString());
        }
        base =
            fmt::format("Wait for event(s): {}", fmt::join(trigger_strs, ", "));
        break;
      }

      case Kind::kComplete:
        base = "Complete";
        break;

      case Kind::kFinish:
        base = "Finish";
        break;
    }

    if (!modified_variables.empty()) {
      base +=
          fmt::format(", modified: {}", fmt::join(modified_variables, ", "));
    }

    if (!nba_actions.empty()) {
      std::vector<std::string> nba_actions_str;
      for (const auto& action : nba_actions) {
        nba_actions_str.push_back(action.ToString());
      }
      base += fmt::format(", nba: {}", fmt::join(nba_actions_str, ", "));
    }

    return base;
  }
};

}  // namespace lyra::interpreter
