#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

#include "common/trigger.hpp"
#include "interpreter/actions.hpp"

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
};

}  // namespace lyra::interpreter
