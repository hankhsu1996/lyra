#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

#include "common/trigger.hpp"

namespace lyra::interpreter {

struct LIRBasicBlockResult {
  enum class Kind { kFallthrough, kDelay, kWaitEvent, kFinish, kJump };

  Kind kind = Kind::kFallthrough;

  // For Jump
  std::string target_label{};

  // For Delay
  uint64_t delay_amount = 0;
  std::size_t resume_instruction_index = 0;

  // For WaitEvent
  std::vector<common::Trigger<std::string>> triggers{};

  // Variables modified during block execution
  std::vector<std::string> modified_variables;

  static auto Fallthrough(std::vector<std::string> modified_variables)
      -> LIRBasicBlockResult {
    return {
        .kind = Kind::kFallthrough,
        .modified_variables = std::move(modified_variables)};
  }

  static auto Delay(
      uint64_t amount, std::size_t resume_at,
      std::vector<std::string> modified_variables) -> LIRBasicBlockResult {
    return {
        .kind = Kind::kDelay,
        .delay_amount = amount,
        .resume_instruction_index = resume_at,
        .modified_variables = std::move(modified_variables)};
  }

  static auto WaitEvent(
      std::vector<common::Trigger<std::string>> triggers, std::size_t resume_at,
      std::vector<std::string> modified_variables) -> LIRBasicBlockResult {
    return {
        .kind = Kind::kWaitEvent,
        .resume_instruction_index = resume_at,
        .triggers = std::move(triggers),
        .modified_variables = std::move(modified_variables)};
  }

  static auto Finish(std::vector<std::string> modified_variables)
      -> LIRBasicBlockResult {
    return {
        .kind = Kind::kFinish,
        .modified_variables = std::move(modified_variables)};
  }

  static auto Jump(
      std::string label, std::vector<std::string> modified_variables)
      -> LIRBasicBlockResult {
    return {
        .kind = Kind::kJump,
        .target_label = std::move(label),
        .modified_variables = std::move(modified_variables)};
  }
};

}  // namespace lyra::interpreter
