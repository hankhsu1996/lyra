#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/ranges.h>

#include "lyra/common/symbol.hpp"
#include "lyra/common/trigger.hpp"

namespace lyra::interpreter {

using SymbolRef = common::SymbolRef;

// Result of running a Process execution
struct ProcessResult {
  enum class Kind { kComplete, kDelay, kWaitEvent, kFinish };

  Kind kind = Kind::kComplete;
  uint64_t delay_amount = 0;

  // For resuming after a delay or waiting for event
  std::size_t block_index = 0;
  std::size_t resume_instruction_index = 0;

  // For WaitEvent
  std::vector<common::Trigger> triggers{};

  static auto Complete() -> ProcessResult {
    return ProcessResult{
        .kind = Kind::kComplete,
    };
  }

  static auto Delay(
      uint64_t amount, std::size_t block_index, std::size_t instruction_index)
      -> ProcessResult {
    return ProcessResult{
        .kind = Kind::kDelay,
        .delay_amount = amount,
        .block_index = block_index,
        .resume_instruction_index = instruction_index,
    };
  }

  static auto WaitEvent(
      std::vector<common::Trigger> triggers, std::size_t block_index,
      std::size_t instruction_index) -> ProcessResult {
    return ProcessResult{
        .kind = Kind::kWaitEvent,
        .block_index = block_index,
        .resume_instruction_index = instruction_index,
        .triggers = std::move(triggers),
    };
  }

  static auto Finish() -> ProcessResult {
    return ProcessResult{.kind = Kind::kFinish};
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

    return base;
  }
};

}  // namespace lyra::interpreter
