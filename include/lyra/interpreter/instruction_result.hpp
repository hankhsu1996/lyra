#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/common/trigger.hpp"

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

  static auto Complete() -> InstructionResult {
    return InstructionResult{.kind = Kind::kComplete};
  }

  static auto Continue() -> InstructionResult {
    return InstructionResult{.kind = Kind::kContinue};
  }

  static auto Delay(uint64_t amount) -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kDelay,
        .delay_amount = amount,
    };
  }

  static auto WaitEvent(std::vector<common::Trigger<std::string>> triggers)
      -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kWaitEvent,
        .triggers = std::move(triggers),
    };
  }

  static auto Finish() -> InstructionResult {
    return InstructionResult{.kind = Kind::kFinish};
  }

  static auto Jump(std::string label) -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kJump,
        .target_label = std::move(label),
    };
  }

  [[nodiscard]] auto Summary() const -> std::string {
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
