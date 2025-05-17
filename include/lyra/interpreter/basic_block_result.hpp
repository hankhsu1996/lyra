#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

#include "lyra/common/trigger.hpp"
#include "lyra/lir/context.hpp"

namespace lyra::interpreter {

struct BasicBlockResult {
  enum class Kind { kComplete, kDelay, kWaitEvent, kFinish, kJump };

  Kind kind{};

  // For Jump
  lir::LabelRef target_label{};

  // For Delay
  uint64_t delay_amount = 0;
  std::size_t resume_instruction_index = 0;

  // For WaitEvent
  std::vector<common::Trigger> triggers{};

  static auto Complete() -> BasicBlockResult {
    return {.kind = Kind::kComplete};
  }

  static auto Delay(uint64_t amount, std::size_t resume_at)
      -> BasicBlockResult {
    return {
        .kind = Kind::kDelay,
        .delay_amount = amount,
        .resume_instruction_index = resume_at,
    };
  }

  static auto WaitEvent(
      std::vector<common::Trigger> triggers, std::size_t resume_at)
      -> BasicBlockResult {
    return {
        .kind = Kind::kWaitEvent,
        .resume_instruction_index = resume_at,
        .triggers = std::move(triggers),
    };
  }

  static auto Finish() -> BasicBlockResult {
    return {.kind = Kind::kFinish};
  }

  static auto Jump(lir::LabelRef label) -> BasicBlockResult {
    return {
        .kind = Kind::kJump,
        .target_label = label,
    };
  }
};

inline auto ToString(BasicBlockResult::Kind kind) -> std::string {
  switch (kind) {
    case BasicBlockResult::Kind::kComplete:
      return "Complete";
    case BasicBlockResult::Kind::kDelay:
      return "Delay";
    case BasicBlockResult::Kind::kWaitEvent:
      return "WaitEvent";
    case BasicBlockResult::Kind::kFinish:
      return "Finish";
    case BasicBlockResult::Kind::kJump:
      return "Jump";
  }
}

}  // namespace lyra::interpreter
