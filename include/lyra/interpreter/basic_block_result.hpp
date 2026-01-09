#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "lyra/common/trigger.hpp"
#include "lyra/interpreter/call_frame.hpp"
#include "lyra/lir/context.hpp"

namespace lyra::interpreter {

struct BasicBlockResult {
  enum class Kind {
    kComplete,
    kDelay,
    kWaitEvent,
    kFinish,
    kJump,
    kCallFunction,       // Call function, jump to entry label
    kReturnFromFunction  // Return from function, resume at saved location
  };

  Kind kind{};

  // For Jump and CallFunction
  lir::LabelRef target_label{};

  // For Delay
  uint64_t delay_amount = 0;
  std::size_t resume_instruction_index = 0;

  // For WaitEvent
  std::vector<common::Trigger> triggers{};

  // For Finish - true if terminated via $stop (non-zero exit code)
  bool is_stop = false;

  // For ReturnFromFunction - return address
  size_t return_block_index = 0;
  size_t return_instruction_index = 0;

  // For CallFunction - partially initialized call frame (missing return
  // address)
  std::unique_ptr<CallFrame> call_frame;

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

  static auto Finish(bool stopped = false) -> BasicBlockResult {
    return {.kind = Kind::kFinish, .is_stop = stopped};
  }

  static auto Jump(lir::LabelRef label) -> BasicBlockResult {
    return {
        .kind = Kind::kJump,
        .target_label = label,
    };
  }

  static auto CallFunction(
      lir::LabelRef entry_label, std::size_t resume_at,
      std::unique_ptr<CallFrame> frame) -> BasicBlockResult {
    BasicBlockResult result;
    result.kind = Kind::kCallFunction;
    result.target_label = entry_label;
    result.resume_instruction_index = resume_at;
    result.call_frame = std::move(frame);
    return result;
  }

  static auto ReturnFromFunction(size_t block_index, size_t instruction_index)
      -> BasicBlockResult {
    return {
        .kind = Kind::kReturnFromFunction,
        .return_block_index = block_index,
        .return_instruction_index = instruction_index,
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
    case BasicBlockResult::Kind::kCallFunction:
      return "CallFunction";
    case BasicBlockResult::Kind::kReturnFromFunction:
      return "ReturnFromFunction";
  }
}

}  // namespace lyra::interpreter
