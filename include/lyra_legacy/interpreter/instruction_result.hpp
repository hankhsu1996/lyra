#pragma once

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "lyra/common/trigger.hpp"
#include "lyra/interpreter/call_frame.hpp"
#include "lyra/lir/context.hpp"

namespace lyra::interpreter {

// Represents the result of executing a single LIR instruction
struct InstructionResult {
  enum class Kind {
    kComplete,
    kContinue,
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

  // For WaitEvent
  std::vector<common::Trigger> triggers{};

  // For Finish - true if terminated via $stop (non-zero exit code)
  bool is_stop = false;

  // For ReturnFromFunction - return address
  size_t return_block_index = 0;
  size_t return_instruction_index = 0;

  // For CallFunction - partially initialized call frame (missing return
  // address) Wrapped in unique_ptr to avoid copy issues with unordered_map
  std::unique_ptr<CallFrame> call_frame;

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

  static auto WaitEvent(std::vector<common::Trigger> triggers)
      -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kWaitEvent,
        .triggers = std::move(triggers),
    };
  }

  static auto Finish(bool stopped = false) -> InstructionResult {
    return InstructionResult{.kind = Kind::kFinish, .is_stop = stopped};
  }

  static auto Jump(lir::LabelRef label) -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kJump,
        .target_label = label,
    };
  }

  static auto CallFunction(
      lir::LabelRef entry_label, std::unique_ptr<CallFrame> frame)
      -> InstructionResult {
    InstructionResult result;
    result.kind = Kind::kCallFunction;
    result.target_label = entry_label;
    result.call_frame = std::move(frame);
    return result;
  }

  static auto ReturnFromFunction(size_t block_index, size_t instruction_index)
      -> InstructionResult {
    return InstructionResult{
        .kind = Kind::kReturnFromFunction,
        .return_block_index = block_index,
        .return_instruction_index = instruction_index,
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
        return fmt::format("Jump -> {}", *target_label);

      case Kind::kCallFunction:
        return fmt::format("CallFunction -> {}", *target_label);

      case Kind::kReturnFromFunction:
        return fmt::format(
            "ReturnFromFunction -> block {}, instr {}", return_block_index,
            return_instruction_index);

      case Kind::kComplete:
      case Kind::kContinue:
        return "";
    }

    return "";
  }
};

}  // namespace lyra::interpreter
