#pragma once

#include <string>
#include <vector>

#include <fmt/core.h>

#include "lir/value.hpp"

namespace volans::lir {

enum class InstructionKind {
  kLiteralInt,
  kLiteralString,
  kLoadSignal,
  kStoreSignal,
  kBinaryAdd,
  kAssign
};

struct Instruction {
  InstructionKind kind;

  // Destination temporary name, or empty if not applicable
  std::string result;

  // Operand values used by the instruction
  std::vector<Value> operands;

  [[nodiscard]] auto ToString() const -> std::string {
    switch (kind) {
      case InstructionKind::kLiteralInt:
        return fmt::format("{} = {}", result, operands[0].ToString());
      case InstructionKind::kLiteralString:
        return fmt::format("{} = {}", result, operands[0].ToString());
      case InstructionKind::kLoadSignal:
        return fmt::format("{} = {}", result, operands[0].ToString());
      case InstructionKind::kStoreSignal:
        return fmt::format("{} = {}", result, operands[0].ToString());
      case InstructionKind::kBinaryAdd:
        return fmt::format(
            "{} = {} + {}", result, operands[0].ToString(),
            operands[1].ToString());
      case InstructionKind::kAssign:
        return fmt::format("{} = {}", result, operands[0].ToString());
      default:
        return fmt::format("{} = {}", result, operands[0].ToString());
    }
  }
};

}  // namespace volans::lir
