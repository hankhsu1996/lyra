#pragma once

#include <string>
#include <vector>

#include <fmt/core.h>

#include "lir/value.hpp"

namespace lyra::lir {

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
      case InstructionKind::kLiteralString:
      case InstructionKind::kLoadSignal:
        return fmt::format("{} = {}", result, operands[0].ToString());

      case InstructionKind::kStoreSignal:
        return fmt::format(
            "store {}, {}", operands[0].ToString(), operands[1].ToString());

      case InstructionKind::kBinaryAdd:
        return fmt::format(
            "{} = add {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kAssign:
        if (operands.size() == 2) {
          return fmt::format(
              "{} = {}", operands[0].ToString(), operands[1].ToString());
        } else {
          return "(invalid assign)";
        }

      default:
        return "(unknown instruction)";
    }
  }
};

inline auto operator<<(std::ostream& os, const lyra::lir::Instruction& instr)
    -> std::ostream& {
  return os << instr.ToString();
}

}  // namespace lyra::lir

template <>
struct fmt::formatter<lyra::lir::Instruction> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::Instruction& instr, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", instr.ToString());
  }
};
