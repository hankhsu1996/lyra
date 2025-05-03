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
  kDelay,
  kSystemCall,
  kJump,
  kBranch
};

struct Instruction {
  InstructionKind kind;

  // Destination temporary name, or empty if not applicable
  std::string result;

  // Operand values used by the instruction
  std::vector<Value> operands;

  // System call name
  std::string system_call_name;
  [[nodiscard]] auto ToString() const -> std::string {
    switch (kind) {
      case InstructionKind::kLiteralInt:
      case InstructionKind::kLiteralString:
        return fmt::format("mov {}, {}", result, operands[0].ToString());

      case InstructionKind::kLoadSignal:
        return fmt::format("load {}, {}", result, operands[0].ToString());

      case InstructionKind::kStoreSignal:
        return fmt::format(
            "store {}, {}", operands[0].ToString(), operands[1].ToString());

      case InstructionKind::kBinaryAdd:
        return fmt::format(
            "add {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kDelay:
        return fmt::format("delay {}", operands[0].ToString());

      case InstructionKind::kSystemCall:
        if (operands.empty()) {
          return fmt::format("syscall {}", system_call_name);
        } else {
          std::string args;
          for (size_t i = 0; i < operands.size(); ++i) {
            if (i > 0) {
              args += ", ";
            }
            args += operands[i].ToString();
          }
          return fmt::format("syscall {} {}", system_call_name, args);
        }

      case InstructionKind::kJump:
        if (operands.size() == 1) {
          return fmt::format("jump {}", operands[0].ToString());
        } else {
          return "(invalid jump)";
        }

      case InstructionKind::kBranch:
        if (operands.size() == 3) {
          return fmt::format(
              "branch {}, {}, {}", operands[0].ToString(),
              operands[1].ToString(), operands[2].ToString());
        } else {
          return "(invalid branch)";
        }
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
