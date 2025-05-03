#pragma once

#include <string>
#include <vector>

#include <fmt/core.h>

#include "lir/value.hpp"

namespace lyra::lir {

enum class InstructionKind {
  kLiteralBit,
  kLiteralInt,
  kLiteralLongInt,
  kLiteralString,
  kLoadVariable,
  kStoreVariable,
  kBinaryAdd,
  kBinarySubtract,
  kBinaryMultiply,
  kBinaryDivide,
  kBinaryModulo,
  kBinaryEqualInt,
  kBinaryEqualString,
  kBinaryNotEqualInt,
  kBinaryNotEqualString,
  kBinaryLessThan,
  kBinaryLessThanEqual,
  kBinaryGreaterThan,
  kBinaryGreaterThanEqual,
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
      case InstructionKind::kLiteralBit:
      case InstructionKind::kLiteralInt:
      case InstructionKind::kLiteralLongInt:
      case InstructionKind::kLiteralString:
        return fmt::format("mov {}, {}", result, operands[0].ToString());

      case InstructionKind::kLoadVariable:
        return fmt::format("load {}, {}", result, operands[0].ToString());

      case InstructionKind::kStoreVariable:
        return fmt::format(
            "store {}, {}", operands[0].ToString(), operands[1].ToString());

      case InstructionKind::kBinaryAdd:
        return fmt::format(
            "add {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinarySubtract:
        return fmt::format(
            "sub {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryMultiply:
        return fmt::format(
            "mul {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryDivide:
        return fmt::format(
            "div {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryModulo:
        return fmt::format(
            "mod {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryEqualInt:
        return fmt::format(
            "eq {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryEqualString:
        return fmt::format(
            "eq {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryNotEqualInt:
        return fmt::format(
            "neq {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryNotEqualString:
        return fmt::format(
            "neq {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLessThan:
        return fmt::format(
            "lt {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLessThanEqual:
        return fmt::format(
            "lteq {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryGreaterThan:
        return fmt::format(
            "gt {}, {}, {}", result, operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryGreaterThanEqual:
        return fmt::format(
            "gteq {}, {}, {}", result, operands[0].ToString(),
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
