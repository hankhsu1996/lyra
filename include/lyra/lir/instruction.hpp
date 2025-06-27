#pragma once

#include <optional>
#include <string>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/trigger.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::lir {

enum class InstructionKind {
  // Memory operations
  kIntegerLiteral,
  kStringLiteral,
  kLoadVariable,
  kStoreVariable,
  kStoreVariableNonBlocking,

  // Move operation
  kMove,

  // Unary operations
  kUnaryPlus,
  kUnaryMinus,
  kUnaryLogicalNot,
  kUnaryBitwiseNot,

  // Reduction operations
  kReductionAnd,
  kReductionNand,
  kReductionOr,
  kReductionNor,
  kReductionXor,
  kReductionXnor,

  // Binary operations
  kBinaryAdd,
  kBinarySubtract,
  kBinaryMultiply,
  kBinaryDivide,
  kBinaryModulo,
  kBinaryEqual,
  kBinaryNotEqual,
  kBinaryLessThan,
  kBinaryLessThanEqual,
  kBinaryGreaterThan,
  kBinaryGreaterThanEqual,

  // Power operation
  kBinaryPower,

  // Bitwise operations
  kBinaryBitwiseAnd,
  kBinaryBitwiseOr,
  kBinaryBitwiseXor,
  kBinaryBitwiseXnor,

  // Logical operations
  kBinaryLogicalAnd,
  kBinaryLogicalOr,

  // Shift operations
  kBinaryLogicalShiftLeft,
  kBinaryLogicalShiftRight,
  kBinaryArithmeticShiftLeft,
  kBinaryArithmeticShiftRight,

  // Type operations
  kConversion,

  // Control flow
  kComplete,
  kWaitEvent,
  kDelay,
  kSystemCall,
  kJump,
  kBranch
};

struct Instruction {
  InstructionKind kind{};

  // Destination temporary name, or empty if not applicable
  std::optional<TempRef> result{};
  std::optional<common::Type> result_type{};

  // Operand values used by the instruction
  std::vector<Operand> operands{};

  // System call name
  std::string system_call_name{};

  // Event name
  std::vector<common::Trigger> wait_triggers{};

  static auto Basic(
      InstructionKind kind, TempRef result, std::vector<Operand> operands)
      -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .operands = std::move(operands)};
  }

  static auto Basic(InstructionKind kind, TempRef result, Operand operand)
      -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .operands = {std::move(operand)}};
  }

  static auto Basic(InstructionKind kind, TempRef result, TempRef operand)
      -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .operands = {Operand::Temp(operand)}};
  }

  static auto Basic(InstructionKind kind, TempRef result, SymbolRef operand)
      -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .operands = {Operand::Variable(operand)}};
  }

  static auto Basic(InstructionKind kind, TempRef result, LiteralRef operand)
      -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .operands = {Operand::Literal(operand)}};
  }

  static auto WithType(
      InstructionKind kind, TempRef result, TempRef operand,
      common::Type result_type) -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .result_type = std::move(result_type),
        .operands = {Operand::Temp(operand)}};
  }

  static auto StoreVariable(
      SymbolRef variable, TempRef value, bool is_non_blocking) -> Instruction {
    return Instruction{
        .kind = is_non_blocking ? InstructionKind::kStoreVariableNonBlocking
                                : InstructionKind::kStoreVariable,
        .operands = {Operand::Variable(variable), Operand::Temp(value)}};
  }

  static auto WaitEvent(std::vector<common::Trigger> triggers) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kWaitEvent,
        .wait_triggers = std::move(triggers)};
  }

  static auto Delay(Operand delay) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kDelay, .operands = {std::move(delay)}};
  }

  static auto SystemCall(std::string name, std::vector<TempRef> args)
      -> Instruction {
    std::vector<Operand> operands;
    for (auto& arg : args) {
      operands.push_back(Operand::Temp(arg));
    }
    return Instruction{
        .kind = InstructionKind::kSystemCall,
        .result = std::nullopt,
        .result_type = std::nullopt,
        .operands = std::move(operands),
        .system_call_name = std::move(name)};
  }

  static auto Complete() -> Instruction {
    return Instruction{
        .kind = InstructionKind::kComplete,
        .result = std::nullopt,
        .result_type = std::nullopt,
        .operands = {}};
  }

  static auto Jump(LabelRef label) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kJump, .operands = {Operand::Label(label)}};
  }

  static auto Branch(
      TempRef condition, LabelRef true_label, LabelRef false_label)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kBranch,
        .operands = {
            Operand::Temp(condition), Operand::Label(true_label),
            Operand::Label(false_label)}};
  }

  [[nodiscard]] auto ToString() const -> std::string {
    switch (kind) {
      // Memory operations
      case InstructionKind::kIntegerLiteral:
        return fmt::format(
            "lit   {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kStringLiteral:
        return fmt::format(
            "lit   {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kLoadVariable:
        return fmt::format(
            "load  {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kStoreVariable:
        return fmt::format(
            "store {}, {}", operands[0].ToString(), operands[1].ToString());

      case InstructionKind::kStoreVariableNonBlocking:
        return fmt::format(
            "store {}, {}", operands[0].ToString(), operands[1].ToString());

      case InstructionKind::kMove:
        return fmt::format(
            "move  {}, {}", result.value(), operands[0].ToString());

      // Unary operations
      case InstructionKind::kUnaryPlus:
        return fmt::format(
            "plus  {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kUnaryMinus:
        return fmt::format(
            "minus {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kUnaryLogicalNot:
        return fmt::format(
            "not.l {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kUnaryBitwiseNot:
        return fmt::format(
            "not.b {}, {}", result.value(), operands[0].ToString());

      // Reduction operations
      case InstructionKind::kReductionAnd:
        return fmt::format(
            "and.r {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kReductionNand:
        return fmt::format(
            "nand.r {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kReductionOr:
        return fmt::format(
            "or.r  {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kReductionNor:
        return fmt::format(
            "nor.r {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kReductionXor:
        return fmt::format(
            "xor.r {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kReductionXnor:
        return fmt::format(
            "xnor.r {}, {}", result.value(), operands[0].ToString());

      // Binary operations
      case InstructionKind::kBinaryAdd:
        return fmt::format(
            "add   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinarySubtract:
        return fmt::format(
            "sub   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryMultiply:
        return fmt::format(
            "mul   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryDivide:
        return fmt::format(
            "div   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryModulo:
        return fmt::format(
            "mod   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryEqual:
        return fmt::format(
            "eq    {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryNotEqual:
        return fmt::format(
            "neq   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLessThan:
        return fmt::format(
            "lt    {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLessThanEqual:
        return fmt::format(
            "lteq  {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryGreaterThan:
        return fmt::format(
            "gt    {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryGreaterThanEqual:
        return fmt::format(
            "gteq  {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryPower:
        return fmt::format(
            "pow   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryBitwiseAnd:
        return fmt::format(
            "and.b {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryBitwiseOr:
        return fmt::format(
            "or.b  {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryBitwiseXor:
        return fmt::format(
            "xor.b {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryBitwiseXnor:
        return fmt::format(
            "xnor.b {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLogicalAnd:
        return fmt::format(
            "and.l {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLogicalOr:
        return fmt::format(
            "or.l  {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLogicalShiftLeft:
        return fmt::format(
            "shl   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLogicalShiftRight:
        return fmt::format(
            "shr   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryArithmeticShiftLeft:
        return fmt::format(
            "sal   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryArithmeticShiftRight:
        return fmt::format(
            "sar   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kConversion:
        return fmt::format(
            "cvt   {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kWaitEvent: {
        std::string result = "wait  ";
        for (size_t i = 0; i < wait_triggers.size(); ++i) {
          if (i > 0) {
            result += " or ";
          }
          result += wait_triggers[i].ToString();
        }
        return result;
      }

      case InstructionKind::kComplete:
        return "complete";

      case InstructionKind::kDelay:
        return fmt::format("delay {}", operands[0].ToString());

      case InstructionKind::kSystemCall:
        if (operands.empty()) {
          return fmt::format("call  {}", system_call_name);
        } else {
          std::string args;
          for (size_t i = 0; i < operands.size(); ++i) {
            if (i > 0) {
              args += ", ";
            }
            args += operands[i].ToString();
          }
          return fmt::format("call  {} {}", system_call_name, args);
        }

      case InstructionKind::kJump:
        if (operands.size() == 1) {
          return fmt::format("jump  {}", operands[0].ToString());
        } else {
          return "(invalid jump)";
        }

      case InstructionKind::kBranch:
        if (operands.size() == 3) {
          return fmt::format(
              "br    {}, {}, {}", operands[0].ToString(),
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
