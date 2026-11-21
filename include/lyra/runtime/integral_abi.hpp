#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/integral_operation.hpp"

// How a generated module calls the library to carry out an operation over
// integral values of types the library was compiled without (LRM 11.4, 20.8,
// 20.9).
//
// The library holds one entry per operation a module calls it for, and the
// entries stand in one table, in the order those operations are enumerated in,
// so a module reaches an operation's entry by its position there. An integral
// value crosses as the address of the bytes its type lays it out in, and what
// the operation has to know of a type crosses as machine arguments: how many
// bits wide, whether read as signed where the operation reads a number, whether
// able to hold x or z. What an entry takes follows from the operation's own
// declaration by the one rule below, which the module composing a call and the
// library defining the entry both read.
namespace lyra::runtime {

// The symbol the table of entries is published under.
inline constexpr std::string_view kIntegralEntriesSymbol =
    "lyra_rt_integral_operations";

// Whether a generated module ever calls the library to carry `op` out, which
// is what gives the operation an entry: an operation the library carries out
// over a value wider than two words, or over one of at most two, and one whose
// amount may be wider than what it moves (LRM 11.4.10, 11.4.3).
[[nodiscard]] constexpr auto HasAnEntry(support::IntegralOp op) -> bool {
  const support::IntegralOperation& operation =
      support::IntegralOperationOf(op);
  const bool wide_is_a_call = [&] {
    switch (operation.wide) {
      case support::WideAction::kLibCall:
        return true;
      case support::WideAction::kExpand:
      case support::WideAction::kNever:
        return false;
    }
    throw InternalError("unknown wide action");
  }();
  const bool primitive_is_a_call = [&] {
    switch (operation.primitive) {
      case support::PrimitiveAction::kLibCall:
        return true;
      case support::PrimitiveAction::kLegal:
        return false;
    }
    throw InternalError("unknown primitive action");
  }();
  return wide_is_a_call || primitive_is_a_call || operation.reads_an_amount;
}

namespace detail {

[[nodiscard]] constexpr auto CountOfEntries() -> std::size_t {
  std::size_t count = 0;
  for (const support::IntegralOperation& operation :
       support::IntegralOperations()) {
    if (HasAnEntry(operation.op)) {
      ++count;
    }
  }
  return count;
}

}  // namespace detail

// The operations with an entry, in the order they are enumerated in; each
// entry stands in the table at its operation's position here.
inline constexpr auto kOperationsWithAnEntry = [] {
  std::array<support::IntegralOp, detail::CountOfEntries()> operations{};
  std::size_t at = 0;
  for (const support::IntegralOperation& operation :
       support::IntegralOperations()) {
    if (HasAnEntry(operation.op)) {
      operations.at(at) = operation.op;
      ++at;
    }
  }
  return operations;
}();

// The position of each operation's entry in the table, where it has one, by
// the position of the operation in the order operations are enumerated in.
inline constexpr auto kEntryRows = [] {
  std::array<std::optional<std::size_t>, support::IntegralOperations().size()>
      rows{};
  std::size_t row = 0;
  for (std::size_t at = 0; at < rows.size(); ++at) {
    if (HasAnEntry(support::IntegralOperations()[at].op)) {
      rows.at(at) = row;
      ++row;
    }
  }
  return rows;
}();

// The position of the entry carrying out `op` in the table.
[[nodiscard]] constexpr auto EntryRowOf(support::IntegralOp op) -> std::size_t {
  const std::optional<std::size_t> row = kEntryRows.at(std::to_underlying(op));
  if (!row.has_value()) {
    throw InternalError(
        std::string("the integral operation '") +
        std::string(support::IntegralOperationOf(op).name) +
        "' is called in the library, which has no entry for it -- please "
        "report this as a bug");
  }
  return *row;
}

// One fact of an integral type as an entry is told it.
enum class IntegralTypeFact : std::uint8_t {
  kWidth,
  kIsSigned,
  kIsFourState,
};

// What an entry is handed in one argument: an operand; the storage it lays an
// integral answer out in; or one fact of the type of an operand, or of the
// answer.
struct OperandArgument {
  std::size_t operand = 0;
};
struct AnswerStorageArgument {};
struct OperandTypeArgument {
  std::size_t operand = 0;
  IntegralTypeFact fact = IntegralTypeFact::kWidth;
};
struct AnswerTypeArgument {
  IntegralTypeFact fact = IntegralTypeFact::kWidth;
};
using IntegralEntryArgument = std::variant<
    OperandArgument, AnswerStorageArgument, OperandTypeArgument,
    AnswerTypeArgument>;

// The arguments of one entry, in the order it takes them.
class IntegralEntryArguments {
 public:
  constexpr void Add(const IntegralEntryArgument& argument) {
    arguments_.at(count_) = argument;
    ++count_;
  }

  [[nodiscard]] constexpr auto All() const
      -> std::span<const IntegralEntryArgument> {
    return std::span<const IntegralEntryArgument>(arguments_).first(count_);
  }

 private:
  // Three operands, the answer's storage, three facts of each operand's type
  // and two of the answer's.
  static constexpr std::size_t kMost = 15;
  std::array<IntegralEntryArgument, kMost> arguments_{};
  std::size_t count_ = 0;
};

// Whether an operation's answer is an integral value, which its entry lays out
// in storage the caller gives; a machine answer is what the entry returns.
[[nodiscard]] constexpr auto AnswersAnIntegralValue(
    support::IntegralAnswer answer) -> bool {
  switch (answer) {
    case support::IntegralAnswer::kOfFirstOperand:
    case support::IntegralAnswer::kOneBit:
    case support::IntegralAnswer::kTwoStateBit:
    case support::IntegralAnswer::kInt:
    case support::IntegralAnswer::kInteger:
    case support::IntegralAnswer::kPosition:
    case support::IntegralAnswer::kOfTheCall:
      return true;
    case support::IntegralAnswer::kMachineBool:
    case support::IntegralAnswer::kMachineInt:
      return false;
  }
  throw InternalError("unknown integral answer");
}

// What the entry carrying out `op` takes: its operands in order; the storage
// for an integral answer; then what it is told of each operand's type, in
// operand order -- width and states of one it reads the bits of, signedness
// between them for one it reads as a number, nothing of one declared to be of
// the first operand's type or of one that is no integral value; and last the
// width and states of an answer whose type the call states.
[[nodiscard]] constexpr auto IntegralEntryArgumentsOf(support::IntegralOp op)
    -> IntegralEntryArguments {
  const support::IntegralOperation& operation =
      support::IntegralOperationOf(op);
  const std::span<const support::IntegralOperandKind> kinds =
      operation.operands.Kinds();
  IntegralEntryArguments arguments;
  for (std::size_t i = 0; i < kinds.size(); ++i) {
    arguments.Add(OperandArgument{.operand = i});
  }
  if (AnswersAnIntegralValue(operation.answer)) {
    arguments.Add(AnswerStorageArgument{});
  }
  for (std::size_t i = 0; i < kinds.size(); ++i) {
    const auto told = [&](IntegralTypeFact fact) {
      arguments.Add(OperandTypeArgument{.operand = i, .fact = fact});
    };
    switch (kinds[i]) {
      case support::IntegralOperandKind::kBits:
        told(IntegralTypeFact::kWidth);
        told(IntegralTypeFact::kIsFourState);
        break;
      case support::IntegralOperandKind::kNumber:
        told(IntegralTypeFact::kWidth);
        told(IntegralTypeFact::kIsSigned);
        told(IntegralTypeFact::kIsFourState);
        break;
      case support::IntegralOperandKind::kSameType:
      case support::IntegralOperandKind::kMachineInt:
      case support::IntegralOperandKind::kMachineBool:
      case support::IntegralOperandKind::kSvLogic:
      case support::IntegralOperandKind::kText:
      case support::IntegralOperandKind::kCanonicalBits:
      case support::IntegralOperandKind::kCanonicalLogic:
      case support::IntegralOperandKind::kPositions:
        break;
    }
  }
  switch (operation.answer) {
    case support::IntegralAnswer::kOfTheCall:
      arguments.Add(AnswerTypeArgument{.fact = IntegralTypeFact::kWidth});
      arguments.Add(AnswerTypeArgument{.fact = IntegralTypeFact::kIsFourState});
      break;
    case support::IntegralAnswer::kOfFirstOperand:
    case support::IntegralAnswer::kOneBit:
    case support::IntegralAnswer::kTwoStateBit:
    case support::IntegralAnswer::kInt:
    case support::IntegralAnswer::kInteger:
    case support::IntegralAnswer::kPosition:
    case support::IntegralAnswer::kMachineBool:
    case support::IntegralAnswer::kMachineInt:
      break;
  }
  return arguments;
}

// The machine type one argument crosses as.
enum class IntegralEntryMachineType : std::uint8_t {
  kAddress,
  kStorage,
  kInt64,
  kBool,
  kByte,
};

[[nodiscard]] constexpr auto MachineTypeOf(
    support::IntegralOp op, const IntegralEntryArgument& argument)
    -> IntegralEntryMachineType {
  const auto of_fact = [](IntegralTypeFact fact) {
    switch (fact) {
      case IntegralTypeFact::kWidth:
        return IntegralEntryMachineType::kInt64;
      case IntegralTypeFact::kIsSigned:
      case IntegralTypeFact::kIsFourState:
        return IntegralEntryMachineType::kBool;
    }
    throw InternalError("unknown integral type fact");
  };
  return std::visit(
      Overloaded{
          [&](const OperandArgument& operand) {
            switch (support::IntegralOperationOf(op)
                        .operands.Kinds()[operand.operand]) {
              case support::IntegralOperandKind::kBits:
              case support::IntegralOperandKind::kNumber:
              case support::IntegralOperandKind::kSameType:
              case support::IntegralOperandKind::kText:
              case support::IntegralOperandKind::kCanonicalBits:
              case support::IntegralOperandKind::kCanonicalLogic:
              case support::IntegralOperandKind::kPositions:
                return IntegralEntryMachineType::kAddress;
              case support::IntegralOperandKind::kMachineInt:
                return IntegralEntryMachineType::kInt64;
              case support::IntegralOperandKind::kMachineBool:
                return IntegralEntryMachineType::kBool;
              case support::IntegralOperandKind::kSvLogic:
                return IntegralEntryMachineType::kByte;
            }
            throw InternalError("unknown integral operand kind");
          },
          [](const AnswerStorageArgument&) {
            return IntegralEntryMachineType::kStorage;
          },
          [&](const OperandTypeArgument& told) { return of_fact(told.fact); },
          [&](const AnswerTypeArgument& told) { return of_fact(told.fact); }},
      argument);
}

}  // namespace lyra::runtime
