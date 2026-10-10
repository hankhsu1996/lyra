#include "lyra/runtime/integral_abi.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <span>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_operation.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

using support::IntegralOp;
using support::IntegralOperandKind;
using support::IntegralOperation;
using support::IntegralOperationOf;

// One argument as an entry received it.
using Word = std::variant<const void*, void*, std::int64_t, bool, std::uint8_t>;

template <typename T>
auto As(const Word& word) -> T {
  const T* held = std::get_if<T>(&word);
  if (held == nullptr) {
    throw InternalError(
        "an integral operation's entry is handed an argument of another "
        "machine type than its declaration gives it");
  }
  return *held;
}

// An integral type as far as an entry is told it. An entry is told the
// signedness only of an operand its operation reads as a number.
struct ToldType {
  value::IntegralExtent extent;
  value::Signedness signedness = value::Signedness::kUnsigned;
};

void Take(ToldType& type, IntegralTypeFact fact, const Word& word) {
  switch (fact) {
    case IntegralTypeFact::kWidth:
      type.extent.width = static_cast<std::uint64_t>(As<std::int64_t>(word));
      return;
    case IntegralTypeFact::kIsSigned:
      type.signedness = As<bool>(word) ? value::Signedness::kSigned
                                       : value::Signedness::kUnsigned;
      return;
    case IntegralTypeFact::kIsFourState:
      type.extent.domain = As<bool>(word) ? value::StateDomain::kFourState
                                          : value::StateDomain::kTwoState;
      return;
  }
  throw InternalError("unknown integral type fact");
}

auto Loaded(const void* bytes, value::IntegralExtent extent)
    -> value::LoadedWords {
  return value::LoadedWords::Load(
      bytes,
      value::IntegralShape{.width = extent.width, .domain = extent.domain});
}

// Carries `op` out over the arguments its entry was handed: reads each as the
// operation's declaration says what it is, loads the planes the integral
// operands' bytes hold, applies the operation, and lays an integral answer out
// in the storage given for it.
auto CarryOut(IntegralOp op, std::span<const Word> words)
    -> value::MachineAnswer {
  constexpr std::size_t kMostOperands = 3;
  const IntegralOperation& operation = IntegralOperationOf(op);
  const std::span<const IntegralOperandKind> kinds = operation.operands.Kinds();
  const IntegralEntryArguments arguments = IntegralEntryArgumentsOf(op);
  if (words.size() != arguments.All().size()) {
    throw InternalError(
        "an integral operation's entry is handed a number of arguments its "
        "declaration does not give it");
  }

  std::array<Word, kMostOperands> handed{};
  std::array<ToldType, kMostOperands> told{};
  ToldType answer_told;
  void* answer_storage = nullptr;
  for (std::size_t i = 0; i < words.size(); ++i) {
    std::visit(
        Overloaded{
            [&](const OperandArgument& argument) {
              handed.at(argument.operand) = words[i];
            },
            [&](const AnswerStorageArgument&) {
              answer_storage = As<void*>(words[i]);
            },
            [&](const OperandTypeArgument& argument) {
              Take(told.at(argument.operand), argument.fact, words[i]);
            },
            [&](const AnswerTypeArgument& argument) {
              Take(answer_told, argument.fact, words[i]);
            }},
        arguments.All()[i]);
  }

  std::array<value::LoadedWords, kMostOperands> loaded{
      value::LoadedWords(value::IntegralShape{}),
      value::LoadedWords(value::IntegralShape{}),
      value::LoadedWords(value::IntegralShape{})};
  std::array<value::IntegralOperand, kMostOperands> operands{};
  std::array<value::IntegralExtent, kMostOperands> extents{};
  std::size_t integral_count = 0;
  const auto integral = [&](std::size_t i, const ToldType& type) {
    loaded.at(i) = Loaded(As<const void*>(handed.at(i)), type.extent);
    extents.at(integral_count++) = type.extent;
    return loaded.at(i).Read();
  };
  for (std::size_t i = 0; i < kinds.size(); ++i) {
    switch (kinds[i]) {
      case IntegralOperandKind::kBits:
        operands.at(i) = value::BitsOperand{
            .planes = integral(i, told.at(i)),
            .width = told.at(i).extent.width};
        break;
      case IntegralOperandKind::kNumber:
        operands.at(i) = value::NumberOperand{
            .planes = integral(i, told.at(i)),
            .width = told.at(i).extent.width,
            .signedness = told.at(i).signedness};
        break;
      case IntegralOperandKind::kSameType:
        operands.at(i) = value::BitsOperand{
            .planes = integral(i, told.front()),
            .width = told.front().extent.width};
        break;
      case IntegralOperandKind::kMachineInt:
        operands.at(i) = As<std::int64_t>(handed.at(i));
        break;
      case IntegralOperandKind::kMachineBool:
        operands.at(i) = As<bool>(handed.at(i));
        break;
      case IntegralOperandKind::kSvLogic:
        operands.at(i) = As<std::uint8_t>(handed.at(i));
        break;
      case IntegralOperandKind::kText:
        operands.at(i) =
            static_cast<const value::String*>(As<const void*>(handed.at(i)))
                ->View();
        break;
      case IntegralOperandKind::kCanonicalBits:
        operands.at(i) =
            static_cast<const svBitVecVal*>(As<const void*>(handed.at(i)));
        break;
      case IntegralOperandKind::kCanonicalLogic:
        operands.at(i) =
            static_cast<const svLogicVecVal*>(As<const void*>(handed.at(i)));
        break;
    }
  }

  const std::span<const value::IntegralOperand> applied =
      std::span<const value::IntegralOperand>(operands).first(kinds.size());
  const auto answered_at =
      [&](value::IntegralExtent extent) -> value::MachineAnswer {
    value::LoadedWords answer(
        value::IntegralShape{.width = extent.width, .domain = extent.domain});
    value::MachineAnswer machine = value::ApplyIntegralOperation(
        op, applied, answer.Write(), extent.width);
    answer.StoreTo(answer_storage);
    return machine;
  };
  const auto answered_as_a_machine_value = [&]() -> value::MachineAnswer {
    return value::ApplyIntegralOperation(op, applied, value::Planes{}, 0);
  };
  return std::visit(
      Overloaded{
          [&](const value::IntegralExtent& extent) {
            return answered_at(extent);
          },
          [&](const value::AnswerOfTheCall&) {
            return answered_at(answer_told.extent);
          },
          [&](const value::MachineBoolAnswer&) {
            return answered_as_a_machine_value();
          },
          [&](const value::MachineIntAnswer&) {
            return answered_as_a_machine_value();
          }},
      value::AnswerTypeOf(
          op, std::span<const value::IntegralExtent>(extents).first(
                  integral_count)));
}

template <IntegralEntryMachineType type>
struct CppTypeOf;
template <>
struct CppTypeOf<IntegralEntryMachineType::kAddress> {
  using Type = const void*;
};
template <>
struct CppTypeOf<IntegralEntryMachineType::kStorage> {
  using Type = void*;
};
template <>
struct CppTypeOf<IntegralEntryMachineType::kInt64> {
  using Type = std::int64_t;
};
template <>
struct CppTypeOf<IntegralEntryMachineType::kBool> {
  using Type = bool;
};
template <>
struct CppTypeOf<IntegralEntryMachineType::kByte> {
  using Type = std::uint8_t;
};

// What an entry returns for an operation answering each way: the machine
// answer of one that has one, and nothing for one whose integral answer it
// lays out in the storage it is given.
template <support::IntegralAnswer answer>
struct ReturnCppTypeOf;
template <>
struct ReturnCppTypeOf<support::IntegralAnswer::kOfFirstOperand> {
  using Type = void;
};
template <>
struct ReturnCppTypeOf<support::IntegralAnswer::kOneBit> {
  using Type = void;
};
template <>
struct ReturnCppTypeOf<support::IntegralAnswer::kTwoStateBit> {
  using Type = void;
};
template <>
struct ReturnCppTypeOf<support::IntegralAnswer::kJoined> {
  using Type = void;
};
template <>
struct ReturnCppTypeOf<support::IntegralAnswer::kInt> {
  using Type = void;
};
template <>
struct ReturnCppTypeOf<support::IntegralAnswer::kInteger> {
  using Type = void;
};
template <>
struct ReturnCppTypeOf<support::IntegralAnswer::kPosition> {
  using Type = void;
};
template <>
struct ReturnCppTypeOf<support::IntegralAnswer::kOfTheCall> {
  using Type = void;
};
template <>
struct ReturnCppTypeOf<support::IntegralAnswer::kMachineBool> {
  using Type = bool;
};
template <>
struct ReturnCppTypeOf<support::IntegralAnswer::kMachineInt> {
  using Type = std::int64_t;
};

// The operation at one row of the table, and what its entry takes. An entry is
// generated per row, by the row's position, which is the position its code
// address takes in the table.
template <std::size_t row>
inline constexpr IntegralOp kOperationAt =
    support::IntegralOperations()[row].op;

template <std::size_t row>
inline constexpr IntegralEntryArguments kArgumentsAt =
    IntegralEntryArgumentsOf(kOperationAt<row>);

template <std::size_t row, std::size_t at>
using ArgumentCppType = typename CppTypeOf<MachineTypeOf(
    kOperationAt<row>, kArgumentsAt<row>.All()[at])>::Type;

template <std::size_t row>
using ReturnCppType =
    typename ReturnCppTypeOf<support::IntegralOperations()[row].answer>::Type;

// The entry carrying out the operation at `row`, whose parameters are the
// arguments the operation's declaration gives it, each of the machine type it
// crosses as.
template <
    std::size_t row,
    typename = std::make_index_sequence<kArgumentsAt<row>.All().size()>>
struct Entry;

template <std::size_t row, std::size_t... at>
struct Entry<row, std::index_sequence<at...>> {
  static auto Call(ArgumentCppType<row, at>... arguments)
      -> ReturnCppType<row> {
    const std::array<Word, sizeof...(at)> words{
        Word{std::in_place_type<ArgumentCppType<row, at>>, arguments}...};
    if constexpr (std::is_void_v<ReturnCppType<row>>) {
      CarryOut(kOperationAt<row>, words);
    } else {
      const value::MachineAnswer answer = CarryOut(kOperationAt<row>, words);
      const auto* machine = std::get_if<ReturnCppType<row>>(&answer);
      if (machine == nullptr) {
        throw InternalError(
            "an integral operation answering a machine value answered none");
      }
      return *machine;
    }
  }
};

}  // namespace

// One code address per operation, in the order the operations are enumerated
// in, laid out as an array of code addresses is. Each is held at its own type,
// which is what lets the table be fixed before the program starts: a code
// address read as another type is no constant.
template <typename... Signature>
struct IntegralEntryTable;

template <>
struct IntegralEntryTable<> {};

template <typename First, typename... Rest>
struct IntegralEntryTable<First, Rest...> {
  constexpr explicit IntegralEntryTable(
      First* first_entry, Rest*... rest_entries)
      : first(first_entry), rest(rest_entries...) {
  }

  First* first;
  [[no_unique_address]] IntegralEntryTable<Rest...> rest;
};

// The table holding the entry of the operation at each of `rows`.
template <typename Rows>
struct IntegralEntryTableOver;

template <std::size_t... row>
struct IntegralEntryTableOver<std::index_sequence<row...>> {
  using Table = IntegralEntryTable<decltype(Entry<row>::Call)...>;

  static constexpr auto Make() -> Table {
    return Table(&Entry<row>::Call...);
  }
};

using IntegralEntryTableOfEveryOperation = IntegralEntryTableOver<
    std::make_index_sequence<support::IntegralOperations().size()>>;
using IntegralEntries = IntegralEntryTableOfEveryOperation::Table;
static_assert(
    sizeof(IntegralEntries) ==
    support::IntegralOperations().size() * sizeof(void (*)()));

}  // namespace lyra::runtime

extern "C" {

extern constinit const lyra::runtime::IntegralEntries
    lyra_rt_integral_operations;
constinit const lyra::runtime::IntegralEntries lyra_rt_integral_operations =
    lyra::runtime::IntegralEntryTableOfEveryOperation::Make();
}
