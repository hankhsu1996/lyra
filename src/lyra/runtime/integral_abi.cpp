#include "lyra/runtime/integral_abi.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
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

// An integral type as far as an entry is told it. An entry is told the
// signedness only of an operand its operation reads as a number.
struct ToldType {
  value::IntegralExtent extent;
  value::Signedness signedness = value::Signedness::kUnsigned;
};

// One operand as an entry received it, in the member of the machine type the
// operation's declaration has it cross as.
struct HandedOperand {
  const void* address = nullptr;
  std::int64_t number = 0;
  bool predicate = false;
  std::uint8_t scalar = 0;
};

inline constexpr std::size_t kMostOperands = support::IntegralOperands::kMost;

// Everything one call of an entry was handed, each argument where the
// operation's declaration says it belongs.
struct Handed {
  std::array<HandedOperand, kMostOperands> operands{};
  std::array<ToldType, kMostOperands> told{};
  void* return_slot = nullptr;
  ToldType answer_told;
};

void Hand(HandedOperand& operand, const void* address) {
  operand.address = address;
}
void Hand(HandedOperand& operand, std::int64_t number) {
  operand.number = number;
}
void Hand(HandedOperand& operand, bool predicate) {
  operand.predicate = predicate;
}
void Hand(HandedOperand& operand, std::uint8_t scalar) {
  operand.scalar = scalar;
}

[[noreturn]] void ToldAsAnotherMachineType() {
  throw InternalError(
      "an integral operation's entry is told a fact of a type as another "
      "machine type than its declaration gives it");
}

void Tell(ToldType& type, IntegralTypeFact fact, std::int64_t told) {
  switch (fact) {
    case IntegralTypeFact::kWidth:
      type.extent.width = static_cast<std::uint64_t>(told);
      return;
    case IntegralTypeFact::kIsSigned:
    case IntegralTypeFact::kIsFourState:
      break;
  }
  ToldAsAnotherMachineType();
}

void Tell(ToldType& type, IntegralTypeFact fact, bool told) {
  switch (fact) {
    case IntegralTypeFact::kIsSigned:
      type.signedness =
          told ? value::Signedness::kSigned : value::Signedness::kUnsigned;
      return;
    case IntegralTypeFact::kIsFourState:
      type.extent.domain =
          told ? value::StateDomain::kFourState : value::StateDomain::kTwoState;
      return;
    case IntegralTypeFact::kWidth:
      break;
  }
  ToldAsAnotherMachineType();
}

// The operands of one call as the operation's statement reads them.
class EntryOperands {
 public:
  explicit EntryOperands(const Handed& handed) : handed_(&handed) {
  }

  void View(std::size_t at, value::ConstIntegralView view) {
    views_.at(at) = view;
  }

  [[nodiscard]] auto At(std::size_t at) const
      -> const value::ConstIntegralView& {
    return views_.at(at);
  }

  template <typename T>
  [[nodiscard]] auto MachineAt(std::size_t at) const -> T {
    T machine{};
    Receive(handed_->operands.at(at), machine);
    return machine;
  }

 private:
  static void Receive(const HandedOperand& operand, std::int64_t& number) {
    number = operand.number;
  }
  static void Receive(const HandedOperand& operand, bool& predicate) {
    predicate = operand.predicate;
  }
  static void Receive(const HandedOperand& operand, std::uint8_t& scalar) {
    scalar = operand.scalar;
  }
  static void Receive(const HandedOperand& operand, std::string_view& text) {
    text = static_cast<const value::String*>(operand.address)->View();
  }
  static void Receive(
      const HandedOperand& operand, const svBitVecVal*& vector) {
    vector = static_cast<const svBitVecVal*>(operand.address);
  }
  static void Receive(
      const HandedOperand& operand, const svLogicVecVal*& vector) {
    vector = static_cast<const svLogicVecVal*>(operand.address);
  }

  const Handed* handed_;
  std::array<value::ConstIntegralView, kMostOperands> views_{};
};

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

template <std::size_t row, std::size_t at>
inline constexpr IntegralEntryArgument kArgumentAt =
    kArgumentsAt<row>.All()[at];

// Puts argument `at` of the entry at `row` where the operation's declaration
// says it belongs.
template <std::size_t row, std::size_t at>
void Take(Handed& handed, ArgumentCppType<row, at> argument) {
  if constexpr (constexpr const auto* operand =
                    std::get_if<OperandArgument>(&kArgumentAt<row, at>);
                operand != nullptr) {
    Hand(handed.operands.at(operand->operand), argument);
  } else if constexpr (constexpr const auto* operand_type =
                           std::get_if<OperandTypeArgument>(
                               &kArgumentAt<row, at>);
                       operand_type != nullptr) {
    Tell(handed.told.at(operand_type->operand), operand_type->fact, argument);
  } else if constexpr (constexpr const auto* answer_type =
                           std::get_if<AnswerTypeArgument>(
                               &kArgumentAt<row, at>);
                       answer_type != nullptr) {
    Tell(handed.answer_told, answer_type->fact, argument);
  } else {
    static_assert(
        std::holds_alternative<AnswerStorageArgument>(kArgumentAt<row, at>));
    handed.return_slot = argument;
  }
}

// What laying the integral operands of one call out leaves: the words each
// one's bytes hold, and their extents in operand order.
struct LoadedOperands {
  std::array<std::optional<value::LoadedWords>, kMostOperands> words{};
  std::array<value::IntegralExtent, kMostOperands> extents{};
  std::size_t count = 0;
};

// Loads operand `at`, of the kind `kind`, where it is an integral value, at the
// type the entry was told for it.
void Load(
    IntegralOperandKind kind, const Handed& handed, std::size_t at,
    LoadedOperands& loaded, EntryOperands& operands) {
  const auto load = [&](const ToldType& type) {
    loaded.extents.at(loaded.count++) = type.extent;
    loaded.words.at(at) = value::LoadedWords::Load(
        handed.operands.at(at).address, value::IntegralShape{
                                            .width = type.extent.width,
                                            .signedness = type.signedness,
                                            .domain = type.extent.domain});
    operands.View(at, loaded.words.at(at)->View());
  };
  switch (kind) {
    case IntegralOperandKind::kBits:
    case IntegralOperandKind::kNumber:
      load(handed.told.at(at));
      return;
    case IntegralOperandKind::kSameType:
      load(handed.told.front());
      return;
    case IntegralOperandKind::kMachineInt:
    case IntegralOperandKind::kMachineBool:
    case IntegralOperandKind::kSvLogic:
    case IntegralOperandKind::kText:
    case IntegralOperandKind::kCanonicalBits:
    case IntegralOperandKind::kCanonicalLogic:
      return;
  }
}

// Carries the operation at `row` out over what its entry was handed: loads the
// words the integral operands' bytes hold, applies the operation, and lays an
// integral answer out in the storage given for it.
template <std::size_t row>
auto CarryOut(const Handed& handed) -> value::MachineAnswer {
  const std::span<const IntegralOperandKind> kinds =
      support::IntegralOperations()[row].operands.Kinds();
  LoadedOperands loaded;
  EntryOperands operands(handed);
  for (std::size_t at = 0; at < kinds.size(); ++at) {
    Load(kinds[at], handed, at, loaded, operands);
  }

  // The extent an integral answer is laid out at, or none where the operation
  // answers a machine value.
  using Extent = std::optional<value::IntegralExtent>;
  const Extent extent = std::visit(
      Overloaded{
          [](const value::IntegralExtent& fixed) -> Extent { return fixed; },
          [&](const value::AnswerOfTheCall&) -> Extent {
            return handed.answer_told.extent;
          },
          [](const value::MachineBoolAnswer&) -> Extent {
            return std::nullopt;
          },
          [](const value::MachineIntAnswer&) -> Extent {
            return std::nullopt;
          }},
      value::AnswerTypeOf<kOperationAt<row>>(
          std::span<const value::IntegralExtent>(loaded.extents)
              .first(loaded.count)));
  if (!extent) {
    return value::ApplyIntegralOperation<kOperationAt<row>>(
        operands, value::IntegralView{});
  }
  value::WrittenWords answer = value::WrittenWords::ZeroedAt(
      handed.return_slot,
      value::IntegralShape{.width = extent->width, .domain = extent->domain});
  value::MachineAnswer machine =
      value::ApplyIntegralOperation<kOperationAt<row>>(
          operands, answer.MutableView());
  answer.StoreTo(handed.return_slot);
  return machine;
}

// The entry carrying out the operation at `row`, whose parameters are the
// arguments the operation's declaration gives it, each of the machine type it
// crosses as. Which argument is which, and what each operand is, are settled
// where the library is compiled; a call works out neither.
template <
    std::size_t row,
    typename = std::make_index_sequence<kArgumentsAt<row>.All().size()>>
struct Entry;

template <std::size_t row, std::size_t... at>
struct Entry<row, std::index_sequence<at...>> {
  static auto Call(ArgumentCppType<row, at>... arguments)
      -> ReturnCppType<row> {
    Handed handed;
    (Take<row, at>(handed, arguments), ...);
    if constexpr (std::is_void_v<ReturnCppType<row>>) {
      CarryOut<row>(handed);
    } else {
      const value::MachineAnswer answer = CarryOut<row>(handed);
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
