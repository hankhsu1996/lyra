#pragma once

#include <algorithm>
#include <array>
#include <bit>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <tuple>
#include <variant>

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_trait.hpp"
#include "lyra/support/machine_signature.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::backend::llvm_backend {

// How an operation's body takes and answers its integral values. The type the
// library asks through answers an equality as its scalar and a case equality
// as whether it holds, writes a stream into an integral value it is handed and
// reads one out of one, and writes its positions into a view a net hands it,
// while a type's own function takes and answers each as a value of the type
// its own code states -- a stream a run of bits, where the type's is as long as
// the value makes it.
struct TakenAsStated {};
struct AnswerScalar {};
struct AnswerWhetherItHolds {};
struct AnswerIntoStream {};
struct AnswerIntoView {};
struct StreamOutOfValue {};
struct RunIntoStream {};
struct StreamOutOfRun {};
using IntegralCrossing = std::variant<
    TakenAsStated, AnswerScalar, AnswerWhetherItHolds, AnswerIntoStream,
    AnswerIntoView, StreamOutOfValue, RunIntoStream, StreamOutOfRun>;

// An operation a type answers with a function of its own, and how that
// function takes and answers its integral values.
struct OwnOperationWitness {
  support::ValueOperation answers;
  IntegralCrossing crossing = TakenAsStated{};
};

// A stream a type writes or reads by whichever of two functions of its own it
// states: one at the width its type fixes, or one whose stream is a run of
// bits as long as the value makes it (LRM 6.24.3).
struct EitherStreamForm {
  OwnOperationWitness fixed;
  OwnOperationWitness run;
};

using WitnessFilling =
    std::variant<support::ValueStep, OwnOperationWitness, EitherStreamForm>;

// One witness of a table a type is told to the library through: the storage's
// own lifecycle, the operations the language defines on the whole value (LRM
// 11.4.5, 20.6.2, 20.9, 6.24.3), what a net of it asks of a value's positions
// (LRM 6.6.7), which a record's declaration states as its methods and a unit
// states for an array, or one no such type answers. `offset` is where the table
// lays it out, and a body filling it takes and answers what its declaration
// does.
struct Witness {
  std::string_view name;
  std::size_t offset;
  support::MachineSignature stands_in_for;
  WitnessFilling filled_by;
};

// What a function of the table takes after the type and what it answers,
// read off its declaration. One that does not throw is read the same way.
template <typename Function>
struct WitnessSignatureOf;
template <typename R, typename... Params>
struct WitnessSignatureOf<R (*)(const value::ValueType&, Params...)> {
  static auto Get() -> support::MachineSignature {
    return support::MachineSignatureOf<R(Params...)>::Get();
  }
};
template <typename R, typename... Params>
struct WitnessSignatureOf<R (*)(const value::ValueType&, Params...) noexcept>
    : WitnessSignatureOf<R (*)(const value::ValueType&, Params...)> {};
// An operation's function is handed the operations it belongs to first.
template <typename R, typename... Params>
struct WitnessSignatureOf<R (*)(const value::ValueOperations&, Params...)> {
  static auto Get() -> support::MachineSignature {
    return support::MachineSignatureOf<R(Params...)>::Get();
  }
};

// Where a table's declaration lays out its member `member`, read off one
// table laid out as it is.
template <typename Table, typename Field>
auto OffsetInTable(Field Table::* member) -> std::size_t {
  const Table table{};
  return static_cast<std::size_t>(
      std::bit_cast<std::uintptr_t>(&(table.*member)) -
      std::bit_cast<std::uintptr_t>(&table));
}

// The witness a table's member `member` holds: where the declaration lays it
// out, and what a body filling it takes and answers.
template <typename Table, typename Function>
auto WitnessOf(
    std::string_view name, Function Table::* member, WitnessFilling filled_by)
    -> Witness {
  return {
      .name = name,
      .offset = OffsetInTable(member),
      .stands_in_for = WitnessSignatureOf<Function>::Get(),
      .filled_by = filled_by};
}

// Every witness of the type's table but its parts, which no body fills.
inline auto ValueWitnesses() {
  using support::BuiltinFn;
  using value::ValueWitnessTable;
  return std::to_array<Witness>({
      WitnessOf("copy", &ValueWitnessTable::copy, support::ValueStep::kCopy),
      WitnessOf(
          "relocate", &ValueWitnessTable::relocate,
          support::ValueStep::kRelocate),
      WitnessOf(
          "destroy", &ValueWitnessTable::destroy, support::ValueStep::kEnd),
      WitnessOf(
          "assign", &ValueWitnessTable::assign, support::ValueStep::kAssign),
      WitnessOf(
          "bit_identical", &ValueWitnessTable::bit_identical,
          OwnOperationWitness{
              .answers = support::ChangeTestItem::kBitIdentical}),
  });
}

// The table is its witnesses, then its parts, one address each.
static_assert(
    sizeof(value::ValueWitnessTable) ==
    (std::tuple_size_v<decltype(ValueWitnesses())> + 1) * sizeof(void*));

// Whether `type` implements the library's trait `trait`, as the unit
// states its implementation.
inline auto ImplementsTrait(
    const lir::CompilationUnit& unit, lir::TypeId type,
    const support::LibraryTrait& trait) -> bool {
  const auto found = unit.implemented_traits.find(type);
  return found != unit.implemented_traits.end() &&
         std::ranges::contains(found->second, &trait);
}

// Every function of the language's operations on a whole value of a type.
inline auto OperationWitnesses() {
  using support::StreamItem;
  using value::OperationFunctions;
  return std::to_array<Witness>({
      WitnessOf(
          "equal", &OperationFunctions::equal,
          OwnOperationWitness{
              .answers = support::EqualityItem::kEqual,
              .crossing = AnswerScalar{}}),
      WitnessOf(
          "case_equal", &OperationFunctions::case_equal,
          OwnOperationWitness{
              .answers = support::EqualityItem::kCaseEqual,
              .crossing = AnswerWhetherItHolds{}}),
      WitnessOf(
          "has_unknown", &OperationFunctions::has_unknown,
          OwnOperationWitness{.answers = support::UnknownItem::kHasUnknown}),
      WitnessOf(
          "bitstream_width", &OperationFunctions::bitstream_width,
          OwnOperationWitness{.answers = StreamItem::kBitstreamWidth}),
      WitnessOf(
          "count_bits", &OperationFunctions::count_bits,
          OwnOperationWitness{.answers = StreamItem::kCountBits}),
      WitnessOf(
          "write_to_stream", &OperationFunctions::write_to_stream,
          EitherStreamForm{
              .fixed =
                  OwnOperationWitness{
                      .answers = StreamItem::kToBitstream,
                      .crossing = AnswerIntoStream{}},
              .run =
                  OwnOperationWitness{
                      .answers = StreamItem::kToBitVec,
                      .crossing = RunIntoStream{}}}),
      WitnessOf(
          "read_from_stream", &OperationFunctions::read_from_stream,
          EitherStreamForm{
              .fixed =
                  OwnOperationWitness{
                      .answers = StreamItem::kFromBitstream,
                      .crossing = StreamOutOfValue{}},
              .run =
                  OwnOperationWitness{
                      .answers = StreamItem::kFromBitVec,
                      .crossing = StreamOutOfRun{}}}),
  });
}

static_assert(
    sizeof(value::OperationFunctions) ==
    std::tuple_size_v<decltype(OperationWitnesses())> * sizeof(void*));

// Every witness of what a net of a type asks of a value of it -- its positions
// read out whole, how many it has, and some of them written in -- each the
// function of the type's own answering it.
inline auto NetWitnesses() {
  using support::PositionsItem;
  using value::NetWitnessTable;
  return std::to_array<Witness>({
      WitnessOf(
          "to_positions", &NetWitnessTable::to_positions,
          OwnOperationWitness{
              .answers = PositionsItem::kToPositions,
              .crossing = AnswerIntoView{}}),
      WitnessOf(
          "position_count", &NetWitnessTable::position_count,
          OwnOperationWitness{.answers = PositionsItem::kPositionCount}),
      WitnessOf(
          "write_positions", &NetWitnessTable::write_positions,
          OwnOperationWitness{.answers = PositionsItem::kWritePositions}),
  });
}

static_assert(
    sizeof(value::NetWitnessTable) ==
    std::tuple_size_v<decltype(NetWitnesses())> * sizeof(void*));

// What a value's type constant names of other types, the one list the constant
// and the query for what a unit needs both read: an array how its elements
// start (LRM 7.4.5), a dynamic array the same of its elements, and a product
// the type of each component.
struct TypeConstantReferences {
  std::optional<lir::TypeId> element_type;
  std::span<const lir::TypeId> components;
};

inline auto ReferencesOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> TypeConstantReferences {
  const lir::Type& stated = unit.types.Get(type);
  if (const auto* array = stated.As<lir::UnpackedArrayType>()) {
    return {.element_type = array->element_type, .components = {}};
  }
  if (stated.Is<lir::LibraryType>()) {
    return {.element_type = stated.ContainerElementType(), .components = {}};
  }
  return {
      .element_type = std::nullopt,
      .components = lir::ProductElements(unit, type)
                        .value_or(std::span<const lir::TypeId>{})};
}

}  // namespace lyra::backend::llvm_backend
