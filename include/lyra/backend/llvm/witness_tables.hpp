#pragma once

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
#include "lyra/support/machine_signature.hpp"
#include "lyra/support/value_operation.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::backend::llvm_backend {

// How an operation's body takes and answers its integral values. The type the
// library asks through answers an equality as its scalar and a case equality
// as whether it holds, writes a stream into an integral value it is handed
// and reads one out of one, and is handed a count's control bits as one too,
// while a type's own function takes and answers each as a value of
// the type its own code states -- a stream a run of bits, where the type's is
// as long as the value makes it.
struct TakenAsStated {};
struct AnswerScalar {};
struct AnswerWhetherItHolds {};
struct AnswerIntoStream {};
struct StreamOutOfValue {};
struct RunIntoStream {};
struct StreamOutOfRun {};
struct ControlOutOfValue {};
using IntegralCrossing = std::variant<
    TakenAsStated, AnswerScalar, AnswerWhetherItHolds, AnswerIntoStream,
    StreamOutOfValue, RunIntoStream, StreamOutOfRun, ControlOutOfValue>;

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
    std::variant<lir::ValueStep, OwnOperationWitness, EitherStreamForm>;

// One witness of a table a type is told to the library through: the storage's
// own lifecycle, the operations the language defines on the whole value (LRM
// 11.4.5, 20.6.2, 20.9, 6.24.3), how a net folds values of it (LRM 6.6.1,
// 28.12.1), which a record's declaration states as its methods and a unit
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
      WitnessOf("copy", &ValueWitnessTable::copy, lir::ValueStep::kCopy),
      WitnessOf("move", &ValueWitnessTable::move, lir::ValueStep::kMove),
      WitnessOf("destroy", &ValueWitnessTable::destroy, lir::ValueStep::kEnd),
      WitnessOf("assign", &ValueWitnessTable::assign, lir::ValueStep::kAssign),
      WitnessOf(
          "bit_identical", &ValueWitnessTable::bit_identical,
          OwnOperationWitness{.answers = BuiltinFn::kBitIdentical}),
  });
}

// The table is its witnesses, then its parts, one address each.
static_assert(
    sizeof(value::ValueWitnessTable) ==
    (std::tuple_size_v<decltype(ValueWitnesses())> + 1) * sizeof(void*));

// Every function of the language's operations on a whole value of a type.
inline auto OperationWitnesses() {
  using support::BuiltinFn;
  using value::OperationFunctions;
  return std::to_array<Witness>({
      WitnessOf(
          "equal", &OperationFunctions::equal,
          OwnOperationWitness{
              .answers = support::ValueOperator::kEquality,
              .crossing = AnswerScalar{}}),
      WitnessOf(
          "case_equal", &OperationFunctions::case_equal,
          OwnOperationWitness{
              .answers = BuiltinFn::kCaseEqual,
              .crossing = AnswerWhetherItHolds{}}),
      WitnessOf(
          "has_unknown", &OperationFunctions::has_unknown,
          OwnOperationWitness{.answers = BuiltinFn::kHasUnknown}),
      WitnessOf(
          "bitstream_width", &OperationFunctions::bitstream_width,
          OwnOperationWitness{.answers = BuiltinFn::kBitstreamWidth}),
      WitnessOf(
          "count_bits", &OperationFunctions::count_bits,
          OwnOperationWitness{
              .answers = BuiltinFn::kCountBits,
              .crossing = ControlOutOfValue{}}),
      WitnessOf(
          "write_to_stream", &OperationFunctions::write_to_stream,
          EitherStreamForm{
              .fixed =
                  OwnOperationWitness{
                      .answers = BuiltinFn::kToBitstream,
                      .crossing = AnswerIntoStream{}},
              .run =
                  OwnOperationWitness{
                      .answers = BuiltinFn::kToBitVec,
                      .crossing = RunIntoStream{}}}),
      WitnessOf(
          "read_from_stream", &OperationFunctions::read_from_stream,
          EitherStreamForm{
              .fixed =
                  OwnOperationWitness{
                      .answers = BuiltinFn::kFromBitstream,
                      .crossing = StreamOutOfValue{}},
              .run =
                  OwnOperationWitness{
                      .answers = BuiltinFn::kFromBitVec,
                      .crossing = StreamOutOfRun{}}}),
  });
}

static_assert(
    sizeof(value::OperationFunctions) ==
    std::tuple_size_v<decltype(OperationWitnesses())> * sizeof(void*));

// Every witness of how a net folds values of a type, each the function of the
// type's own answering that fold.
inline auto NetFoldWitnesses() {
  using support::BuiltinFn;
  using value::NetFoldTable;
  return std::to_array<Witness>({
      WitnessOf(
          "resolve_tri_state", &NetFoldTable::tri_state,
          OwnOperationWitness{.answers = BuiltinFn::kResolveTriState}),
      WitnessOf(
          "resolve_wired_and", &NetFoldTable::wired_and,
          OwnOperationWitness{.answers = BuiltinFn::kResolveWiredAnd}),
      WitnessOf(
          "resolve_wired_or", &NetFoldTable::wired_or,
          OwnOperationWitness{.answers = BuiltinFn::kResolveWiredOr}),
      WitnessOf(
          "dominating", &NetFoldTable::dominating,
          OwnOperationWitness{.answers = BuiltinFn::kDominating}),
      WitnessOf(
          "filled_like", &NetFoldTable::filled_like,
          OwnOperationWitness{.answers = BuiltinFn::kFilledLike}),
  });
}

static_assert(
    sizeof(value::NetFoldTable) ==
    std::tuple_size_v<decltype(NetFoldWitnesses())> * sizeof(void*));

// What a value's type constant names of other types, the one list the constant
// and the query for what a unit needs both read: an array how its elements
// start (LRM 7.4.5), and a product the type of each component.
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
  return {
      .element_type = std::nullopt,
      .components = lir::ProductElements(unit, type)
                        .value_or(std::span<const lir::TypeId>{})};
}

}  // namespace lyra::backend::llvm_backend
