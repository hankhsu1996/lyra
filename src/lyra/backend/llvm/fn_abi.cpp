#include "lyra/backend/llvm/fn_abi.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <format>
#include <iterator>
#include <optional>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/llvm/entry_realizations.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/backend/llvm/type_realization.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::backend::llvm_backend {

namespace {

// The type a call states it is made at, which an entry told something of its
// answer reads.
auto ToldOfType(std::optional<lir::TypeId> stated, std::string_view what)
    -> lir::TypeId {
  if (!stated.has_value()) {
    throw InternalError(
        std::format(
            "llvm codegen: an entry is told {} by a call that states none -- "
            "please report this as a bug",
            what));
  }
  return *stated;
}

// What an entry building an array of `built` is told of it, which no operand
// states: a fixed-size unpacked array's type, a queue's whole type, or an
// element type led by an associative array's index order.
auto BuiltArrayTypeArgs(const lir::TypePool& types, lir::TypeId built)
    -> std::vector<TypeArg> {
  const lir::Type& array = types.Get(built);
  if (array.Is<lir::UnpackedArrayType>()) {
    return {TypeConstantArg{.type = built}};
  }
  // A queue is told its whole type, whose bound is part of it (LRM 7.10).
  if (array.IsQueue()) {
    return {QueueTypeArg{.type = built}};
  }
  const std::optional<lir::TypeId> element = array.ContainerElementType();
  if (!element.has_value()) {
    throw InternalError(
        "llvm codegen: an entry told of the array it builds builds no array "
        "-- please report this as a bug");
  }
  // An associative array holds its entries in the order its index type
  // imposes (LRM 7.8), which it is built holding.
  if (const std::optional<lir::TypeId> key = array.IndexType()) {
    return {IndexOrderArg{.key = *key}, ElementTypeArg{.type = *element}};
  }
  return {ElementTypeArg{.type = *element}};
}

}  // namespace

auto WrapperOf(const lir::Type& type)
    -> std::optional<std::pair<WrapperKind, lir::TypeId>> {
  if (const auto* observable = type.As<lir::ObservableType>()) {
    return std::pair{WrapperKind::kCell, observable->value};
  }
  if (const auto* net = type.As<lir::ResolvedType>()) {
    return std::pair{WrapperKind::kNet, net->value};
  }
  if (const auto* driver = type.As<lir::DriverType>()) {
    return std::pair{WrapperKind::kDriver, driver->value};
  }
  // A reference is a wrapper rather than a way of reaching one: it is an
  // address whose storage may be subscribable or not, and only the operation
  // performed through it can tell which (LRM 13.5.2). So it is classified
  // where it stands, before anything strips it to look at what it points at.
  if (const auto* reference = type.As<lir::VariableRefType>()) {
    return std::pair{WrapperKind::kRef, reference->pointee};
  }
  return std::nullopt;
}

auto ValuesHeldBy(const lir::Type& type) -> std::optional<lir::TypeId> {
  if (const std::optional<std::pair<WrapperKind, lir::TypeId>> wrapper =
          WrapperOf(type)) {
    return wrapper->second;
  }
  // A type the library declares holds the values its declaration says.
  if (const auto* library = type.As<lir::LibraryType>()) {
    return library->FindValuesHeld();
  }
  if (const auto* designation = type.As<lir::DesignationType>()) {
    return designation->value;
  }
  return std::nullopt;
}

void RequireReadAs(
    const lir::TypePool& types, support::OperandReading reading,
    lir::TypeId operand) {
  const auto* integral = types.Get(operand).As<lir::IntegralType>();
  switch (reading) {
    // An ordinal the layer above stated crosses as the bytes of a position, so
    // one that reaches here in any other type was never brought to it.
    case support::OperandReading::kPosition:
      if (integral == nullptr ||
          ShapeOf(*integral) != value::kShapeOf<value::Position>) {
        throw InternalError(
            "llvm codegen: an entry reads an ordinal that is not stated as a "
            "position -- please report this as a bug");
      }
      break;
    case support::OperandReading::kMachine:
      if (integral != nullptr) {
        throw InternalError(
            "llvm codegen: an entry is handed an integral value its "
            "declaration states no reading of -- please report this as a bug");
      }
      break;
    case support::OperandReading::kHeld:
    case support::OperandReading::kBits:
    case support::OperandReading::kNumber:
    case support::OperandReading::kTyped:
    case support::OperandReading::kKey:
      break;
  }
}

auto PassModeOf(support::OperandReading reading, lir::TypeId operand)
    -> PassMode {
  switch (reading) {
    case support::OperandReading::kHeld:
    case support::OperandReading::kPosition:
    case support::OperandReading::kMachine:
      return PassDirect{};
    case support::OperandReading::kBits:
      return PassWithExtent{.type = operand};
    case support::OperandReading::kNumber:
      return PassWithShape{.type = operand};
    case support::OperandReading::kTyped:
    case support::OperandReading::kKey:
      return PassWithType{.type = operand};
  }
  throw InternalError("llvm codegen: unknown operand reading");
}

auto CrossingModeOf(
    const lir::TypePool& types, support::OperandReading reading,
    lir::TypeId operand) -> PassMode {
  switch (reading) {
    case support::OperandReading::kBits:
    case support::OperandReading::kNumber:
      if (types.Get(operand).Is<lir::PointerType>()) {
        return PassModeOf(reading, *types.Get(operand).Pointee());
      }
      return PassModeOf(reading, operand);
    case support::OperandReading::kHeld:
    case support::OperandReading::kPosition:
    case support::OperandReading::kMachine:
    case support::OperandReading::kTyped:
    case support::OperandReading::kKey:
      return PassModeOf(reading, operand);
  }
  throw InternalError("llvm codegen: unknown operand reading");
}

namespace {

// How operand `index`, of type `own` and read as `reading`, crosses where what
// `realization` is told concerns it -- with what it is told, or as it; nothing
// where the realization is told nothing of that operand, or where the library
// realizes the entry once, which then crosses as its reading says.
auto ToldModeOf(
    const std::optional<Realization>& realization, std::size_t index,
    support::OperandReading reading, lir::TypeId own, const lir::TypePool& pool)
    -> std::optional<PassMode> {
  using Mode = std::optional<PassMode>;
  if (!realization.has_value()) {
    return std::nullopt;
  }
  const lir::TypeId of = realization->of;
  return std::visit(
      Overloaded{
          [](const ToldNothing&) -> Mode { return std::nullopt; },
          [](const ToldWidthMetadataAhead&) -> Mode { return std::nullopt; },
          [](const ToldTypeMetadataAhead&) -> Mode { return std::nullopt; },
          [](const ToldContainerTypeAhead&) -> Mode { return std::nullopt; },
          [](const ToldTypeAndNetFoldsAhead&) -> Mode { return std::nullopt; },
          [](const ToldTakePositions&) -> Mode { return std::nullopt; },
          [](const ToldOperations&) -> Mode { return std::nullopt; },
          [&](const ToldWidthMetadata& t) -> Mode {
            if (t.after != index) {
              return std::nullopt;
            }
            return PassWithWidth{.type = of};
          },
          [&](const ToldLaidOutType&) -> Mode {
            if (!lir::IsLaidOutByItsType(pool.Get(own))) {
              return std::nullopt;
            }
            // A value handed over with nothing of its type is one of the type
            // the realization is told, or the realization could not read it;
            // any other reading already says what crosses with the operand.
            switch (reading) {
              case support::OperandReading::kHeld:
              case support::OperandReading::kMachine:
                if (own != of) {
                  throw InternalError(
                      "llvm codegen: a realization for values laid out by "
                      "one type is handed a value laid out by another, which "
                      "its declaration does not read with its type -- please "
                      "report this as a bug");
                }
                return std::nullopt;
              case support::OperandReading::kBits:
              case support::OperandReading::kNumber:
              case support::OperandReading::kPosition:
              case support::OperandReading::kTyped:
              case support::OperandReading::kKey:
                return std::nullopt;
            }
            throw InternalError("llvm codegen: unknown operand reading");
          },
          [&](const ToldKeyType& t) -> Mode {
            if (t.after != index) {
              return std::nullopt;
            }
            const std::optional<lir::TypeId> key = pool.Get(own).IndexType();
            if (!key.has_value()) {
              throw InternalError(
                  "llvm codegen: a realization told the type a memory's keys "
                  "are built at is handed no associative array -- please "
                  "report this as a bug");
            }
            return PassWithType{.type = *key};
          },
          [&](const ToldEntryType& t) -> Mode {
            if (t.after != index) {
              return std::nullopt;
            }
            const auto* entries = pool.Get(own).As<lir::MachineArrayType>();
            if (entries == nullptr) {
              throw InternalError(
                  "llvm codegen: a realization told the type of a literal's "
                  "entries is handed no sequence of them -- please report "
                  "this as a bug");
            }
            return PassWithType{.type = entries->element};
          }},
      realization->told);
}

// What a realization for values laid out by their type is told after its
// operands: that type, once, however many values of it it takes or builds, how
// the type takes some of a value's positions, or the type's operations.
auto TypeArgsOfTheRealization(const std::optional<Realization>& realization)
    -> std::vector<TypeArg> {
  if (!realization.has_value()) {
    return {};
  }
  using Args = std::vector<TypeArg>;
  const lir::TypeId of = realization->of;
  return std::visit(
      Overloaded{
          [&](const ToldLaidOutType&) -> Args {
            return {TypeConstantArg{.type = of}};
          },
          [&](const ToldTakePositions&) -> Args {
            return {TakePositionsArg{.type = of}};
          },
          [&](const ToldOperations&) -> Args {
            return {OperationsArg{.type = of}};
          },
          [](const ToldNothing&) -> Args { return {}; },
          [](const ToldWidthMetadata&) -> Args { return {}; },
          [](const ToldWidthMetadataAhead&) -> Args { return {}; },
          [](const ToldTypeMetadataAhead&) -> Args { return {}; },
          [](const ToldContainerTypeAhead&) -> Args { return {}; },
          [](const ToldTypeAndNetFoldsAhead&) -> Args { return {}; },
          [](const ToldKeyType&) -> Args { return {}; },
          [](const ToldEntryType&) -> Args { return {}; }},
      realization->told);
}

// What an entry told `told` of its answer is handed after its operands, where
// that is read from the type `called_at` it is called at.
auto TypeArgsCalledAt(
    support::AnswerTold told, std::optional<lir::TypeId> called_at)
    -> std::vector<TypeArg> {
  switch (told) {
    case support::AnswerTold::kNothing:
      return {};
    case support::AnswerTold::kElementType:
      return {ElementTypeArg{
          .type = ToldOfType(
              called_at, "the element type of the element it answers with")}};
    case support::AnswerTold::kIntegralExtent:
      return {TypeExtentArg{
          .type = ToldOfType(called_at, "how wide the value it answers is")}};
    case support::AnswerTold::kTypeCalledAt:
      return {TypeConstantArg{
          .type = ToldOfType(called_at, "the type it is called at")}};
    // The array an entry builds, and the value it answers, are what the call
    // answers with, which a call stating only the type it is made at does not
    // name.
    case support::AnswerTold::kBuiltArray:
      throw InternalError(
          "llvm codegen: an entry told of the array it builds is called where "
          "no answer is stated -- please report this as a bug");
    case support::AnswerTold::kAnswerType:
      throw InternalError(
          "llvm codegen: an entry told the type it answers is called where no "
          "answer is stated -- please report this as a bug");
    case support::AnswerTold::kAnswerTypeAndOperations:
      throw InternalError(
          "llvm codegen: an entry told the type it answers and its operations "
          "is called where no answer is stated -- please report this as a "
          "bug");
  }
  throw InternalError("llvm codegen: unknown answer told");
}

// The same for a call answering a value of `answered`, out of which the type
// of an array the entry builds is read -- the array's own where it is a
// fixed-size unpacked array or a queue, and otherwise its element type -- and
// the type of the value it answers is read too.
auto TypeArgsToldAs(
    const lir::TypePool& types, support::AnswerTold told,
    std::optional<lir::TypeId> called_at, lir::TypeId answered)
    -> std::vector<TypeArg> {
  switch (told) {
    case support::AnswerTold::kAnswerType:
      return {TypeConstantArg{.type = answered}};
    case support::AnswerTold::kAnswerTypeAndOperations:
      return {
          ElementTypeArg{.type = answered}, OperationsArg{.type = answered}};
    case support::AnswerTold::kBuiltArray:
      return BuiltArrayTypeArgs(types, answered);
    case support::AnswerTold::kNothing:
    case support::AnswerTold::kElementType:
    case support::AnswerTold::kIntegralExtent:
    case support::AnswerTold::kTypeCalledAt:
      return TypeArgsCalledAt(told, called_at);
  }
  throw InternalError("llvm codegen: unknown answer told");
}

// What a call through `realization`, none where the library realizes the
// entry once, hands it ahead of its operands.
auto ImplicitArgOf(
    const lir::TypePool& types, const std::optional<Realization>& realization)
    -> ImplicitArg {
  if (!realization.has_value()) {
    return NoImplicitArg{};
  }
  return std::visit(
      Overloaded{
          [&](const ToldWidthMetadataAhead&) -> ImplicitArg {
            return WidthMetadataArg{.of = realization->of};
          },
          [&](const ToldTypeMetadataAhead&) -> ImplicitArg {
            return TypeMetadataArg{
                .args = {ElementTypeArg{.type = realization->of}}};
          },
          [&](const ToldContainerTypeAhead&) -> ImplicitArg {
            return TypeMetadataArg{
                .args = BuiltArrayTypeArgs(types, realization->of)};
          },
          [&](const ToldTypeAndNetFoldsAhead&) -> ImplicitArg {
            return TypeAndNetFoldsArg{.of = realization->of};
          },
          [](const ToldNothing&) -> ImplicitArg { return NoImplicitArg{}; },
          [](const ToldWidthMetadata&) -> ImplicitArg {
            return NoImplicitArg{};
          },
          [](const ToldLaidOutType&) -> ImplicitArg { return NoImplicitArg{}; },
          [](const ToldKeyType&) -> ImplicitArg { return NoImplicitArg{}; },
          [](const ToldEntryType&) -> ImplicitArg { return NoImplicitArg{}; },
          [](const ToldTakePositions&) -> ImplicitArg {
            return NoImplicitArg{};
          },
          [](const ToldOperations&) -> ImplicitArg { return NoImplicitArg{}; }},
      realization->told);
}

}  // namespace

auto AnswerTypeArgs(
    const lir::TypePool& types, const std::optional<Realization>& realization,
    support::AnswerTold declared, std::optional<lir::TypeId> called_at,
    lir::TypeId answered, bool answered_in_storage) -> std::vector<TypeArg> {
  switch (declared) {
    case support::AnswerTold::kNothing:
      if (answered_in_storage && lir::IsLaidOutByItsType(types.Get(answered)) &&
          realization.has_value() &&
          lir::IsLaidOutByItsType(types.Get(realization->of)) &&
          answered != realization->of) {
        throw InternalError(
            "llvm codegen: a realization for values laid out by one type "
            "builds a value laid out by another, which its declaration says "
            "it is told nothing of -- please report this as a bug");
      }
      return {};
    case support::AnswerTold::kBuiltArray:
    case support::AnswerTold::kAnswerType:
    case support::AnswerTold::kAnswerTypeAndOperations:
    case support::AnswerTold::kElementType:
    case support::AnswerTold::kIntegralExtent:
    case support::AnswerTold::kTypeCalledAt:
      return TypeArgsToldAs(types, declared, called_at, answered);
  }
  throw InternalError("llvm codegen: unknown answer told");
}

auto ArrangeCall(
    const lir::TypePool& types, const support::OperandReadings& readings,
    std::span<const StatedOperand> operands,
    const std::optional<Realization>& realization,
    std::optional<NamedPartArg> named_part,
    std::vector<TypeArg> told_of_the_answer) -> FnAbi {
  std::vector<ArgAbi> args;
  args.reserve(operands.size());
  for (std::size_t i = 0; i < operands.size(); ++i) {
    const support::OperandReading reading = readings.At(i);
    args.push_back(
        ArgAbi{
            .operand = operands[i],
            .mode = std::visit(
                Overloaded{
                    [&](const ValueOperand& value) -> PassMode {
                      RequireReadAs(types, reading, value.type);
                      return ToldModeOf(
                                 realization, i, reading, value.type, types)
                          .value_or(CrossingModeOf(types, reading, value.type));
                    },
                    // A machine value is of no type whose facts could cross
                    // with it, so it is only ever read as the machine value it
                    // is.
                    [&](const MachineOperand&) -> PassMode {
                      switch (reading) {
                        case support::OperandReading::kMachine:
                          return PassDirect{};
                        case support::OperandReading::kHeld:
                        case support::OperandReading::kPosition:
                        case support::OperandReading::kBits:
                        case support::OperandReading::kNumber:
                        case support::OperandReading::kTyped:
                        case support::OperandReading::kKey:
                          break;
                      }
                      throw InternalError(
                          "llvm codegen: an entry reads a value of the design "
                          "where it is handed a machine value -- please report "
                          "this as a bug");
                    }},
                operands[i])});
  }
  std::vector<TypeArg> type_args = TypeArgsOfTheRealization(realization);
  std::ranges::move(told_of_the_answer, std::back_inserter(type_args));
  return FnAbi{
      .implicit_arg = ImplicitArgOf(types, realization),
      .named_part = named_part,
      .args = std::move(args),
      .type_args = std::move(type_args)};
}

auto ArrangeEntryCall(
    const lir::TypePool& types, RuntimeOp op,
    std::span<const StatedOperand> operands,
    const std::optional<Realization>& realization,
    std::optional<lir::TypeId> called_at) -> FnAbi {
  return ArrangeCall(
      types, OperandReadingsOf(op), operands, realization, std::nullopt,
      TypeArgsCalledAt(AnswerToldOf(op), called_at));
}

auto ValuesHeldByMember(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<lir::TypeId> {
  if (unit.types.Get(type).IsHeldInStorage()) {
    return type;
  }
  return ValuesHeldBy(unit.types.Get(type));
}

auto ArrangeMemberBuild(
    const lir::CompilationUnit& unit, lir::TypeId type,
    support::DeclaredMemberStorage storage) -> FnAbi {
  const std::array<StatedOperand, 1> stated{
      MachineOperand{.kind = MachineOperandKind::kAddress}};
  return ArrangeCall(
      unit.types, support::OperandReadings{support::OperandReading::kMachine},
      stated, ValuesHeldByMember(unit, type).transform([&](lir::TypeId held) {
        return Realization{.told = ToldBuilding(storage), .of = held};
      }),
      std::nullopt, {});
}

auto VariableStorageOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<support::DeclaredMemberStorage> {
  return DeclaredStorageOf(unit, type, MemberSlotRole::kVariable);
}

auto ArrangeVariableBuild(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<FnAbi> {
  return VariableStorageOf(unit, type)
      .transform([&](const support::DeclaredMemberStorage& storage) {
        return ArrangeMemberBuild(unit, type, storage);
      });
}

auto ArrangeLibraryStep(
    const lir::TypePool& types, lir::TypeId type,
    const support::RuntimeObject& object, RuntimeOp op, std::size_t operands)
    -> FnAbi {
  const std::optional<Realization> realization = std::visit(
      Overloaded{
          [&](support::ValueDomain domain) -> std::optional<Realization> {
            return Realization{.told = ToldOf(domain, op), .of = type};
          },
          [](support::LibraryDecl) -> std::optional<Realization> {
            return std::nullopt;
          },
          [](support::LibraryObject) -> std::optional<Realization> {
            return std::nullopt;
          }},
      object);
  const std::vector<StatedOperand> stated(
      operands, StatedOperand{ValueOperand{.type = type}});
  return ArrangeEntryCall(types, op, stated, realization, std::nullopt);
}

auto RealizedDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> diag::Result<support::ValueDomain> {
  const std::optional<support::ValueDomain> domain = ValueDomainOf(unit, type);
  if (!domain) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedTypeKind,
        "a value of type '{}' is not yet supported with '--backend llvm'",
        unit.types.Get(type).KindName());
  }
  return *domain;
}

}  // namespace lyra::backend::llvm_backend
