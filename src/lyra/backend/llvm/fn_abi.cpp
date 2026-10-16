#include "lyra/backend/llvm/fn_abi.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::backend::llvm_backend {

namespace {

auto Unsupported(std::string message) -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm, std::move(message));
}

// The type a realization told of one is told of, which the call entering it
// states.
auto ToldOfType(std::optional<lir::TypeId> stated, std::string_view what)
    -> lir::TypeId {
  if (!stated.has_value()) {
    throw InternalError(
        std::format(
            "llvm codegen: a realization is told {} by a call that states "
            "none -- please report this as a bug",
            what));
  }
  return *stated;
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
  if (const auto* history = type.As<lir::SampledHistoryType>()) {
    return history->value;
  }
  if (const auto* designation = type.As<lir::DesignationType>()) {
    return designation->value;
  }
  return std::nullopt;
}

CallArranger::CallArranger(CodeGenModule& module, const lir::Function& fn)
    : module_(&module), fn_(&fn) {
}

auto CallArranger::OperandType(const lir::Operand& operand) const
    -> lir::TypeId {
  return lir::OperandType(*fn_, operand);
}

auto CallArranger::StorageReached(lir::TypeId operand) const
    -> const lir::Type& {
  const lir::TypePool& types = module_->Unit().types;
  const lir::Type& carried = types.Get(operand);
  // A reference is a handle naming storage someone else owns, so it reaches
  // that storage by being what it is rather than by pointing at it: what it
  // states is the values the storage holds, which is not the storage.
  if (carried.Is<lir::VariableRefType>()) {
    return carried;
  }
  const std::optional<lir::TypeId> pointee = carried.Pointee();
  return types.Get(pointee.value_or(operand));
}

auto CallArranger::StorageDomainBehind(lir::TypeId operand) const
    -> diag::Result<support::ValueDomain> {
  const std::optional<lir::TypeId> held = ValuesHeldBy(StorageReached(operand));
  if (!held.has_value()) {
    throw InternalError(
        "llvm codegen: an entry named by the representation of what a storage "
        "holds needs a storage whose values are all of one");
  }
  return module_->DomainOf(*held);
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

auto ToldModeOf(
    const Told& told, std::size_t index, support::OperandReading reading,
    std::optional<lir::TypeId> acted_on, lir::TypeId own,
    const lir::TypePool& pool) -> std::optional<PassMode> {
  using Mode = std::optional<PassMode>;
  const auto held = [&] {
    return ToldOfType(
        acted_on, "how wide the values the storage it acts on holds are");
  };
  return std::visit(
      Overloaded{
          [](const ToldNothing&) -> Mode { return std::nullopt; },
          [](const ToldWidthMetadataAhead&) -> Mode { return std::nullopt; },
          [](const ToldTypeMetadataAhead&) -> Mode { return std::nullopt; },
          [](const ToldByTheStorage&) -> Mode { return std::nullopt; },
          [&](const ToldWidthMetadata& t) -> Mode {
            if (t.after != index) {
              return std::nullopt;
            }
            return PassWithWidth{.type = held()};
          },
          [&](const ToldLaidOutTypes& t) -> Mode {
            if (t.reached_after == index) {
              return PassWithType{
                  .type = ToldOfType(
                      acted_on, "the type of the value the storage holds")};
            }
            if (!IsLaidOutByType(pool.Get(own))) {
              return std::nullopt;
            }
            // The readings that hand a value over with nothing of its type
            // are the ones a realization for such values adds the type to;
            // any other already says what crosses with the operand.
            switch (reading) {
              case support::OperandReading::kHeld:
              case support::OperandReading::kMachine:
                return PassWithType{.type = own};
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
            const auto* keyed = pool.Get(own).As<lir::AssociativeArrayType>();
            if (keyed == nullptr) {
              throw InternalError(
                  "llvm codegen: a realization told the type a memory's keys "
                  "are built at is handed no associative array -- please "
                  "report this as a bug");
            }
            return PassWithType{.type = keyed->key_type};
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
      told);
}

namespace {

// What `operand` is held to by being read as `reading`.
void RequireReadAs(
    const CodeGenTypes& types, support::OperandReading reading,
    lir::TypeId operand) {
  const auto integral = [&] { return types.IntegralShapeOf(operand); };
  switch (reading) {
    // An ordinal the layer above stated crosses as the bytes of a position, so
    // one that reaches here in any other type was never brought to it.
    case support::OperandReading::kPosition: {
      const std::optional<value::IntegralShape> shape = integral();
      if (!shape.has_value() || *shape != value::kShapeOf<value::Position>) {
        throw InternalError(
            "llvm codegen: an entry reads an ordinal that is not stated as a "
            "position -- please report this as a bug");
      }
      break;
    }
    case support::OperandReading::kMachine:
      if (integral().has_value()) {
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
    case support::AnswerTold::kBuiltArray: {
      const lir::Type& built = types.Get(answered);
      if (built.Is<lir::UnpackedArrayType>()) {
        return {TypeConstantArg{.type = answered}};
      }
      if (built.Is<lir::QueueType>()) {
        return {QueueTypeArg{.type = answered}};
      }
      const std::optional<lir::TypeId> element = built.ContainerElementType();
      if (!element.has_value()) {
        throw InternalError(
            "llvm codegen: an entry told of the array it builds answers with "
            "no array -- please report this as a bug");
      }
      return {ElementTypeArg{.type = *element}};
    }
    case support::AnswerTold::kNothing:
    case support::AnswerTold::kElementType:
    case support::AnswerTold::kIntegralExtent:
      return TypeArgsCalledAt(told, called_at);
  }
  throw InternalError("llvm codegen: unknown answer told");
}

}  // namespace

auto ImplicitArgOf(const Told& told, std::optional<lir::TypeId> acted_on)
    -> ImplicitArg {
  return std::visit(
      Overloaded{
          [&](const ToldWidthMetadataAhead&) -> ImplicitArg {
            return WidthMetadataArg{
                .of = ToldOfType(
                    acted_on,
                    "how wide the values the storage it makes holds are")};
          },
          [&](const ToldTypeMetadataAhead&) -> ImplicitArg {
            return TypeMetadataArg{
                .of = ToldOfType(
                    acted_on,
                    "the type of the value the storage it makes holds")};
          },
          [](const ToldNothing&) -> ImplicitArg { return NoImplicitArg{}; },
          [](const ToldByTheStorage&) -> ImplicitArg {
            return NoImplicitArg{};
          },
          [](const ToldWidthMetadata&) -> ImplicitArg {
            return NoImplicitArg{};
          },
          [](const ToldLaidOutTypes&) -> ImplicitArg {
            return NoImplicitArg{};
          },
          [](const ToldKeyType&) -> ImplicitArg { return NoImplicitArg{}; },
          [](const ToldEntryType&) -> ImplicitArg { return NoImplicitArg{}; }},
      told);
}

auto TypeArgsOf(
    const lir::TypePool& types, const Told& told, support::AnswerTold declared,
    std::optional<lir::TypeId> called_at, lir::TypeId answered,
    const ReturnInfo& ret) -> std::vector<TypeArg> {
  switch (declared) {
    // A realization for values laid out by their type building one is
    // told its type, which its bytes do not state; whether one builds such a
    // value follows from the type a call answers at, which no declaration
    // holds.
    case support::AnswerTold::kNothing: {
      const bool builds_a_value_laid_out_by_type =
          std::holds_alternative<ToldLaidOutTypes>(told) &&
          std::holds_alternative<ReturnIndirect>(ret) &&
          IsLaidOutByType(types.Get(answered));
      if (!builds_a_value_laid_out_by_type) {
        return {};
      }
      return {TypeConstantArg{.type = answered}};
    }
    case support::AnswerTold::kBuiltArray:
    case support::AnswerTold::kAnswerType:
    case support::AnswerTold::kElementType:
    case support::AnswerTold::kIntegralExtent:
      return TypeArgsToldAs(types, declared, called_at, answered);
  }
  throw InternalError("llvm codegen: unknown answer told");
}

auto FnAbiOf(
    CodeGenModule& module, RuntimeOp op,
    std::span<const StatedOperand> operands, const Told& told,
    std::optional<lir::TypeId> acted_on, std::optional<lir::TypeId> called_at,
    ReturnInfo ret) -> FnAbi {
  const support::OperandReadings readings = OperandReadingsOf(op);
  std::vector<ArgAbi> args;
  args.reserve(operands.size());
  for (std::size_t i = 0; i < operands.size(); ++i) {
    const support::OperandReading reading = readings.At(i);
    args.push_back(
        std::visit(
            Overloaded{
                [&](const ValueOperand& value) {
                  RequireReadAs(module.Types(), reading, value.type);
                  return ArgAbi{
                      .type = module.Types().Map(value.type),
                      .mode = ToldModeOf(
                                  told, i, reading, acted_on, value.type,
                                  module.Unit().types)
                                  .value_or(PassModeOf(reading, value.type))};
                },
                // A machine value is of no type whose facts could cross with
                // it, so it is only ever read as the machine value it is.
                [&](const MachineOperand& machine) {
                  switch (reading) {
                    case support::OperandReading::kMachine:
                      return CallArranger::Direct(machine.type);
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
            operands[i]));
  }
  return FnAbi{
      .implicit_arg = ImplicitArgOf(told, acted_on),
      .named_part = std::nullopt,
      .args = std::move(args),
      .type_args = TypeArgsCalledAt(AnswerToldOf(op), called_at),
      .ret = ret};
}

auto CallArranger::ArgOf(
    const support::OperandReadings& readings, std::size_t index,
    lir::TypeId operand, const Told& told,
    std::optional<lir::TypeId> acted_on) const -> ArgAbi {
  const support::OperandReading reading = readings.At(index);
  RequireReadAs(module_->Types(), reading, operand);
  return ArgAbi{
      .type = module_->Types().Map(operand),
      .mode =
          ToldModeOf(
              told, index, reading, acted_on, operand, module_->Unit().types)
              .value_or(llvm_backend::PassModeOf(reading, operand))};
}

auto CallArranger::ArgsAsRead(
    const lir::CallInstr& call, const support::OperandReadings& readings,
    const Told& told, std::optional<lir::TypeId> acted_on) const
    -> std::vector<ArgAbi> {
  std::vector<ArgAbi> args;
  args.reserve(call.args.size());
  for (std::size_t i = 0; i < call.args.size(); ++i) {
    args.push_back(
        ArgOf(readings, i, OperandType(call.args[i]), told, acted_on));
  }
  return args;
}

auto CallArranger::Direct(llvm::Type* type) -> ArgAbi {
  return ArgAbi{.type = type, .mode = PassDirect{}};
}

auto CallArranger::ReturnOfValue(CodeGenTypes& types, lir::TypeId type)
    -> ReturnInfo {
  if (types.HasAggregateEvaluationKind(type)) {
    return ReturnIndirect{.returned = types.Ptr()};
  }
  return ReturnDirect{.type = types.Map(type)};
}

auto CallArranger::Arrange(std::vector<ArgAbi> args, ReturnInfo ret) -> FnAbi {
  return FnAbi{
      .implicit_arg = NoImplicitArg{},
      .named_part = std::nullopt,
      .args = std::move(args),
      .type_args = {},
      .ret = ret};
}

auto CallArranger::ReturnOf(
    const lir::CallInstr& call, lir::TypeId result_type) const -> ReturnInfo {
  // What builds its answer where its caller says answers with where that is.
  const ReturnInfo built = ReturnIndirect{.returned = module_->Types().Ptr()};
  // An execution is built in storage its caller gives, whatever the body it
  // runs answers once it is driven.
  if (lir::CallBuildsExecution(call.target)) {
    return built;
  }
  if (lir::CallMakesValue(call.target)) {
    return ReturnOfValue(module_->Types(), result_type);
  }
  return ReturnDirect{.type = module_->Types().Map(result_type)};
}

// The entry behind a builtin. What names it is the operation, plus -- where the
// library realizes an operation once per value representation -- the
// representation of a value the call carries, which is one it is handed for an
// operation on a value and the one it answers with for a factory. Which of
// those the builtin takes is the builtin's own property, so it is read from its
// identity.
auto CallArranger::EntryOf(
    const lir::BuiltinTarget& target, const lir::CallInstr& call,
    lir::TypeId result_type) const -> diag::Result<LibraryEntry> {
  using Named = diag::Result<LibraryEntry>;
  const auto over = [&](diag::Result<support::ValueDomain> domain,
                        std::optional<lir::TypeId> acted_on) -> Named {
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return LibraryEntry{
        .symbol = RuntimeSymbol(*domain, target.fn),
        .told = ToldOf(*domain, target.fn),
        .acted_on = acted_on};
  };
  return std::visit(
      Overloaded{
          [&](const NamedAlone&) -> Named {
            return LibraryEntry{
                .symbol = RuntimeSymbol(target.fn),
                .told = ToldNothing{},
                .acted_on = std::nullopt};
          },
          [&](const NamedByValue& named) -> Named {
            return over(
                module_->DomainOf(OperandType(call.args.at(named.operand))),
                std::nullopt);
          },
          [&](const NamedByResult&) -> Named {
            return over(module_->DomainOf(result_type), std::nullopt);
          },
          // The operation acts on the wrapper itself rather than reaching
          // through it, and a wrapper classifies the same way whether it
          // arrives as an operand or as a place.
          [&](const NamedByWrapper&) -> Named {
            const std::optional<std::pair<WrapperKind, lir::TypeId>> wrapper =
                WrapperOf(StorageReached(OperandType(call.args.at(0))));
            if (!wrapper.has_value()) {
              throw InternalError(
                  "llvm codegen: an operation on a wrapper needs one to act "
                  "on");
            }
            const auto& [kind, held] = *wrapper;
            auto domain = module_->DomainOf(held);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            return LibraryEntry{
                .symbol = RuntimeSymbol(*domain, kind, target.fn),
                .told = ToldOf(*domain, kind, target.fn),
                .acted_on = held};
          },
          [&](const NamedByStorageDomain&) -> Named {
            const std::optional<lir::TypeId> held =
                ValuesHeldBy(StorageReached(OperandType(call.args.at(0))));
            return over(
                StorageDomainBehind(OperandType(call.args.at(0))), held);
          },
          [&](const NamedByConversion&) -> Named {
            auto destination = module_->DomainOf(result_type);
            if (!destination) {
              return std::unexpected(std::move(destination.error()));
            }
            auto source = module_->DomainOf(OperandType(call.args.front()));
            if (!source) {
              return std::unexpected(std::move(source.error()));
            }
            return LibraryEntry{
                .symbol = RuntimeSymbol(*destination, target.fn, *source),
                .told = ToldNothing{},
                .acted_on = std::nullopt};
          },
          [&](const OverIntegralValues&) -> Named {
            return Unsupported(
                std::format(
                    "llvm codegen: the {} builtin is an operation over "
                    "integral values and the library has no entry for it by "
                    "that name",
                    support::RuntimeEntryOf(target.fn).name));
          },
          [&](const NotRealized& unrealized) -> Named {
            return Unsupported(
                std::format(
                    "llvm codegen: the {} builtin {} and the library has no "
                    "entry of that shape",
                    support::RuntimeEntryOf(target.fn).name, unrealized.shape));
          }},
      EntryNamingOf(target.fn));
}

auto CallArranger::ConstructorOf(const lir::CallInstr& call, lir::TypeId result)
    const -> diag::Result<std::string> {
  auto construction = ConstructionOf(call, result);
  if (!construction) {
    return std::unexpected(std::move(construction.error()));
  }
  return std::move(construction->symbol);
}

auto CallArranger::ConstructionOf(
    const lir::CallInstr& call, lir::TypeId result) const
    -> diag::Result<Construction> {
  using Built = diag::Result<Construction>;
  const lir::TypePool& types = module_->Unit().types;
  const auto entry = [](RuntimeOp op) -> Built {
    return Construction{
        .symbol = RuntimeSymbol(op),
        .operands = OperandReadingsOf(op),
        .told = ToldNothing{},
        .answer_told = AnswerToldOf(op),
        .implicit_arg = NoImplicitArg{}};
  };
  const auto entry_over = [](support::ValueDomain domain,
                             RuntimeOp op) -> Built {
    return Construction{
        .symbol = RuntimeSymbol(domain, op),
        .operands = OperandReadingsOf(op),
        .told = ToldOf(domain, op),
        .answer_told = AnswerToldOf(op),
        .implicit_arg = NoImplicitArg{}};
  };
  const auto no_construct = [&]() -> Built {
    return Unsupported(
        std::format(
            "llvm codegen: a value of type {} has no construct on this backend",
            types.Get(result).KindName()));
  };
  // An entry named by the representation of the value the construction is built
  // over, which is the call's first operand. An integral value has one entry
  // serving every integral type, where every other value has its domain's.
  const auto over_operand = [&](RuntimeOp of_a_domain,
                                RuntimeOp of_an_integral) -> Built {
    const lir::TypeId operand = OperandType(call.args.at(0));
    if (module_->Types().IntegralShapeOf(operand).has_value()) {
      return entry(of_an_integral);
    }
    auto domain = module_->DomainOf(operand);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return entry_over(*domain, of_a_domain);
  };
  const auto real_from_host = [&]() -> Built {
    if (!types.Get(OperandType(call.args.at(0))).Is<lir::MachineFloatType>()) {
      return Unsupported(
          std::format(
              "llvm codegen: building a {} from a host scalar has no entry on "
              "this backend",
              types.Get(result).KindName()));
    }
    auto domain = module_->DomainOf(result);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return entry_over(*domain, RuntimeOp::kConst);
  };
  return types.Get(result).Visit(
      Overloaded{
          [&](const lir::StringType&) -> Built {
            return entry_over(support::ValueDomain::kString, RuntimeOp::kMake);
          },
          // LRM 6.14: a chandle is a host pointer, so the domain carries its
          // value inline and what comes into existence is the pointer the
          // boundary handed back.
          [&](const lir::ChandleType&) -> Built {
            return entry_over(support::ValueDomain::kChandle, RuntimeOp::kMake);
          },
          [&](const lir::ClosureType&) -> Built {
            throw InternalError(
                "llvm codegen: a closure is built by an instruction of its "
                "own, never by a construction -- please report this as a bug");
          },
          // A container is built over an element list laid down a stated number
          // of times, and a queue holds as many as the bound its type declares
          // admits (LRM 7.10.5), which the entry is told with that type.
          [&](const lir::DynamicArrayType&) -> Built {
            return entry_over(
                support::ValueDomain::kDynArray, RuntimeOp::kFromLiteral);
          },
          [&](const lir::QueueType&) -> Built {
            return entry_over(
                support::ValueDomain::kQueue, RuntimeOp::kFromLiteral);
          },
          // A fixed-size unpacked array's type fixes how many elements it has,
          // so its element list is all it is built over.
          [&](const lir::UnpackedArrayType&) -> Built {
            return entry_over(
                support::ValueDomain::kUnpackedArray, RuntimeOp::kFromElements);
          },
          // LRM 7.8: the index type imposes the order the entries are held in.
          // Every declared index type's order is its own, which an index's type
          // answers wherever the index crosses; a wildcard index (LRM 7.8.1)
          // is self-determined, treated as unsigned, and admits one value at
          // any width, so its order is absent from the indices and the array
          // is built holding it.
          [&](const lir::AssociativeArrayType& a) -> Built {
            const bool wildcard =
                types.Get(a.key_type).Is<lir::WildcardIndexType>();
            return entry_over(
                support::ValueDomain::kAssocArray,
                wildcard ? RuntimeOp::kFromEntriesWildcard
                         : RuntimeOp::kFromEntries);
          },
          // A sequence outlives the body that built it -- the owner keeps
          // its address, and a dimension above it keeps that address as an
          // ordinary element -- so the runtime takes the handles rather than
          // the element list standing as the value.
          [&](const lir::VectorType&) -> Built {
            return entry(RuntimeOp::kSequenceMake);
          },
          [&](const lir::RuntimeLibraryType& r) -> Built {
            switch (r.kind) {
              case lir::RuntimeLibraryKind::kPrintLiteralItem:
                return entry(RuntimeOp::kMakePrintLiteralItem);
              case lir::RuntimeLibraryKind::kHierarchySegment:
                return entry(RuntimeOp::kMakeSegment);
              case lir::RuntimeLibraryKind::kTrigger:
                return entry(RuntimeOp::kMakeTrigger);
              case lir::RuntimeLibraryKind::kFormatSpec:
                return entry(RuntimeOp::kMakeFormatSpec);
              // What a value formats as is the value's own answer, so both of
              // these are named by the representation of what they are built
              // over. Each borrows that value rather than copying it, which
              // holds because the value it borrows belongs to the same
              // full-expression as the print, and ends only after it.
              case lir::RuntimeLibraryKind::kPrintValueItem:
                return over_operand(
                    RuntimeOp::kMakePrintValueItem,
                    RuntimeOp::kMakeIntegralPrintValueItem);
              case lir::RuntimeLibraryKind::kFormatArg:
                return over_operand(
                    RuntimeOp::kMakeFormatArg,
                    RuntimeOp::kMakeIntegralFormatArg);
              // The DPI-C boundary temporaries (LRM 35.5.6.1, Annex H.7.7).
              // Each images one SV value in the canonical form the C side
              // reads, so the entry is one function over every value it can
              // image: a vector's reads its bits, and an open array's is
              // handed the actual with its type.
              case lir::RuntimeLibraryKind::kDpiBitBuffer:
                return entry(RuntimeOp::kMakeDpiBitBuffer);
              case lir::RuntimeLibraryKind::kDpiLogicBuffer:
                return entry(RuntimeOp::kMakeDpiLogicBuffer);
              case lir::RuntimeLibraryKind::kDpiOpenArray:
                return entry(RuntimeOp::kMakeDpiOpenArray);
              // The rest come into existence some other way, so a construction
              // naming one would have nothing to call. A print item is built as
              // one of its two forms and never as their sum; a time format, an
              // open-array handle, a control effect and an observation are what
              // some other entry answers with, and so are a read report and a
              // held wait; a chunk is the element type a canonical buffer's
              // pointer addresses rather than a value; a cancellation target,
              // a timer and a channel's joint cancel state are storage the
              // owner holds and reaches by address; and a write into an object
              // is opened by the entry that opens it.
              case lir::RuntimeLibraryKind::kPrintItem:
              case lir::RuntimeLibraryKind::kTimeFormat:
              case lir::RuntimeLibraryKind::kDpiBitChunk:
              case lir::RuntimeLibraryKind::kDpiLogicChunk:
              case lir::RuntimeLibraryKind::kDpiOpenArrayHandle:
              case lir::RuntimeLibraryKind::kControlEffect:
              case lir::RuntimeLibraryKind::kObservation:
              case lir::RuntimeLibraryKind::kReadReport:
              case lir::RuntimeLibraryKind::kWait:
              case lir::RuntimeLibraryKind::kObjectWrite:
              case lir::RuntimeLibraryKind::kCancellationTarget:
              case lir::RuntimeLibraryKind::kTimer:
              case lir::RuntimeLibraryKind::kChannelCancellation:
              // A class's definition, what a definition is made of, an
              // enumeration's member table and a construction table are
              // constants a unit emits, so a body names one and never builds
              // one.
              case lir::RuntimeLibraryKind::kEnumeration:
              case lir::RuntimeLibraryKind::kObjectDefinition:
              case lir::RuntimeLibraryKind::kScopeInfo:
              case lir::RuntimeLibraryKind::kScopeCallable:
              case lir::RuntimeLibraryKind::kConstructionTable:
              case lir::RuntimeLibraryKind::kInstantiation:
                return no_construct();
            }
            throw InternalError("llvm codegen: unknown runtime library kind");
          },
          // A shared owner answers with a hold, because what it brings into
          // existence outlives the scope that asked for it and ends with the
          // last holder rather than at any one exit (LRM 6.21); a hold makes an
          // empty variable's cell, so it takes nothing and is named by the
          // domain the cell holds. A sole owner is the address of the storage
          // it takes over, which no entry builds, and a borrowed pointer is
          // bound to storage that already exists, so nothing constructs one.
          [&](const lir::PointerType& p) -> Built {
            switch (p.ownership) {
              case lir::PointerOwnership::kUnique:
                throw InternalError(
                    "llvm codegen: a unique pointer is the address of the "
                    "storage it takes over, which no entry builds -- please "
                    "report this as a bug");
              case lir::PointerOwnership::kShared: {
                const auto* cell =
                    types.Get(p.pointee).As<lir::ObservableType>();
                if (cell == nullptr) {
                  throw InternalError(
                      "llvm codegen: a counted hold is made over a variable's "
                      "cell -- please report this as a bug");
                }
                auto domain = module_->DomainOf(cell->value);
                if (!domain) {
                  return std::unexpected(std::move(domain.error()));
                }
                const Told told = ToldOf(*domain, RuntimeOp::kSharedCellMake);
                return Construction{
                    .symbol =
                        RuntimeSymbol(*domain, RuntimeOp::kSharedCellMake),
                    .operands = OperandReadingsOf(RuntimeOp::kSharedCellMake),
                    .told = told,
                    .answer_told = AnswerToldOf(RuntimeOp::kSharedCellMake),
                    .implicit_arg = ImplicitArgOf(told, cell->value)};
              }
              case lir::PointerOwnership::kBorrowed:
                return no_construct();
            }
            throw InternalError("llvm codegen: unknown pointer ownership");
          },
          // A handle owning an object the program built (LRM 8.3), handed that
          // object once its construction has run.
          [&](const lir::ManagedRefType&) -> Built {
            return entry(RuntimeOp::kObjectAdopt);
          },
          // Landing a machine integer in a real and reshaping across precisions
          // are named conversions, so what reaches the real family here is a
          // build over a host scalar: a constant of the destination's own
          // precision. Anything else the boundary hands back has no entry.
          [&](const lir::RealType&) -> Built { return real_from_host(); },
          [&](const lir::ShortRealType&) -> Built { return real_from_host(); },
          // A value carrying no bits has exactly one value (a tagged union's
          // void member, LRM 7.3.2), so its construction takes nothing and
          // yields that value.
          [&](const lir::EmptyType&) -> Built {
            return entry_over(
                support::ValueDomain::kEmpty, RuntimeOp::kDefault);
          },

          // Nothing below has a construction entry on this backend. Each says
          // so on a line of its own, because "nothing brings one of these into
          // existence" is a claim about that type and a reader can only check
          // a claim that was made; a type added later lands in none of them
          // and fails to compile until someone places it.

          // A value that is a vector of bits, or a host scalar standing beside
          // one.
          [&](const lir::IntegralType&) -> Built { return no_construct(); },
          [&](const lir::WildcardIndexType&) -> Built {
            return no_construct();
          },
          [&](const lir::MachineBoolType&) -> Built { return no_construct(); },
          [&](const lir::MachineIntType&) -> Built { return no_construct(); },
          [&](const lir::MachineFloatType&) -> Built { return no_construct(); },
          [&](const lir::MachineCStringType&) -> Built {
            return no_construct();
          },
          [&](const lir::VoidType&) -> Built { return no_construct(); },

          // Aggregates this layer lays out itself, and the code address of a
          // body.
          [&](const lir::MachineArrayType&) -> Built { return no_construct(); },
          [&](const lir::TupleType&) -> Built { return no_construct(); },
          [&](const lir::StructType&) -> Built { return no_construct(); },

          // A union's value is made by the instruction that states which
          // member it holds, never by a construction.
          [&](const lir::UnionType&) -> Built { return no_construct(); },
          [&](const lir::TaggedUnionType&) -> Built { return no_construct(); },
          [&](const lir::MachineFunctionType&) -> Built {
            return no_construct();
          },
          [&](const lir::CoroutineType&) -> Built { return no_construct(); },

          // A node of the object tree. What brings one into existence is the
          // construction of the unique owner whose pointee it is, so the node
          // type itself never names an entry.
          [&](const lir::ObjectType&) -> Built { return no_construct(); },
          [&](const lir::CrossUnitClassType&) -> Built {
            return no_construct();
          },
          [&](const lir::RuntimeClassType&) -> Built { return no_construct(); },

          // A reference is bound to storage that already exists, naming it
          // rather than bringing it about, so nothing constructs one.
          [&](const lir::VariableRefType&) -> Built { return no_construct(); },

          // Storage an owner holds, which comes into existence with the owner
          // and is reached by address.
          [&](const lir::ObservableType&) -> Built { return no_construct(); },
          [&](const lir::ResolvedType&) -> Built { return no_construct(); },
          [&](const lir::DriverType&) -> Built { return no_construct(); },
          [&](const lir::SampledHistoryType&) -> Built {
            return no_construct();
          },
          [&](const lir::EvaluationAttemptsType&) -> Built {
            return no_construct();
          },
          [&](const lir::LoopBlocksType&) -> Built { return no_construct(); },
          [&](const lir::EventType&) -> Built { return no_construct(); },
          // A write is opened by the wrapper it writes through, which is an
          // operation on the wrapper rather than a value anything builds, and
          // a part designated within it is built by the step that reaches it.
          [&](const lir::OpenWriteType&) -> Built { return no_construct(); },
          [&](const lir::DesignationType&) -> Built { return no_construct(); },

          // A stable runtime facade, realized as a live reference rather than
          // as a value anything builds.
          [&](const lir::RuntimeEffectsType&) -> Built {
            return no_construct();
          },
          [&](const lir::FilesType&) -> Built { return no_construct(); },
          [&](const lir::DiagnosticType&) -> Built { return no_construct(); }});
}

auto CallArranger::FnAbiOf(const lir::CallInstr& call, lir::TypeId result_type)
    const -> diag::Result<FnAbi> {
  using Arranged = diag::Result<FnAbi>;
  const lir::TypePool& types = module_->Unit().types;
  const ReturnInfo ret = ReturnOf(call, result_type);
  // Code whose parameters are already typed takes each operand as the call
  // states it.
  const auto as_stated = [&]() -> Arranged {
    std::vector<ArgAbi> args;
    args.reserve(call.args.size());
    for (const lir::Operand& arg : call.args) {
      args.push_back(Direct(module_->Types().Map(OperandType(arg))));
    }
    return Arrange(std::move(args), ret);
  };
  // An entry acting on storage that holds values of type `value`, as the
  // realization for that type's domain takes its operands. No such entry's
  // declaration says it is told anything of its answer.
  const auto on_storage_of = [&](lir::TypeId value,
                                 const auto& op) -> Arranged {
    auto domain = module_->DomainOf(value);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    const Told told = ToldOf(*domain, op);
    return FnAbi{
        .implicit_arg = ImplicitArgOf(told, value),
        .named_part = std::nullopt,
        .args = ArgsAsRead(call, OperandReadingsOf(op), told, value),
        .type_args = TypeArgsOf(
            types, told, support::AnswerTold::kNothing, std::nullopt,
            result_type, ret),
        .ret = ret};
  };
  // A part the callee names goes right after the object whose part it is, and
  // first where the entry acts on no object -- which is what its declaration
  // says by being a factory on the type it builds.
  const auto named_part_of =
      [](const lir::BuiltinTarget& t,
         const support::RuntimeEntry& declared) -> std::optional<NamedPartArg> {
    if (!t.position.has_value()) {
      return std::nullopt;
    }
    const bool acts_on_an_object =
        !std::holds_alternative<support::StaticFactory>(declared.declaration);
    return NamedPartArg{
        .before = acts_on_an_object ? std::size_t{1} : std::size_t{0},
        .position = t.position->value};
  };
  return std::visit(
      Overloaded{
          [&](const lir::BuiltinTarget& t) -> Arranged {
            auto entry = EntryOf(t, call, result_type);
            if (!entry) {
              return std::unexpected(std::move(entry.error()));
            }
            const support::RuntimeEntry declared =
                support::RuntimeEntryOf(t.fn);
            return FnAbi{
                .implicit_arg = ImplicitArgOf(entry->told, entry->acted_on),
                .named_part = named_part_of(t, declared),
                .args = ArgsAsRead(
                    call, declared.operands, entry->told, entry->acted_on),
                .type_args = TypeArgsOf(
                    types, entry->told, declared.answer_told, t.type_argument,
                    result_type, ret),
                .ret = ret};
          },
          [&](const lir::ConstructTarget&) -> Arranged {
            auto construction = ConstructionOf(call, result_type);
            if (!construction) {
              return std::unexpected(std::move(construction.error()));
            }
            return FnAbi{
                .implicit_arg = construction->implicit_arg,
                .named_part = std::nullopt,
                .args = ArgsAsRead(
                    call, construction->operands, construction->told, {}),
                .type_args = TypeArgsOf(
                    types, construction->told, construction->answer_told,
                    std::nullopt, result_type, ret),
                .ret = ret};
          },
          [&](const lir::FunctionTarget&) { return as_stated(); },
          [&](const lir::DispatchTarget&) { return as_stated(); },
          [&](const lir::IndirectTarget&) { return as_stated(); },
          [&](const lir::LibraryConstructorTarget&) { return as_stated(); },
          [&](const lir::SymbolTarget&) { return as_stated(); },
          [&](const lir::ForeignTarget&) { return as_stated(); },
          [&](const lir::ValueCellTarget& t) -> Arranged {
            return on_storage_of(t.value, t.op);
          },
          [&](const lir::OpenWriteTarget& t) -> Arranged {
            return on_storage_of(t.value, t.op);
          },
          // The entry takes the designation, the parts the bits lie in as
          // one span, where the bits start, and the bits, whatever number of
          // parts the write reaches them through; it is told how wide the
          // value designated is.
          [&](const lir::DesignatedBitsTarget& t) -> Arranged {
            auto domain = module_->DomainOf(t.value);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            const support::OperandReadings readings =
                OperandReadingsOf(RuntimeOp::kInsertDesignatedBits);
            const Told told = ToldOf(*domain, RuntimeOp::kInsertDesignatedBits);
            return FnAbi{
                .implicit_arg = NoImplicitArg{},
                .named_part = std::nullopt,
                .args =
                    {ArgOf(
                         readings, 0, OperandType(call.args.front()), told,
                         t.value),
                     Direct(module_->Types().Span()),
                     ArgOf(
                         readings, 2, OperandType(t.parts.back().start), told,
                         t.value),
                     ArgOf(readings, 3, t.parts.back().type, told, t.value)},
                .type_args = {},
                .ret = ret};
          },
          [&](const lir::EndValueTarget&) { return as_stated(); },
          [&](const lir::CopyValueTarget&) { return as_stated(); },
          [&](const lir::DynamicStorageTarget&) { return as_stated(); },
          [&](const lir::ControlEffectTarget&) { return as_stated(); },
          [&](const lir::CoroutineTarget&) { return as_stated(); }},
      call.target);
}

}  // namespace lyra::backend::llvm_backend
