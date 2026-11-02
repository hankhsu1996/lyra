#include "lyra/backend/llvm/target_lowering.hpp"

#include <array>
#include <cstddef>
#include <iterator>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/llvm/entry_realizations.hpp"
#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/backend/llvm/type_realization.hpp"
#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

namespace {

template <typename... Args>
auto Unsupported(base::DiagFormatFor<Args...> format, Args&&... args)
    -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm, format,
      std::forward<Args>(args)...);
}

// How a value cell of `domain` is written where its value lies, none where the
// cell keeps an object of the library's own.
auto CellStoreOf(support::ValueDomain domain) -> std::optional<CellStore> {
  return runtime::HeldContentsOf(support::ValueCellMember{.domain = domain})
      .transform([&](const runtime::HeldContents& contents) {
        return CellStore{.domain = domain, .contents = contents};
      });
}

// The declaration a member step names as the one that gave the member.
auto DeclarationOf(const lir::CompilationUnit& unit, lir::TypeId declared_by)
    -> lir::TypeDeclaration {
  std::optional<lir::TypeDeclaration> declaration =
      unit.types.Get(declared_by).Declaration();
  if (!declaration.has_value()) {
    throw InternalError(
        "llvm codegen: a member step names a type that declares nothing");
  }
  return *std::move(declaration);
}

}  // namespace

TargetLowering::TargetLowering(
    const lir::CompilationUnit& unit, const lir::Function& fn)
    : unit_(&unit), fn_(&fn) {
}

auto TargetLowering::OperandType(const lir::Operand& operand) const
    -> lir::TypeId {
  return lir::OperandType(*fn_, operand);
}

auto TargetLowering::StorageReached(lir::TypeId operand) const
    -> const lir::Type& {
  const lir::TypePool& types = unit_->types;
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

auto TargetLowering::ValuesBehind(lir::TypeId operand) const -> lir::TypeId {
  const std::optional<lir::TypeId> held = ValuesHeldBy(StorageReached(operand));
  if (!held.has_value()) {
    throw InternalError(
        "llvm codegen: an entry named by the representation of what a storage "
        "holds needs a storage whose values are all of one");
  }
  return *held;
}

auto TargetLowering::StatedOperands(const lir::CallInstr& call) const
    -> std::vector<StatedOperand> {
  std::vector<StatedOperand> stated;
  stated.reserve(call.args.size());
  for (const lir::Operand& arg : call.args) {
    stated.emplace_back(ValueOperand{.type = OperandType(arg)});
  }
  return stated;
}

auto TargetLowering::CallFormOf(
    const lir::CallInstr& call, lir::TypeId result_type) const
    -> diag::Result<CallForm> {
  if (std::holds_alternative<lir::ConstructTarget>(call.target) &&
      unit_->types.Get(result_type).IsMoveOnly()) {
    return ForwardedOperand{};
  }
  const auto* cell = std::get_if<lir::ValueCellTarget>(&call.target);
  if (cell == nullptr) {
    return OrdinaryCall{};
  }
  auto domain = RealizedDomainOf(*unit_, cell->value);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  // A cell whose value lies where the library states is read and written
  // there; only one keeping an object of the library's own is asked of the
  // library.
  switch (cell->op) {
    case lir::ValueCellTarget::Op::kAllocate:
      return OrdinaryCall{};
    case lir::ValueCellTarget::Op::kLoad:
      return CellLoad{.domain = *domain};
    case lir::ValueCellTarget::Op::kStore:
      if (const std::optional<CellStore> store = CellStoreOf(*domain)) {
        return *store;
      }
      return OrdinaryCall{};
  }
  throw InternalError("llvm codegen: unknown value cell operation");
}

auto TargetLowering::OperatorFormOf(const lir::BinaryInstr& binary) const
    -> diag::Result<OperatorForm> {
  const lir::TypeId operand = OperandType(binary.lhs);
  const lir::Type& operated = unit_->types.Get(operand);
  // A machine integer's operator is the machine instruction over one scalar,
  // and an address compared with another is a machine comparison too, as it is
  // in what clang makes of the same comparison.
  if (operated.MachineIntegerSignedness().has_value() ||
      operated.Is<lir::PointerType>()) {
    return MachineOperator{
        .signedness = operated.MachineIntegerSignedness().value_or(
            lir::Signedness::kUnsigned)};
  }
  if (operated.Is<lir::UnmanagedRefType>()) {
    return IdentityOperator{};
  }
  if (operated.Is<lir::RealType>()) {
    return RealOperator{};
  }
  if (operated.Is<lir::IntegralType>()) {
    return IntegralOperator{};
  }
  auto domain = RealizedDomainOf(*unit_, operand);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  // An operator's entry takes each operand as the value it is, and is told what
  // the realization for its domain is told.
  const std::array<StatedOperand, 2> operands{
      ValueOperand{.type = operand},
      ValueOperand{.type = OperandType(binary.rhs)}};
  return EntryCall{
      .symbol = RuntimeSymbol(*domain, binary.op),
      .abi = ArrangeCall(
          unit_->types, support::OperandReadings{}, operands,
          Realization{.told = ToldOfAnOperator(*domain), .of = operand},
          std::nullopt, {})};
}

auto TargetLowering::StoreFormOf(const lir::StoreInstr& store) const
    -> diag::Result<StoreForm> {
  auto wrapper = WrapperPlaceOf(store.place);
  if (!wrapper) {
    return std::unexpected(std::move(wrapper.error()));
  }
  // What a store is told is of the values its storage holds, which are of the
  // stored value's type, and what is stored crosses as a value they hold.
  const lir::TypeId stored = OperandType(store.value);
  const auto arranged = [&](const support::OperandReadings& readings,
                            const Told& told, StatedOperand storage) {
    const std::array<StatedOperand, 2> operands{
        storage, ValueOperand{.type = stored}};
    return ArrangeCall(
        unit_->types, readings, operands,
        Realization{.told = told, .of = stored}, std::nullopt, {});
  };
  if (wrapper->has_value()) {
    const auto& [domain, kind, place] = **wrapper;
    // The wrapper and then the value, each as the wrapper's store reads it.
    FnAbi abi = arranged(
        support::RuntimeEntryOf(support::BuiltinFn::kSet).operands,
        ToldOf(domain, kind, support::BuiltinFn::kSet),
        ValueOperand{
            .type = lir::PlaceTypeBefore(
                *unit_, *fn_, store.place, std::ssize(store.place.chain) - 1)});
    return WrapperStore{
        .wrapper = place,
        .entry = EntryCall{
            .symbol = RuntimeSymbol(domain, kind, support::BuiltinFn::kSet),
            .abi = std::move(abi)}};
  }
  const std::optional<support::ValueDomain> cell =
      PlaceValueCellDomain(store.place, stored);
  if (!cell.has_value()) {
    return AddressStore{};
  }
  // A cell holding the value as its own bytes takes them where they lie.
  if (const std::optional<CellStore> store = CellStoreOf(*cell)) {
    return *store;
  }
  return CellEntryStore{
      .entry = EntryCall{
          .symbol = RuntimeSymbol(*cell, lir::ValueCellTarget::Op::kStore),
          .abi = arranged(
              OperandReadingsOf(lir::ValueCellTarget::Op::kStore),
              ToldOf(*cell, lir::ValueCellTarget::Op::kStore),
              MachineOperand{.kind = MachineOperandKind::kAddress})}};
}

auto TargetLowering::BitsStoreCallOf(
    const lir::BitsStoreInstr& store, lir::TypeId whole) const -> EntryCall {
  const std::array<StatedOperand, 4> stated{
      ValueOperand{.type = whole},
      MachineOperand{.kind = MachineOperandKind::kSpan},
      ValueOperand{.type = OperandType(store.parts.back().start)},
      ValueOperand{.type = OperandType(store.bits)}};
  return EntryCall{
      .symbol = RuntimeSymbol(RuntimeOp::kInsertBits),
      .abi = ArrangeEntryCall(
          unit_->types, RuntimeOp::kInsertBits, stated, std::nullopt,
          std::nullopt)};
}

auto TargetLowering::ElementStepOf(
    lir::TypeId container, std::span<const lir::Operand> coordinates,
    PlaceAccess access) const -> diag::Result<ElementStep> {
  const lir::Type& stated = unit_->types.Get(container);
  if (const auto* array = stated.As<lir::UnpackedArrayType>()) {
    // What a position naming no element reaches is the element type's to
    // answer, so the entry is asked of that type alone.
    const RuntimeOp answered_by = [&] {
      switch (access) {
        case PlaceAccess::kRead:
          return RuntimeOp::kElementDefault;
        case PlaceAccess::kWrite:
          return RuntimeOp::kDiscardedWrite;
      }
      throw InternalError("llvm codegen: unknown access");
    }();
    return StepWithinArray{
        .no_element = EntryCall{
            .symbol = RuntimeSymbol(answered_by),
            .abi = ArrangeEntryCall(
                unit_->types, answered_by, {}, std::nullopt,
                array->element_type)}};
  }
  auto domain = RealizedDomainOf(*unit_, container);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  // A container indexed by a type is reached by a key, which crosses with the
  // type it was written in (LRM 7.8).
  const ElementEntries entries = stated.IndexType().has_value()
                                     ? KeyedElementEntries()
                                     : PositionedElementEntries(*domain);
  StepEntry entry = [&] {
    switch (access) {
      case PlaceAccess::kRead:
        return entries.read;
      case PlaceAccess::kWrite:
        return entries.write;
    }
    throw InternalError("llvm codegen: unknown access");
  }();
  const support::OperandReadings readings =
      support::RuntimeEntryOf(entry.entry).operands;
  std::vector<PassMode> modes;
  modes.reserve(coordinates.size());
  for (std::size_t i = 0; i < coordinates.size(); ++i) {
    const support::OperandReading reading = readings.At(i + 1);
    const lir::TypeId coordinate = OperandType(coordinates[i]);
    RequireReadAs(unit_->types, reading, coordinate);
    modes.push_back(CrossingModeOf(unit_->types, reading, coordinate));
  }
  return StepByEntry{
      .entry = std::move(entry), .coordinates = std::move(modes)};
}

auto TargetLowering::StorageAt(const lir::Place& place) const
    -> diag::Result<std::optional<support::DeclaredMemberStorage>> {
  const lir::TypeId type = lir::PlaceType(*unit_, *fn_, place);
  const lir::Type& stated = unit_->types.Get(type);
  if (stated.RuntimeClassOf().has_value() || stated.Is<lir::ObjectType>()) {
    return std::nullopt;
  }
  MemberSlotRole role = MemberSlotRole::kVariable;
  if (!place.chain.empty()) {
    if (const auto* member =
            std::get_if<lir::MemberProjection>(&place.chain.back())) {
      const std::optional<lir::TypeDeclaration> declaration =
          unit_->types.Get(member->member.declared_by).Declaration();
      if (declaration.has_value()) {
        role = MemberSlotRoleOf(*declaration);
      }
    }
  }
  const std::optional<support::DeclaredMemberStorage> storage =
      DeclaredStorageOf(*unit_, type, role);
  if (!storage) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedTypeKind,
        "storage of type '{}' is not yet supported with '--backend llvm'",
        stated.KindName());
  }
  return storage;
}

auto TargetLowering::WrapperPlaceOf(const lir::Place& place) const
    -> diag::Result<std::optional<WrapperPlace>> {
  // The last step names the storage behind whatever the chain had reached. When
  // that is a capability wrapper, the storage is the wrapper's contents, which
  // have no address of their own -- the wrapper decides what reading and
  // writing them mean, so the prefix names the wrapper and the access goes
  // through it.
  if (place.chain.empty() ||
      !std::holds_alternative<lir::DerefProjection>(place.chain.back())) {
    return std::nullopt;
  }
  const std::optional<std::pair<WrapperKind, lir::TypeId>> reached =
      WrapperOf(unit_->types.Get(
          lir::PlaceTypeBefore(
              *unit_, *fn_, place, std::ssize(place.chain) - 1)));
  if (!reached.has_value()) {
    return std::nullopt;
  }
  auto domain = RealizedDomainOf(*unit_, reached->second);
  if (!domain) {
    return std::unexpected(std::move(domain.error()));
  }
  return WrapperPlace{
      .domain = *domain,
      .kind = reached->first,
      .wrapper = lir::Place{
          .base = place.base,
          .chain = {place.chain.begin(), std::prev(place.chain.end())}}};
}

auto TargetLowering::PlaceValueCellDomain(
    const lir::Place& place, lir::TypeId value) const
    -> std::optional<support::ValueDomain> {
  // A value cell is storage the runtime built around a declaration's value and
  // owns the representation of, and only the member step naming it reaches it:
  // an address taken of one is of the value it holds, so what a dereference
  // arrives at is the value. An element or a component is the value itself,
  // where its container holds it; so is what a write opened on a wrapper
  // reaches, and a base local's own slot.
  if (place.chain.empty()) {
    return std::nullopt;
  }
  const auto* step = std::get_if<lir::MemberProjection>(&place.chain.back());
  if (step == nullptr) {
    return std::nullopt;
  }
  const std::optional<support::DeclaredMemberStorage> storage =
      DeclaredStorageOf(
          *unit_, value,
          MemberSlotRoleOf(DeclarationOf(*unit_, step->member.declared_by)));
  if (!storage.has_value()) {
    return std::nullopt;
  }
  const auto* cell = std::get_if<support::ValueCellMember>(&*storage);
  if (cell == nullptr) {
    return std::nullopt;
  }
  return cell->domain;
}

auto TargetLowering::AnswersInStorage(
    const lir::CallInstr& call, lir::TypeId result_type) const -> bool {
  // An execution is built in storage its caller gives, whatever the body it
  // runs answers once it is driven.
  return lir::CallBuildsExecution(call.target) ||
         (lir::CallMakesValue(call.target) &&
          unit_->types.Get(result_type).IsHeldInStorage());
}

// The entry behind a builtin. What names it is the operation, plus -- where the
// library realizes an operation once per value representation -- the
// representation of a value the call carries, which is one it is handed for an
// operation on a value and the one it answers with for a factory. Which of
// those the builtin takes is the builtin's own property, so it is read from its
// identity.
auto TargetLowering::EntryOf(
    const lir::BuiltinTarget& target, const lir::CallInstr& call,
    lir::TypeId result_type) const -> diag::Result<LibraryEntry> {
  using Named = diag::Result<LibraryEntry>;
  // The realization for the representation values of `of` are held in.
  const auto over = [&](lir::TypeId of) -> Named {
    auto domain = RealizedDomainOf(*unit_, of);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return LibraryEntry{
        .symbol = RuntimeSymbol(*domain, target.fn),
        .realization =
            Realization{.told = ToldOf(*domain, target.fn), .of = of}};
  };
  return std::visit(
      Overloaded{
          [&](const NamedAlone&) -> Named {
            return LibraryEntry{
                .symbol = RuntimeSymbol(target.fn),
                .realization = std::nullopt};
          },
          [&](const NamedByValue& named) -> Named {
            return over(OperandType(call.args.at(named.operand)));
          },
          [&](const NamedByResult&) -> Named { return over(result_type); },
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
            auto domain = RealizedDomainOf(*unit_, held);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            return LibraryEntry{
                .symbol = RuntimeSymbol(*domain, kind, target.fn),
                .realization = Realization{
                    .told = ToldOf(*domain, kind, target.fn), .of = held}};
          },
          [&](const NamedByStorageDomain&) -> Named {
            return over(ValuesBehind(OperandType(call.args.at(0))));
          },
          [&](const NamedByConversion&) -> Named {
            auto destination = RealizedDomainOf(*unit_, result_type);
            if (!destination) {
              return std::unexpected(std::move(destination.error()));
            }
            auto source =
                RealizedDomainOf(*unit_, OperandType(call.args.front()));
            if (!source) {
              return std::unexpected(std::move(source.error()));
            }
            return LibraryEntry{
                .symbol = RuntimeSymbol(*destination, target.fn, *source),
                .realization = std::nullopt};
          },
          [&](const OverIntegralValues&) -> Named {
            return Unsupported(
                "the operation '{}' over integral values is not yet supported "
                "with '--backend llvm'",
                support::RuntimeEntryOf(target.fn).name);
          },
          [&](const NotRealized& unrealized) -> Named {
            return Unsupported(
                "the operation '{}', which {}, is not yet supported with "
                "'--backend llvm'",
                support::RuntimeEntryOf(target.fn).name, unrealized.shape);
          }},
      EntryNamingOf(target.fn));
}

auto TargetLowering::ConstructorOf(
    const lir::CallInstr& call, lir::TypeId result) const
    -> diag::Result<std::string> {
  auto construction = ConstructionOf(call, result);
  if (!construction) {
    return std::unexpected(std::move(construction.error()));
  }
  return std::move(construction->symbol);
}

auto TargetLowering::ConstructionOf(
    const lir::CallInstr& call, lir::TypeId result) const
    -> diag::Result<Construction> {
  using Built = diag::Result<Construction>;
  const lir::TypePool& types = unit_->types;
  const auto entry = [](RuntimeOp op) -> Built {
    return Construction{
        .symbol = RuntimeSymbol(op),
        .operands = OperandReadingsOf(op),
        .realization = std::nullopt,
        .answer_told = AnswerToldOf(op)};
  };
  // The entry realized for `domain`, the representation values of `of` are
  // held in.
  const auto entry_over = [&](support::ValueDomain domain, lir::TypeId of,
                              RuntimeOp op) -> Built {
    return Construction{
        .symbol = RuntimeSymbol(domain, op),
        .operands = OperandReadingsOf(op),
        .realization = Realization{.told = ToldOf(domain, op), .of = of},
        .answer_told = AnswerToldOf(op)};
  };
  const auto no_construct = [&]() -> Built {
    return Unsupported(
        "building a value of type '{}' is not yet supported with "
        "'--backend llvm'",
        types.Get(result).KindName());
  };
  return types.Get(result).Visit(
      Overloaded{
          [&](const lir::ClosureType&) -> Built {
            throw InternalError(
                "llvm codegen: a closure is built by an instruction of its "
                "own, never by a construction -- please report this as a bug");
          },
          // A fixed-size unpacked array's type fixes how many elements it has,
          // so its element list is all it is built over.
          [&](const lir::UnpackedArrayType&) -> Built {
            return entry_over(
                support::ValueDomain::kLaidOut, result,
                RuntimeOp::kFromElements);
          },
          // A sequence outlives the body that built it -- the owner keeps
          // its address, and a dimension above it keeps that address as an
          // ordinary element -- so the runtime takes the handles rather than
          // the element list standing as the value.
          [&](const lir::VectorType&) -> Built {
            return entry(RuntimeOp::kSequenceMake);
          },
          // A type the library declares is built by the constructor its
          // declaration names, where it names one; the rest come into
          // existence some other way, so a construction naming one would have
          // nothing to call.
          [&](const lir::LibraryType& l) -> Built {
            const std::optional<support::LibraryConstructor> constructor =
                support::DeclarationOf(l.decl).constructor;
            if (!constructor.has_value()) {
              return no_construct();
            }
            return ConstructorEntryOf(*constructor, call, result);
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
                auto domain = RealizedDomainOf(*unit_, cell->value);
                if (!domain) {
                  return std::unexpected(std::move(domain.error()));
                }
                return entry_over(
                    *domain, cell->value, RuntimeOp::kSharedCellMake);
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
          // A reference to a value of any type is built referring to nothing,
          // which receives nothing (LRM 15.4.9).
          [&](const lir::AnyReferenceType&) -> Built {
            return Construction{
                .symbol = RuntimeSymbol(
                    support::LibraryObject::kAnyReference, RuntimeOp::kDefault),
                .operands = OperandReadingsOf(RuntimeOp::kDefault),
                .realization = std::nullopt,
                .answer_told = AnswerToldOf(RuntimeOp::kDefault)};
          },

          // Nothing below has a construction entry on this backend. Each says
          // so on a line of its own, because "nothing brings one of these into
          // existence" is a claim about that type and a reader can only check
          // a claim that was made; a type added later lands in none of them
          // and fails to compile until someone places it.

          // A value that is a vector of bits, or a host scalar standing beside
          // one.
          [&](const lir::IntegralType&) -> Built { return no_construct(); },
          // A real is the machine float of its width, which a conversion from
          // that float states.
          [&](const lir::RealType&) -> Built { return no_construct(); },
          [&](const lir::MachineBoolType&) -> Built { return no_construct(); },
          [&](const lir::MachineIntType&) -> Built { return no_construct(); },
          [&](const lir::MachineFloatType&) -> Built { return no_construct(); },
          [&](const lir::MachineCStringType&) -> Built {
            return no_construct();
          },
          [&](const lir::VoidType&) -> Built { return no_construct(); },
          // A reference owning nothing is the address it is given, which a
          // conversion from that address states.
          [&](const lir::UnmanagedRefType&) -> Built { return no_construct(); },

          // Aggregates this layer lays out itself, and the code address of a
          // body.
          [&](const lir::MachineArrayType&) -> Built { return no_construct(); },
          [&](const lir::MachineSpanType&) -> Built { return no_construct(); },
          [&](const lir::TupleType&) -> Built { return no_construct(); },
          // A record's value is made by the instruction that composes it --
          // a structure's from its members, a union's from the member it
          // holds -- never by a construction.
          [&](const lir::RecordType&) -> Built { return no_construct(); },
          [&](const lir::MachineFunctionType&) -> Built {
            return no_construct();
          },
          [&](const lir::ErasedCallableType&) -> Built {
            return no_construct();
          },
          [&](const lir::CoroutineType&) -> Built { return no_construct(); },

          // A node of the object tree. What brings one into existence is the
          // construction of the unique owner whose pointee it is, so the node
          // type itself never names an entry.
          [&](const lir::ObjectType&) -> Built { return no_construct(); },

          // A reference is bound to storage that already exists, naming it
          // rather than bringing it about, so nothing constructs one.
          [&](const lir::VariableRefType&) -> Built { return no_construct(); },

          // Storage an owner holds, which comes into existence with the owner
          // and is reached by address.
          [&](const lir::ObservableType&) -> Built { return no_construct(); },
          [&](const lir::ResolvedType&) -> Built { return no_construct(); },
          [&](const lir::DriverType&) -> Built { return no_construct(); },
          // A write is opened by the wrapper it writes through, which is an
          // operation on the wrapper rather than a value anything builds, and
          // a part designated within it is built by the step that reaches it.
          [&](const lir::OpenWriteType&) -> Built { return no_construct(); },
          [&](const lir::DesignationType&) -> Built { return no_construct(); },
          // A write into an object's properties is opened on the object,
          // which is an operation on the object rather than a value anything
          // builds.
          [&](const lir::ObjectWriteType&) -> Built {
            return no_construct();
          }});
}

auto TargetLowering::ConstructorEntryOf(
    const support::LibraryConstructor& declaring, const lir::CallInstr& call,
    lir::TypeId result) const -> diag::Result<Construction> {
  using Built = diag::Result<Construction>;
  const lir::TypePool& types = unit_->types;
  const support::BuiltinFn constructor = declaring.entry;
  const support::RuntimeEntry declared = support::RuntimeEntryOf(constructor);
  // The constructor `fn` realized once, and the declared constructor realized
  // for the domain values of `of` are held in.
  const auto alone = [](support::BuiltinFn fn) -> Built {
    const support::RuntimeEntry entry = support::RuntimeEntryOf(fn);
    return Construction{
        .symbol = RuntimeSymbol(fn),
        .operands = entry.operands,
        .realization = std::nullopt,
        .answer_told = entry.answer_told};
  };
  const auto over = [&](lir::TypeId of) -> Built {
    auto domain = RealizedDomainOf(*unit_, of);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    return Construction{
        .symbol = RuntimeSymbol(*domain, constructor),
        .operands = declared.operands,
        .realization =
            Realization{.told = ToldOf(*domain, constructor), .of = of},
        .answer_told = declared.answer_told};
  };
  return std::visit(
      Overloaded{
          [&](const NamedAlone&) { return alone(constructor); },
          [&](const NamedByResult&) { return over(result); },
          // What a value formats as is the value's own answer, and a
          // construction over a value is handed a reference to it, so the
          // entry is named by the representation of what that reference
          // reaches -- or is the one serving every integral value.
          [&](const NamedByValue& named) -> Built {
            const std::optional<lir::TypeId> referred =
                types.Get(OperandType(call.args.at(named.operand))).Pointee();
            if (!referred.has_value()) {
              throw InternalError(
                  "llvm codegen: a construction over a value is handed the "
                  "value rather than a reference to it -- please report this "
                  "as a bug");
            }
            if (types.Get(*referred).Is<lir::IntegralType>() &&
                declaring.integral_entry.has_value()) {
              return alone(*declaring.integral_entry);
            }
            return over(*referred);
          },
          [&](const NamedByWrapper&) -> Built {
            throw InternalError(
                "llvm codegen: a constructor is named by the storage a wrapper "
                "stands for -- please report this as a bug");
          },
          [&](const NamedByStorageDomain&) -> Built {
            throw InternalError(
                "llvm codegen: a constructor is named by the storage it acts "
                "on -- please report this as a bug");
          },
          [&](const NamedByConversion&) -> Built {
            throw InternalError(
                "llvm codegen: a constructor is named as a conversion -- "
                "please report this as a bug");
          },
          [&](const OverIntegralValues&) -> Built {
            return Unsupported(
                "the construction '{}' over integral values is not yet "
                "supported with '--backend llvm'",
                declared.name);
          },
          [&](const NotRealized& unrealized) -> Built {
            return Unsupported(
                "the construction '{}', which {}, is not yet supported with "
                "'--backend llvm'",
                declared.name, unrealized.shape);
          }},
      EntryNamingOf(constructor));
}

auto TargetLowering::FnAbiOf(
    const lir::CallInstr& call, lir::TypeId result_type) const
    -> diag::Result<FnAbi> {
  using Arranged = diag::Result<FnAbi>;
  const lir::TypePool& types = unit_->types;
  const bool in_storage = AnswersInStorage(call, result_type);
  // Code whose parameters are already typed takes each operand as the call
  // states it.
  const auto as_stated = [&]() -> Arranged {
    std::vector<ArgAbi> args;
    args.reserve(call.args.size());
    for (StatedOperand& operand : StatedOperands(call)) {
      args.push_back(
          ArgAbi{.operand = std::move(operand), .mode = PassDirect{}});
    }
    return FnAbi{
        .implicit_arg = NoImplicitArg{},
        .named_part = std::nullopt,
        .args = std::move(args),
        .type_args = {}};
  };
  // An entry acting on storage that holds values of type `value`, as the
  // realization for that type's domain takes its operands. No such entry's
  // declaration says it is told anything of its answer.
  const auto on_storage_of = [&](lir::TypeId value,
                                 const auto& op) -> Arranged {
    auto domain = RealizedDomainOf(*unit_, value);
    if (!domain) {
      return std::unexpected(std::move(domain.error()));
    }
    const Realization realization{.told = ToldOf(*domain, op), .of = value};
    return ArrangeCall(
        types, OperandReadingsOf(op), StatedOperands(call), realization,
        std::nullopt,
        AnswerTypeArgs(
            types, realization, support::AnswerTold::kNothing, std::nullopt,
            result_type, in_storage));
  };
  // A part the callee names goes right after the object whose part it is, and
  // first where the entry acts on no object -- which is what its declaration
  // says by being a factory on the type it builds. Every member of a union
  // lies at the union's own start in this target's layout, so a step into one
  // names no position, as clang's code generation adjusts no address for a
  // union's field.
  const auto named_part_of = [&](const lir::BuiltinTarget& t,
                                 const support::RuntimeEntry& declared)
      -> std::optional<NamedPartArg> {
    if (!t.position.has_value() ||
        t.fn == support::BuiltinFn::kDesignateMember) {
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
            return ArrangeCall(
                types, declared.operands, StatedOperands(call),
                entry->realization, named_part_of(t, declared),
                AnswerTypeArgs(
                    types, entry->realization, declared.answer_told,
                    t.type_argument, result_type, in_storage));
          },
          [&](const lir::ConstructTarget&) -> Arranged {
            auto construction = ConstructionOf(call, result_type);
            if (!construction) {
              return std::unexpected(std::move(construction.error()));
            }
            return ArrangeCall(
                types, construction->operands, StatedOperands(call),
                construction->realization, std::nullopt,
                AnswerTypeArgs(
                    types, construction->realization, construction->answer_told,
                    std::nullopt, result_type, in_storage));
          },
          [&](const lir::FunctionTarget&) { return as_stated(); },
          [&](const lir::DispatchTarget&) { return as_stated(); },
          [&](const lir::InterfaceDispatchTarget&) { return as_stated(); },
          [&](const lir::IndirectTarget&) { return as_stated(); },
          [&](const lir::CreationTarget&) { return as_stated(); },
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
            auto domain = RealizedDomainOf(*unit_, t.value);
            if (!domain) {
              return std::unexpected(std::move(domain.error()));
            }
            // What the realization is told is of the value designated, which
            // no operand is.
            const std::array<StatedOperand, 4> stated{
                ValueOperand{.type = OperandType(call.args.front())},
                MachineOperand{.kind = MachineOperandKind::kSpan},
                ValueOperand{.type = OperandType(t.parts.back().start)},
                ValueOperand{.type = t.parts.back().type}};
            return ArrangeCall(
                types, OperandReadingsOf(RuntimeOp::kInsertDesignatedBits),
                stated,
                Realization{
                    .told = ToldOf(*domain, RuntimeOp::kInsertDesignatedBits),
                    .of = t.value},
                std::nullopt, {});
          },
          [&](const lir::EndValueTarget&) { return as_stated(); },
          [&](const lir::CopyValueTarget&) { return as_stated(); },
          [&](const lir::MoveValueTarget&) { return as_stated(); },
          [&](const lir::DynamicStorageTarget&) { return as_stated(); },
          [&](const lir::BuildCellTarget&) -> Arranged {
            const lir::TypeId cell =
                *types.Get(OperandType(call.args.at(0))).Pointee();
            std::optional<FnAbi> build = ArrangeVariableBuild(*unit_, cell);
            if (!build.has_value()) {
              return as_stated();
            }
            return *std::move(build);
          },
          [&](const lir::ControlEffectTarget&) { return as_stated(); },
          [&](const lir::CoroutineTarget&) { return as_stated(); }},
      call.target);
}

}  // namespace lyra::backend::llvm_backend
