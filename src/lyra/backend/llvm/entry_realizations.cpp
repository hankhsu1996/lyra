#include "lyra/backend/llvm/entry_realizations.hpp"

#include <algorithm>
#include <array>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/declared_member_storage.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

namespace {

using Domain = support::ValueDomain;

constexpr DomainSet kOneWordIntegral{
    Domain::kBit8,   Domain::kBit16,   Domain::kBit32,   Domain::kBit64,
    Domain::kLogic8, Domain::kLogic16, Domain::kLogic32, Domain::kLogic64};
constexpr DomainSet kWideIntegral{Domain::kBitWide, Domain::kLogicWide};
constexpr DomainSet kIntegral = kOneWordIntegral | kWideIntegral;
// What is laid out by a type the design is compiled with and states nothing of
// it in its bytes: a tuple, a record, a fixed-size unpacked array, and a
// dynamic array, a queue or an associative array, whose bytes are the core
// keeping its elements.
constexpr DomainSet kLaidOutByType{Domain::kLaidOut};
// The kinds of value no other type parameterizes whose operations the library
// states whole.
constexpr DomainSet kValuesComparedWhole{
    Domain::kString, Domain::kReal, Domain::kShortReal, Domain::kUnmanagedRef,
    Domain::kManagedRef};
// The value domains the values of the declarations `picks` picks are objects
// of, among the declarations whose values are objects of a domain.
template <typename Picks>
constexpr auto DomainsDeclared(Picks picks) -> DomainSet {
  DomainSet found;
  support::ForEachLibraryDecl([&](support::LibraryDecl decl) {
    const auto* held = std::get_if<support::DomainObject>(
        &support::DeclarationOf(decl).holding);
    if (held != nullptr && picks(decl)) {
      found = found | DomainSet{held->domain};
    }
  });
  return found;
}

// The domains whose declaration names `fn` as what builds the value one starts
// as, or as its constructor.
constexpr auto DomainsBuiltBy(support::BuiltinFn fn) -> DomainSet {
  return DomainsDeclared([fn](support::LibraryDecl decl) {
    const support::LibraryDeclData& declaration = support::DeclarationOf(decl);
    return declaration.default_value == fn ||
           (declaration.constructor.has_value() &&
            declaration.constructor->entry == fn);
  });
}
// A real, which generated code lays out as the IEEE value of its width (LRM
// 6.12), copies as those bytes and computes on, and which the library still
// holds in a variable's cell and in a container, compares whole, and prints.
constexpr DomainSet kReals{Domain::kReal, Domain::kShortReal};
// A value generated code lays out as its own bytes beside an integral one -- a
// real, and a reference owning nothing, which it copies and compares as the
// address it is -- which the library still holds in a variable's cell and in
// a container, states the type of, and prints.
constexpr DomainSet kScalarsBesideIntegral =
    kReals | DomainSet{Domain::kUnmanagedRef};
// The values the library holds as an object of its own: every value that is
// neither its bytes nor laid out by a type compiled with the design -- a value
// of a type the library declares, and a class handle.
constexpr DomainSet kLibraryObjects =
    DomainsDeclared([](support::LibraryDecl) { return true; }) |
    DomainSet{Domain::kManagedRef};
// What no net type resolves: a real, a string or a class handle, which only an
// input port written with no port kind declares a net of (LRM 6.6.7,
// 23.2.2.3).
constexpr DomainSet kValuesNoNetTypeResolves =
    kReals | DomainSet{Domain::kString, Domain::kManagedRef};
// What a net resolving positions holds, and what an unresolved net (LRM 6.6.7)
// holds; a value laid out by its type may be held by either.
constexpr DomainSet kResolvedNetValues = kIntegral | kLaidOutByType;
constexpr DomainSet kUnresolvedNetValues =
    kLaidOutByType | kValuesNoNetTypeResolves;
// What a driver gives: the positions it drives, as one integral value, or the
// whole value of an unresolved net.
constexpr DomainSet kDriverValues = kIntegral | kUnresolvedNetValues;

// What a variable holds besides an integral value and reads and writes
// through an entry: an object of the library's, a real and a reference owning
// nothing, whose domain says what they are where they lie; and, among what a
// variable holds, a value laid out by its type, whose bytes do not.
constexpr DomainSet kHeldThroughEntries =
    DomainsDeclared([](support::LibraryDecl decl) {
      return support::DeclarationOf(decl).is_a_data_type;
    }) |
    kScalarsBesideIntegral | DomainSet{Domain::kAny, Domain::kManagedRef};
constexpr DomainSet kVariableObjects = kHeldThroughEntries | kLaidOutByType;
constexpr DomainSet kVariableValues = kIntegral | kVariableObjects;
// What a body's own variable holds: whatever a variable may, and a run of bits,
// which a streaming operator keeps there while it packs or unpacks one (LRM
// 11.4.14) and which no declaration names.
constexpr DomainSet kBodyObjects =
    kHeldThroughEntries | DomainSet{Domain::kBitVec};
constexpr DomainSet kBodyValues = kVariableValues | DomainSet{Domain::kBitVec};
// What a variable a procedural continuous assignment takes over may hold: a
// singular value (LRM 10.6.1, 10.6.2, 6.4), but no chandle, which no
// continuous assignment may name (LRM 6.14); and, where a caller asks for a
// force the standard forbids, a value laid out by its type, forced whole or in
// part.
constexpr DomainSet kLibraryValuesTakenOver =
    kIntegral | kReals | DomainSet{Domain::kString, Domain::kManagedRef};
constexpr DomainSet kVariableValuesTakenOver =
    kLibraryValuesTakenOver | kLaidOutByType;

constexpr std::array kForReal{RealizedFor{.domains = {Domain::kReal}}};
constexpr std::array kForReals{RealizedFor{.domains = kReals}};
constexpr std::array kForStrings{RealizedFor{.domains = {Domain::kString}}};
// What an entry answers a change of bits, `===` and an unknown bit for: a
// value no other type parameterizes, whose operations the library states
// whole. An integral value the target answers itself, and one laid out by its
// type that type's own operation answers.
constexpr std::array kForWhatAnEntryComparesWhole{
    RealizedFor{.domains = kValuesComparedWhole}};
constexpr std::array kForBitstreams{
    RealizedFor{.domains = DomainSet{Domain::kString, Domain::kManagedRef}}};
// What answers its stream as a run of bits as long as the value makes it: a
// string. A class handle's stream is the object's, which the library does not
// build.
constexpr std::array kForRunsOfBits{
    RealizedFor{.domains = DomainSet{Domain::kString}}};
// An entry over a value laid out by its type -- a fixed-size array built of
// its elements, and a step into an element of an array, on a reference or
// within a write in progress, leased or not -- told that value's type after
// its operands, as every value laid out by its type is.
constexpr std::array kForValuesLaidOutByType{
    RealizedFor{.domains = kLaidOutByType}};
// A step into a component of a designated record, and a reference to one, is
// told the record's type, as any realization for one is; so is a step into a
// member of a union, whose write keeps the union whole from before it.
constexpr std::array kForComponentsSteppedInto{
    RealizedFor{.domains = kLaidOutByType}};
constexpr std::array kForMembersSteppedInto{
    RealizedFor{.domains = kLaidOutByType}};
// A variable's cell a reference is built over keeps what it holds where it
// lies, so it is told nothing of its type.
constexpr std::array kForVariables{
    RealizedFor{.domains = kVariableValues, .told = ToldNothing{}}};
// What the library states a value type of, which a container it holds reads:
// every value it holds as an object of its own, a real and an address.
constexpr std::array kForStatedValueTypes{
    RealizedFor{.domains = kLibraryObjects | kScalarsBesideIntegral}};
// What an entry copies, moves and writes into whole: an object of the
// library's own. A value laid out by its type is carried through it by code
// compiled for the type, and a real and an address are their bytes.
constexpr std::array kForWhatAnEntryCopies{
    RealizedFor{.domains = kLibraryObjects}};

// A history is filled with a default, which fixes how wide its values are.
constexpr std::array kForHistoriesInstalled{
    RealizedFor{
        .domains = kOneWordIntegral | kLaidOutByType | kReals |
                   DomainSet{Domain::kString, Domain::kManagedRef}},
    RealizedFor{
        .domains = kWideIntegral, .told = ToldWidthMetadata{.after = 1}}};
// A history keeps the type of the default it was filled with, so what is
// pushed into it and read back out is told nothing more.
constexpr std::array kForHistories{
    RealizedFor{
        .domains = kIntegral | kReals |
                   DomainSet{Domain::kString, Domain::kManagedRef}},
    RealizedFor{.domains = kLaidOutByType, .told = ToldNothing{}}};
// A net was told its type where it was built -- an integral one its width, one
// laid out by its value's type that type -- and one of a value the library
// holds whole knows it, so none is told it again where it is installed or,
// with no resolution function, where its one driver attaches.
constexpr std::array kForResolvedNets{
    RealizedFor{.domains = kIntegral},
    RealizedFor{.domains = kLaidOutByType, .told = ToldNothing{}}};
constexpr std::array kForUnresolvedNets{
    RealizedFor{.domains = kUnresolvedNetValues, .told = ToldNothing{}}};

// A variable is taken over whether reached through its cell or through storage
// lent for it. A takeover through a reference acts on the variable the
// reference names, whose cell knows its own type, so it is told nothing of a
// value laid out by its type.
constexpr std::array kTakeovers{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell,
        .realized = {.domains = kVariableValuesTakenOver}},
    ThroughWrapper{
        .wrapper = WrapperKind::kResolvedNet,
        .realized = {.domains = kResolvedNetValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kUnresolvedNet,
        .realized = {.domains = kUnresolvedNetValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .realized = {
            .domains = kVariableValuesTakenOver, .told = ToldNothing{}}}};
// Beginning one on a value laid out by its type is told how the type takes
// some of its positions, which the takeover shows what it holds by, except on
// a net resolving positions, which was told how to write them where it was
// built.
constexpr std::array kTakeoversBegun{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell,
        .realized = {.domains = kLibraryValuesTakenOver}},
    ThroughWrapper{
        .wrapper = WrapperKind::kCell,
        .realized = {.domains = kLaidOutByType, .told = ToldTakePositions{}}},
    ThroughWrapper{
        .wrapper = WrapperKind::kResolvedNet,
        .realized = {.domains = kResolvedNetValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kUnresolvedNet,
        .realized = {.domains = kValuesNoNetTypeResolves}},
    ThroughWrapper{
        .wrapper = WrapperKind::kUnresolvedNet,
        .realized = {.domains = kLaidOutByType, .told = ToldTakePositions{}}},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .realized =
            {.domains = kLibraryValuesTakenOver, .told = ToldNothing{}}},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .realized = {.domains = kLaidOutByType, .told = ToldTakePositions{}}}};
// A variable's cell is installed with its declaration's value. A cell of words
// was told its width where it was built, and one of a value laid out by its
// type that type.
constexpr std::array kInstalls{ThroughWrapper{
    .wrapper = WrapperKind::kCell, .realized = {.domains = kBodyValues}}};
// A cell holds the bytes of a value no wider than a word where generated code
// reads them, and the address of any other value of its own laying it out after
// itself; a net keeps the address of what it shows, whatever that is laid out
// by. So only a cell of an object the library keeps has an entry reading one,
// and so does the one driver of an unresolved net. A reference read, or
// sampled, answers where what it names lies, so it is told nothing of it.
constexpr std::array kLoads{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell, .realized = {.domains = kBodyObjects}},
    ThroughWrapper{
        .wrapper = WrapperKind::kDriver,
        .realized = {.domains = kUnresolvedNetValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .realized = {.domains = kVariableValues, .told = ToldNothing{}}}};
// What a net is told of the object its driver carries, and asked at that
// driver's turn.
constexpr std::array kDrivesFromAnObject{
    ThroughWrapper{
        .wrapper = WrapperKind::kResolvedNet,
        .realized = {.domains = kResolvedNetValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kUnresolvedNet,
        .realized = {.domains = kUnresolvedNetValues}}};
// Where a wait on what a wrapper stands for enrols: a variable is where its
// writes are told, a net keeps the member it shows its value through where its
// value's type puts it, and a reference names the member it was bound into.
constexpr std::array kWaitedOn{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell,
        .realized = {.domains = kVariableValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kResolvedNet,
        .realized = {.domains = kResolvedNetValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kUnresolvedNet,
        .realized = {.domains = kUnresolvedNetValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .realized = {.domains = kVariableValues, .told = ToldNothing{}}}};
// What keeps a sample of what it holds (LRM 16.5.1): armed to take one, and
// read for the one it took.
constexpr std::array kSampled{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell,
        .realized = {.domains = kVariableValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kResolvedNet,
        .realized = {.domains = kResolvedNetValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kUnresolvedNet,
        .realized = {.domains = kUnresolvedNetValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .realized = {.domains = kVariableValues, .told = ToldNothing{}}}};
constexpr ThroughWrapper kThroughAReferenceToBytesOrAnObject{
    .wrapper = WrapperKind::kRef,
    .realized = {.domains = kOneWordIntegral | kHeldThroughEntries}};
// A reference to a value laid out by its type names bytes lying in something
// else, which say nothing of what they are, so an access that replaces the
// value or opens a write on it is told the value's type, as any realization
// for such a value is.
constexpr ThroughWrapper kThroughAReferenceToALaidOutValue{
    .wrapper = WrapperKind::kRef, .realized = {.domains = kLaidOutByType}};
// A reference to a value wider than a word names words lying in something
// else, so an access through one that takes or replaces them is told how wide
// they are.
constexpr ThroughWrapper kThroughAReferenceToWords{
    .wrapper = WrapperKind::kRef,
    .realized = {
        .domains = kWideIntegral, .told = ToldWidthMetadata{.after = 0}}};
constexpr std::array kStores{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell, .realized = {.domains = kBodyValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kDriver,
        .realized = {.domains = kDriverValues}},
    kThroughAReferenceToBytesOrAnObject, kThroughAReferenceToWords,
    kThroughAReferenceToALaidOutValue};
// A driver of a net resolving positions gives the whole of the part it drives,
// so only the one driver of an unresolved net is written into.
constexpr std::array kOpenedWrites{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell, .realized = {.domains = kBodyValues}},
    ThroughWrapper{
        .wrapper = WrapperKind::kDriver,
        .realized = {.domains = kUnresolvedNetValues}},
    kThroughAReferenceToBytesOrAnObject, kThroughAReferenceToWords,
    kThroughAReferenceToALaidOutValue};

constexpr std::array kRealConversions{
    Conversion{.destination = Domain::kReal, .source = Domain::kReal},
    Conversion{.destination = Domain::kReal, .source = Domain::kShortReal},
    Conversion{.destination = Domain::kShortReal, .source = Domain::kReal}};

// A cell the library makes -- what a suspending call answers into, or one a
// counted hold keeps -- is as large as the value it is to hold, so one of
// words is made at their width, and one of a value laid out by its type with
// that type.
constexpr std::array kForCellsMade{
    RealizedFor{.domains = kOneWordIntegral | kHeldThroughEntries},
    RealizedFor{.domains = kWideIntegral, .told = ToldWidthMetadataAhead{}},
    RealizedFor{.domains = kLaidOutByType, .told = ToldTypeMetadataAhead{}}};
// A cell whose value lies where the library states is read and written there,
// so only an object of the library's own is read and written through an entry.
constexpr std::array kForCellsReached{
    RealizedFor{.domains = kHeldThroughEntries}};

// A designation of words is told how wide they are where the write lands.
constexpr std::array kForLandings{
    RealizedFor{.domains = kOneWordIntegral | kBodyObjects | kLaidOutByType},
    RealizedFor{
        .domains = kWideIntegral, .told = ToldWidthMetadata{.after = 0}}};
// What a write designates is told the value's type, by which what a run of its
// elements may change is kept before the run is written. What moved is told
// the same way whatever the value is, so nothing of its type is told then.
constexpr std::array kForDesignatedParts{
    RealizedFor{.domains = kLaidOutByType}};
constexpr std::array kForDesignatedRuns{
    RealizedFor{.domains = kLaidOutByType, .told = ToldNothing{}}};
// A designation of any integral value is told how wide it is where a write of
// some of its bits is reported (LRM 11.5.1), which holds the bits written to
// the value's width.
constexpr std::array kForBitsWritten{
    RealizedFor{.domains = kIntegral, .told = ToldWidthMetadata{.after = 0}}};

// The values the types the library declares start as, each realized for the
// domains whose declarations name its entry for it, and the null handle a
// class handle referring to nothing is.
constexpr std::array kForDefaults{
    RealizedFor{.domains = DomainsBuiltBy(support::BuiltinFn::kDefault)}};
constexpr std::array kForNullHandles{
    RealizedFor{.domains = {Domain::kManagedRef}}};
// What is read as true where it refers to an object and false where it is
// null (LRM 12.4): a class handle.
constexpr std::array kForHandlesReadAsConditions{
    RealizedFor{.domains = {Domain::kManagedRef}}};
constexpr std::array kForWhatFormatsItself{RealizedFor{
    .domains = kScalarsBesideIntegral |
               DomainSet{Domain::kString, Domain::kManagedRef}}};
// The constructor a declaration names, realized for the domains of the
// declarations naming it: a value built from the host text it holds.
constexpr std::array kForWhatTextBuilds{RealizedFor{
    .domains = DomainsBuiltBy(support::BuiltinFn::kStringFromText)}};
constexpr std::array kForWhatEndsWithWork{
    RealizedFor{.domains = kLibraryObjects}};

// What refusing a realization the library does not hold says.
auto Refusal(std::string_view entry, std::string_view realization)
    -> std::string {
  return std::format(
      "llvm codegen: the library realizes no {} for {} -- please report this "
      "as a bug",
      entry, realization);
}

// The realizations of an entry named by one value domain, none for an entry
// named any other way.
auto PerDomain(const EntryNaming& naming) -> std::span<const RealizedFor> {
  using Realized = std::span<const RealizedFor>;
  return std::visit(
      Overloaded{
          [](const NamedByValue& named) -> Realized { return named.realized; },
          [](const NamedByResult& named) -> Realized { return named.realized; },
          [](const NamedByStorageDomain& named) -> Realized {
            return named.realized;
          },
          [](const NamedAlone&) -> Realized { return {}; },
          [](const NamedByWrapper&) -> Realized { return {}; },
          [](const NamedByConversion&) -> Realized { return {}; },
          [](const OverIntegralValues&) -> Realized { return {}; },
          [](const NotRealized&) -> Realized { return {}; }},
      naming);
}

// The same for an entry named by a capability wrapper, and for a conversion.
auto ThroughWrappers(const EntryNaming& naming)
    -> std::span<const ThroughWrapper> {
  using Realized = std::span<const ThroughWrapper>;
  return std::visit(
      Overloaded{
          [](const NamedByWrapper& named) -> Realized {
            return named.realized;
          },
          [](const NamedAlone&) -> Realized { return {}; },
          [](const NamedByValue&) -> Realized { return {}; },
          [](const NamedByResult&) -> Realized { return {}; },
          [](const NamedByStorageDomain&) -> Realized { return {}; },
          [](const NamedByConversion&) -> Realized { return {}; },
          [](const OverIntegralValues&) -> Realized { return {}; },
          [](const NotRealized&) -> Realized { return {}; }},
      naming);
}

auto Conversions(const EntryNaming& naming) -> std::span<const Conversion> {
  using Realized = std::span<const Conversion>;
  return std::visit(
      Overloaded{
          [](const NamedByConversion& named) -> Realized {
            return named.realized;
          },
          [](const NamedAlone&) -> Realized { return {}; },
          [](const NamedByValue&) -> Realized { return {}; },
          [](const NamedByResult&) -> Realized { return {}; },
          [](const NamedByWrapper&) -> Realized { return {}; },
          [](const NamedByStorageDomain&) -> Realized { return {}; },
          [](const OverIntegralValues&) -> Realized { return {}; },
          [](const NotRealized&) -> Realized { return {}; }},
      naming);
}

// What a realization for `domain` is told, given what its entry states of
// it. A realization for a value laid out by its type its entry states nothing
// of is told that type, since no such value's bytes state one.
auto ToldOfTheDomain(
    support::ValueDomain domain, const std::optional<Told>& stated) -> Told {
  if (stated.has_value()) {
    return *stated;
  }
  if (support::IsLaidOutByItsType(domain)) {
    return ToldLaidOutType{};
  }
  return ToldNothing{};
}

// The same for a realization reached through `wrapper`. A cell, a net and a
// driver laying a value out after themselves by its type were told that type
// where they were built, so one is told nothing of the values it is handed; a
// reference holds no type and is told it like any other realization.
auto ToldThroughTheWrapper(
    support::ValueDomain domain, WrapperKind wrapper,
    const std::optional<Told>& stated) -> Told {
  switch (wrapper) {
    case WrapperKind::kCell:
    case WrapperKind::kResolvedNet:
    case WrapperKind::kUnresolvedNet:
    case WrapperKind::kDriver:
      if (support::IsLaidOutByItsType(domain) && !stated.has_value()) {
        return ToldNothing{};
      }
      return ToldOfTheDomain(domain, stated);
    case WrapperKind::kRef:
      return ToldOfTheDomain(domain, stated);
  }
  throw InternalError("llvm codegen: unknown capability wrapper");
}

auto ToldIn(
    std::span<const RealizedFor> realized, support::ValueDomain domain,
    std::string_view entry) -> Told {
  for (const RealizedFor& one : realized) {
    if (one.domains.Holds(domain)) {
      return ToldOfTheDomain(domain, one.told);
    }
  }
  throw InternalError(Refusal(entry, support::ValueDomainName(domain)));
}

}  // namespace

auto ToldOf(support::ValueDomain domain, RuntimeOp op) -> Told {
  return ToldIn(RealizationsOf(op), domain, RuntimeOpName(op));
}

auto ToldOf(support::ValueDomain domain, lir::ValueCellTarget::Op op) -> Told {
  return ToldIn(RealizationsOf(op), domain, lir::ValueCellOpName(op));
}

auto ToldOf(support::ValueDomain domain, lir::OpenWriteTarget::Op op) -> Told {
  return ToldIn(RealizationsOf(op), domain, lir::OpenWriteOpName(op));
}

auto ToldOf(support::ValueDomain domain, support::BuiltinFn fn) -> Told {
  return ToldIn(
      PerDomain(EntryNamingOf(fn)), domain, support::RuntimeEntryOf(fn).name);
}

auto ToldOf(
    support::ValueDomain domain, WrapperKind wrapper, support::BuiltinFn fn)
    -> Told {
  const EntryNaming naming = EntryNamingOf(fn);
  for (const ThroughWrapper& one : ThroughWrappers(naming)) {
    if (one.wrapper == wrapper && one.realized.domains.Holds(domain)) {
      return ToldThroughTheWrapper(domain, wrapper, one.realized.told);
    }
  }
  throw InternalError(Refusal(
      support::RuntimeEntryOf(fn).name,
      std::format(
          "{} through a {}", support::ValueDomainName(domain),
          WrapperName(wrapper))));
}

auto ToldOfAnOperator(support::ValueDomain domain) -> Told {
  return ToldOfTheDomain(domain, std::nullopt);
}

auto ToldBuilding(support::DeclaredMemberStorage storage) -> Told {
  // A member's cell is built told what a cell the library makes is, and a cell
  // of a value no such cell holds is told nothing.
  const auto cell = [](support::ValueDomain domain) -> Told {
    for (const RealizedFor& one : kForCellsMade) {
      if (one.domains.Holds(domain)) {
        return ToldOfTheDomain(domain, one.told);
      }
    }
    return ToldNothing{};
  };
  // A history is installed by an entry of its own, and every other storage
  // holds no value it lays out after itself.
  const auto nothing = [](const auto&) -> Told { return ToldNothing{}; };
  return std::visit(
      Overloaded{
          // A net of an integral type is told how many positions the type
          // has, which a layout of a narrower one does not say, and a net of a
          // value laid out by its type that type and how its positions are
          // read and written.
          [](const support::ResolvedNetMember& m) -> Told {
            if (kIntegral.Holds(m.domain)) {
              return ToldWidthMetadataAhead{};
            }
            if (support::IsLaidOutByItsType(m.domain)) {
              return ToldTypeAndNetWitnessesAhead{};
            }
            return ToldNothing{};
          },
          // An unresolved net is never integral, and one laid out by its type
          // reads and writes no positions, so it is told only the type.
          [](const support::UnresolvedNetMember& m) -> Told {
            if (support::IsLaidOutByItsType(m.domain)) {
              return ToldTypeMetadataAhead{};
            }
            return ToldNothing{};
          },
          [&](const support::ValueCellMember& m) { return cell(m.domain); },
          [&](const support::ObservableCellMember& m) {
            return cell(m.domain);
          },
          [&](const support::SampledHistoryMember& m) { return nothing(m); },
          [&](const support::InlineValueMember& m) { return nothing(m); },
          [&](const support::InlineBytesMember& m) { return nothing(m); },
          [&](const support::BorrowedHandleMember& m) { return nothing(m); },
          [&](const support::LibraryDeclMember& m) { return nothing(m); },
          [&](const support::LibraryObjectMember& m) { return nothing(m); }},
      storage);
}

auto RealizationsOf(RuntimeOp op) -> std::span<const RealizedFor> {
  switch (op) {
    case RuntimeOp::kCellRefer:
      return kForVariables;
    case RuntimeOp::kSharedCellMake:
      return kForCellsMade;
    case RuntimeOp::kDefault:
      return kForNullHandles;
    case RuntimeOp::kFromElements:
      return kForValuesLaidOutByType;
    case RuntimeOp::kToBool:
      return kForHandlesReadAsConditions;
    case RuntimeOp::kCopy:
    case RuntimeOp::kRelocate:
    case RuntimeOp::kAssign:
      return kForWhatAnEntryCopies;
    case RuntimeOp::kDestroy:
      return kForWhatEndsWithWork;
    case RuntimeOp::kInsertDesignatedBits:
      return kForBitsWritten;
    // Realized once, or per object or member storage and never per value
    // domain alone. No value library realizes a run of elements replaced in a
    // value: a write of one goes through the write's own slice write.
    case RuntimeOp::kReferStorage:
    case RuntimeOp::kRunProgram:
    case RuntimeOp::kSequenceMake:
    case RuntimeOp::kSequenceElement:
    case RuntimeOp::kClosureBoxEnd:
    case RuntimeOp::kObjectAdopt:
    case RuntimeOp::kSharedPointerDeref:
    case RuntimeOp::kInterfaceTable:
    case RuntimeOp::kBeginLibraryPart:
    case RuntimeOp::kEndLibraryPart:
    case RuntimeOp::kSettleDeparture:
    case RuntimeOp::kStreamWrite:
    case RuntimeOp::kStreamBuild:
    case RuntimeOp::kRunWrite:
    case RuntimeOp::kRunBuild:
    case RuntimeOp::kPositionsWrite:
    case RuntimeOp::kInsertBits:
    case RuntimeOp::kElementDefault:
    case RuntimeOp::kDiscardedWrite:
    case RuntimeOp::kConstruct:
      return {};
  }
  throw InternalError("llvm codegen: unknown runtime operation");
}

auto RealizationsOf(lir::ValueCellTarget::Op op)
    -> std::span<const RealizedFor> {
  switch (op) {
    case lir::ValueCellTarget::Op::kAllocate:
      return kForCellsMade;
    case lir::ValueCellTarget::Op::kLoad:
    case lir::ValueCellTarget::Op::kStore:
      return kForCellsReached;
  }
  throw InternalError("llvm codegen: unknown value cell operation");
}

auto RealizationsOf(lir::OpenWriteTarget::Op op)
    -> std::span<const RealizedFor> {
  switch (op) {
    case lir::OpenWriteTarget::Op::kLand:
      return kForLandings;
    case lir::OpenWriteTarget::Op::kDesignated:
      return kForDesignatedParts;
    case lir::OpenWriteTarget::Op::kMoved:
      return kForDesignatedRuns;
  }
  throw InternalError("llvm codegen: unknown open-write operation");
}

auto EntryNamingOf(support::BuiltinFn fn) -> EntryNaming {
  constexpr std::string_view kLeavesByUnwinding =
      "leaves a body by unwinding it, where this target's bodies leave by "
      "branching";
  constexpr std::string_view kErasedWhereItLies =
      "makes a closure one of no known type, which this target's closure is "
      "where it lies, its definition opening it";
  constexpr std::string_view kReadWhereItLies =
      "takes a value out of storage, which this target reads where it lies, "
      "the storage owing no end after it";
  constexpr std::string_view kAnsweredForTheType =
      "answers where a position naming no element reads or writes, which "
      "this target asks the library of the element type the call is at";
  constexpr std::string_view kEmittedForTheType =
      "answers where the record of the type the call is at lies, which this "
      "target emits itself";
  constexpr std::string_view kWrittenWhereTheBitsLie =
      "answers a reference to bits, which this target writes through by a "
      "store of the bits where they lie";
  constexpr std::string_view kTheBodyOfItsType =
      "is an operation of an array whose elements a position or an index "
      "names, which is the body its type has, or a step into one, which this "
      "target takes where its elements lie";
  constexpr std::string_view kReadOffTheCore =
      "reads what a dynamic array's core publishes, which this target reads "
      "where it lies";
  constexpr std::string_view kReadAtTheQuery =
      "is read off the array's first or last index where the query is "
      "written, so no call of it reaches this target";
  switch (fn) {
    case support::BuiltinFn::kDynamicArrayPlace:
      return NotRealized{.shape = kReadOffTheCore};
    // LRM 20.7 `$low` / `$high` over an associative dimension.
    case support::BuiltinFn::kAssocMinIndex:
    case support::BuiltinFn::kAssocMaxIndex:
      return NotRealized{.shape = kReadAtTheQuery};
    // What a value starts as, and the constructors, realized per kind of
    // value they build, and per kind of value they are built over: what a
    // value formats as is the value's own answer.
    case support::BuiltinFn::kDefault:
      return NamedByResult{.realized = kForDefaults};
    case support::BuiltinFn::kStringFromText:
      return NamedByResult{.realized = kForWhatTextBuilds};
    case support::BuiltinFn::kPrintItemMake:
    case support::BuiltinFn::kFormatArgMake:
      return NamedByValue{.realized = kForWhatFormatsItself};
    // The operations over integral values alone (LRM 11.4, 11.5.1, 20.9).
    case support::BuiltinFn::kReverseBlocks:
    case support::BuiltinFn::kClog2:
    case support::BuiltinFn::kBitwiseXnor:
    case support::BuiltinFn::kWildcardEquals:
    case support::BuiltinFn::kCasezEquals:
    case support::BuiltinFn::kCasexEquals:
    case support::BuiltinFn::kReductionAnd:
    case support::BuiltinFn::kReductionOr:
    case support::BuiltinFn::kReductionXor:
    case support::BuiltinFn::kReductionNand:
    case support::BuiltinFn::kReductionNor:
    case support::BuiltinFn::kReductionXnor:
    case support::BuiltinFn::kToInt64:
    case support::BuiltinFn::kLogicalEquivalence:
    case support::BuiltinFn::kShiftLeft:
    case support::BuiltinFn::kLogicalShiftRight:
    case support::BuiltinFn::kArithmeticShiftRight:
    case support::BuiltinFn::kReplicateBits:
    case support::BuiltinFn::kSlice:
    case support::BuiltinFn::kWithSlice:
    case support::BuiltinFn::kFromBool:
    case support::BuiltinFn::kToPosition:
    case support::BuiltinFn::kIntegralFromString:
    case support::BuiltinFn::kFromSvLogic:
    case support::BuiltinFn::kReadCanonicalBitVec:
    case support::BuiltinFn::kReadCanonicalLogicVec:
    case support::BuiltinFn::kIntegralPow:
    case support::BuiltinFn::kIntegralCaseEqual:
    case support::BuiltinFn::kIntegralBitIdentical:
    case support::BuiltinFn::kIntegralHasUnknown:
    case support::BuiltinFn::kIntegralCountBits:
    case support::BuiltinFn::kIntegralTakePositions:
    case support::BuiltinFn::kIntegralWithPositions:
    case support::BuiltinFn::kIntegralMergeConditional:
    case support::BuiltinFn::kIntegralFromInt:
    case support::BuiltinFn::kIntegralConvert:
      return OverIntegralValues{};

    // LRM 7.6 assignment between unpacked array kinds, and every operation of
    // an array whose elements a position or an index names (LRM 7.9): the
    // body its type has.
    case support::BuiltinFn::kExists:
    case support::BuiltinFn::kAssocFirst:
    case support::BuiltinFn::kAssocLast:
    case support::BuiltinFn::kAssocNext:
    case support::BuiltinFn::kAssocPrev:
    case support::BuiltinFn::kAssocStatedDefault:
    case support::BuiltinFn::kAssocSetStatedDefault:
    case support::BuiltinFn::kAssocElement:
    case support::BuiltinFn::kAssocElementMut:
    case support::BuiltinFn::kAssocDeleteIndex:
    case support::BuiltinFn::kDelete:
    case support::BuiltinFn::kUnpackedArrayFromArray:
    case support::BuiltinFn::kReverse:
    case support::BuiltinFn::kWindowToBitVec:
    case support::BuiltinFn::kWithWindow:
    case support::BuiltinFn::kFromBitVec:
    case support::BuiltinFn::kQueueFromArray:
    case support::BuiltinFn::kQueueSlice:
    case support::BuiltinFn::kAssignQueueSlice:
    case support::BuiltinFn::kQueueAllocate:
    case support::BuiltinFn::kQueueHoldToBound:
    case support::BuiltinFn::kInsert:
    case support::BuiltinFn::kPopFront:
    case support::BuiltinFn::kPopBack:
    case support::BuiltinFn::kPushFront:
    case support::BuiltinFn::kPushBack:
    case support::BuiltinFn::kDeleteIndex:
    case support::BuiltinFn::kElementSlice:
    case support::BuiltinFn::kAssignElementSlice:
    case support::BuiltinFn::kApplyOrder:
    case support::BuiltinFn::kSortOrder:
    case support::BuiltinFn::kLoadWords:
    case support::BuiltinFn::kStoreWords:
    case support::BuiltinFn::kToImage:
    case support::BuiltinFn::kFromImage:
    case support::BuiltinFn::kWalkRandom:
    case support::BuiltinFn::kDynamicArrayFromArray:
    case support::BuiltinFn::kDynamicArrayNew:
    case support::BuiltinFn::kDynamicArrayNewCopy:
    case support::BuiltinFn::kResized:
      return NotRealized{.shape = kTheBodyOfItsType};
    // The lowering opens the designation for the part, calls the array type's
    // own slice write on it, and tells the write what moved.
    case support::BuiltinFn::kAssignDesignatedRun:
      return NotRealized{.shape = kTheBodyOfItsType};

    // An element of a fixed-size unpacked array lies where arithmetic on its
    // layout says, and one of a dynamic array or a queue where the body its
    // type has for the step says, which may append one to a queue (LRM
    // 7.10.1), so no entry is asked for one, to read it or to write it,
    // whether the index is a position the program computed or a machine index
    // a body counted below the array's own count.
    case support::BuiltinFn::kElement:
    case support::BuiltinFn::kElementMut:
    case support::BuiltinFn::kQueueElementMut:
    case support::BuiltinFn::kElementPlace:
    case support::BuiltinFn::kGetUnchecked:
    case support::BuiltinFn::kGetUncheckedMut:
      return NotRealized{.shape = kTheBodyOfItsType};

    case support::BuiltinFn::kSize:
      return NamedByValue{.realized = kForValuesLaidOutByType};

    case support::BuiltinFn::kLn:
    case support::BuiltinFn::kLog10:
    case support::BuiltinFn::kExp:
    case support::BuiltinFn::kSqrt:
    case support::BuiltinFn::kFloor:
    case support::BuiltinFn::kCeil:
    case support::BuiltinFn::kSin:
    case support::BuiltinFn::kCos:
    case support::BuiltinFn::kTan:
    case support::BuiltinFn::kAsin:
    case support::BuiltinFn::kAcos:
    case support::BuiltinFn::kAtan:
    case support::BuiltinFn::kAtan2:
    case support::BuiltinFn::kHypot:
    case support::BuiltinFn::kSinh:
    case support::BuiltinFn::kCosh:
    case support::BuiltinFn::kTanh:
    case support::BuiltinFn::kAsinh:
    case support::BuiltinFn::kAcosh:
    case support::BuiltinFn::kAtanh:
    case support::BuiltinFn::kTruncate:
      return NamedByValue{.realized = kForReal};
    case support::BuiltinFn::kToIntegral:
    case support::BuiltinFn::kToBits:
    case support::BuiltinFn::kPow:
      return NamedByValue{.realized = kForReals};

    case support::BuiltinFn::kLen:
    case support::BuiltinFn::kGetc:
    case support::BuiltinFn::kPutc:
    case support::BuiltinFn::kToupper:
    case support::BuiltinFn::kTolower:
    case support::BuiltinFn::kCompare:
    case support::BuiltinFn::kIcompare:
    case support::BuiltinFn::kSubstr:
    case support::BuiltinFn::kAtoi:
    case support::BuiltinFn::kAtohex:
    case support::BuiltinFn::kAtooct:
    case support::BuiltinFn::kAtobin:
    case support::BuiltinFn::kAtoreal:
    case support::BuiltinFn::kItoa:
    case support::BuiltinFn::kHextoa:
    case support::BuiltinFn::kOcttoa:
    case support::BuiltinFn::kBintoa:
    case support::BuiltinFn::kRealtoa:
    case support::BuiltinFn::kScanString:
      return NamedByValue{.realized = kForStrings};

    // A real is handed over at the width it is held at.
    case support::BuiltinFn::kSerializeReal:
      return NamedByValue{.operand = 1, .realized = kForReals};

    case support::BuiltinFn::kBitIdentical:
    case support::BuiltinFn::kCaseEqual:
    case support::BuiltinFn::kHasUnknown:
      return NamedByValue{.realized = kForWhatAnEntryComparesWhole};
    case support::BuiltinFn::kBitstreamWidth:
    case support::BuiltinFn::kCountBits:
      return NamedByValue{.realized = kForBitstreams};
    case support::BuiltinFn::kToBitVec:
      return NamedByValue{.realized = kForRunsOfBits};
    // What a value laid out by its type answers -- a structure, a union, a
    // fixed-size array -- which code compiled for the type answers, and
    // nothing the library holds does; an integral value's is its own entry.
    case support::BuiltinFn::kToBitstream:
    case support::BuiltinFn::kMergeConditional:
    case support::BuiltinFn::kTakePositions:
    case support::BuiltinFn::kToPositions:
    case support::BuiltinFn::kWritePositions:
    // A value holding a bounded queue below its outermost dimension, taken
    // into the form of its type with no queue bounded, which the unit's body
    // for the type answers whatever the outer kind.
    case support::BuiltinFn::kToBoundFree:
      return NamedByValue{.realized = {}};
    // What a structure's declaration states, which the structure's own
    // generated body answers and nothing the library holds does.
    case support::BuiltinFn::kDeclaredDefault:
    case support::BuiltinFn::kPositionCount:
    case support::BuiltinFn::kFromPositions:
    // A value built out of the form of its type with no queue bounded, which
    // the unit's body for the type it builds answers.
    case support::BuiltinFn::kFromBoundFree:
      return NamedByResult{.realized = {}};

    // The factories. Each builds a value out of operands that are the material
    // of one -- a machine integer, a byte array, a list of the parts of a join
    // -- so none of them carries the representation the entry is realized for,
    // and only what the call answers with does.
    case support::BuiltinFn::kFromByteArray:
    case support::BuiltinFn::kConcatStrings:
      return NamedByResult{.realized = kForStrings};
    case support::BuiltinFn::kFromBits:
    case support::BuiltinFn::kFromIntegral:
      return NamedByResult{.realized = kForReals};

    // LRM 6.12.1: a real of one precision as a real of the other, whose
    // realization depends on both.
    case support::BuiltinFn::kConvertFrom:
      return NamedByConversion{.realized = kRealConversions};

    // The accesses a capability wrapper defines. Which wrapper the call acts
    // on decides both which family answers and which representation it answers
    // in, so neither half is the call's own. Arming a cell and reading what it
    // retained (LRM 16.5.1) are two more of them: what a sampled read yields is
    // the wrapper's own decision about which of the two values it holds to
    // answer with, which is the same operation on the same storage as an
    // ordinary read.
    case support::BuiltinFn::kInitialize:
      return NamedByWrapper{.realized = kInstalls};
    case support::BuiltinFn::kGet:
      return NamedByWrapper{.realized = kLoads};
    case support::BuiltinFn::kSampledGet:
      return NamedByWrapper{.realized = kSampled};
    case support::BuiltinFn::kArmSampling:
      return NamedByWrapper{.realized = kSampled};
    // A takeover acts on the cell it covers and carries that cell's value, so
    // the wrapper it is reached through is what names the entry (LRM 10.6).
    case support::BuiltinFn::kBeginTakeover:
      return NamedByWrapper{.realized = kTakeoversBegun};
    case support::BuiltinFn::kDriveTakeover:
    case support::BuiltinFn::kEndTakeover:
      return NamedByWrapper{.realized = kTakeovers};
    // Opening a write reaches the contents of the wrapper it is opened on, and
    // what the write reports when it ends is that wrapper's own business.
    case support::BuiltinFn::kSet:
      return NamedByWrapper{.realized = kStores};
    case support::BuiltinFn::kOpenForWrite:
      return NamedByWrapper{.realized = kOpenedWrites};
    // A net whose driver carries the whole of an object of the net's type is
    // told so, and asked at that driver's turn whether it now shows the
    // object; the net, of the object's own type, names the entry.
    case support::BuiltinFn::kDriveFromVariable:
    case support::BuiltinFn::kDriveFromNet:
      return NamedByWrapper{.realized = kDrivesFromAnObject};
    // What a reference names is told to the net by its address, and whether
    // the net shows what it was told is the net's own answer, so neither
    // depends on what the net holds.
    case support::BuiltinFn::kDriveFromReference:
    case support::BuiltinFn::kShowSource:
      return NamedAlone{};
    // Each wrapper answers where a wait on it enrols, so the wrapper names the
    // entry.
    case support::BuiltinFn::kReportsTo:
      return NamedByWrapper{.realized = kWaitedOn};

    // A driver of a net resolving positions is attached over positions, and a
    // join relates positions of two nets, which carry no type, so one
    // realization of each serves any such net. The one driver of an
    // unresolved net is attached by that net, and a fold is installed on the
    // net that applies it, so in each case what names the entry is the
    // representation that net holds. A history's three operations
    // likewise take the storage they act on and are named by the one domain
    // every value in it is realized in (LRM 16.9.3).
    case support::BuiltinFn::kAttachDriver:
    case support::BuiltinFn::kNetJoin:
      return NamedAlone{};
    case support::BuiltinFn::kAttachWholeDriver:
      return NamedByStorageDomain{.realized = kForUnresolvedNets};
    case support::BuiltinFn::kNetInitializeTriState:
    case support::BuiltinFn::kNetInitializeWiredAnd:
    case support::BuiltinFn::kNetInitializeWiredOr:
    case support::BuiltinFn::kNetInitializeRetaining:
      return NamedByStorageDomain{.realized = kForResolvedNets};
    case support::BuiltinFn::kUnresolvedNetInitialize:
      return NamedByStorageDomain{.realized = kForUnresolvedNets};
    case support::BuiltinFn::kSampledHistoryInstall:
      return NamedByStorageDomain{.realized = kForHistoriesInstalled};
    case support::BuiltinFn::kSampledHistoryPush:
    case support::BuiltinFn::kSampledHistoryAt:
      return NamedByStorageDomain{.realized = kForHistories};
    // A step within a write is taken on a designation, and one lending a part
    // on a reference; each is named by the representation of the value
    // designated or referred to there.
    case support::BuiltinFn::kDesignateElement:
    case support::BuiltinFn::kDynamicArrayDesignateElement:
    case support::BuiltinFn::kQueueDesignateElement:
    case support::BuiltinFn::kAssocDesignateElement:
    case support::BuiltinFn::kReferElement:
    case support::BuiltinFn::kDynamicArrayReferElement:
    case support::BuiltinFn::kQueueReferElement:
    case support::BuiltinFn::kAssocReferElement:
      return NamedByStorageDomain{.realized = kForValuesLaidOutByType};
    // A call's binding leases what it steps into only where the program can
    // let it go: an element of a queue, of a dynamic array or of an
    // associative array, whose core is told of the lease; an element of a
    // fixed-size array lies where its array does, and leasing one leases
    // nothing.
    case support::BuiltinFn::kReferElementLeased:
    case support::BuiltinFn::kQueueReferElementLeased:
    case support::BuiltinFn::kAssocReferElementLeased:
      return NamedByStorageDomain{.realized = kForValuesLaidOutByType};
    case support::BuiltinFn::kDesignateComponent:
      return NamedByStorageDomain{.realized = kForComponentsSteppedInto};
    case support::BuiltinFn::kDesignateMember:
      return NamedByStorageDomain{.realized = kForMembersSteppedInto};
    case support::BuiltinFn::kReferComponent:
      return NamedByStorageDomain{.realized = kForComponentsSteppedInto};

    // A body that leaves by branching reaches the gate as the pair of questions
    // the lowering already asks wherever this execution regains control -- and
    // it asks them here in place of this call rather than through an entry of
    // its own.
    case support::BuiltinFn::kTakeDepartureIfDue:
      return NotRealized{.shape = kLeavesByUnwinding};

    // Bits are no one place, so a reference to them is no value the library
    // holds: a write through it is a store of those bits where they lie, or
    // within the write in progress the reference was taken on, which the
    // lowering reaches in place of this call.
    case support::BuiltinFn::kSliceMut:
      return NotRealized{.shape = kWrittenWhereTheBitsLie};

    // The lowering reads the value where it lies and leaves the storage owing
    // no end, in place of this call.
    case support::BuiltinFn::kPtrRead:
      return NotRealized{.shape = kReadWhereItLies};
    // The library's answer for a position naming no element, which this
    // target asks of the element type the call is at.
    case support::BuiltinFn::kElementDefault:
    case support::BuiltinFn::kDiscardedWrite:
      return NotRealized{.shape = kAnsweredForTheType};
    // Where the record of the type the call is at lies, which this target
    // emits itself.
    case support::BuiltinFn::kTypeRecordOf:
      return NotRealized{.shape = kEmittedForTheType};
    // The lowering reads the closure as the erased one in place of this call.
    case support::BuiltinFn::kUnsize:
      return NotRealized{.shape = kErasedWhereItLies};

    // Designating the whole of what a write was opened on reads nothing of the
    // value there, so the library realizes it once.
    case support::BuiltinFn::kDesignateWhole:
    case support::BuiltinFn::kEventNew:
    case support::BuiltinFn::kTrigger:
    case support::BuiltinFn::kTriggered:
    case support::BuiltinFn::kEventOccurrences:
    case support::BuiltinFn::kEventReportsTo:
    case support::BuiltinFn::kCurrentRuntime:
    case support::BuiltinFn::kNbaEffects:
    case support::BuiltinFn::kNbaEffectsAfter:
    case support::BuiltinFn::kNbaEffectsAfterReal:
    case support::BuiltinFn::kRegionEffectsAfter:
    case support::BuiltinFn::kRegionEffectsAfterReal:
    case support::BuiltinFn::kPassEndEffects:
    case support::BuiltinFn::kRunDetached:
    case support::BuiltinFn::kRunObserving:
    case support::BuiltinFn::kResumeInNbaRegion:
    case support::BuiltinFn::kResumeInRegionAfter:
    case support::BuiltinFn::kResumeInRegionAfterReal:
    case support::BuiltinFn::kPostponedEffects:
    case support::BuiltinFn::kSubmitAtRunEnd:
    case support::BuiltinFn::kObservedEffects:
    case support::BuiltinFn::kDynamicArrayConstruct:
    case support::BuiltinFn::kDynamicArrayGeneration:
    case support::BuiltinFn::kDynamicArrayAdopt:
    case support::BuiltinFn::kDynamicArrayLetGo:
    case support::BuiltinFn::kDynamicArrayFree:
    case support::BuiltinFn::kQueueConstruct:
    case support::BuiltinFn::kQueueReserve:
    case support::BuiltinFn::kQueueCommit:
    case support::BuiltinFn::kQueueFreeRing:
    case support::BuiltinFn::kQueueAdopt:
    case support::BuiltinFn::kQueueReverse:
    case support::BuiltinFn::kRecyclerAllocate:
    case support::BuiltinFn::kQueueGrewPastItsBound:
    case support::BuiltinFn::kQueueWrittenAtAnInvalidIndex:
    case support::BuiltinFn::kAssociativeArrayReadAtAnInvalidIndex:
    case support::BuiltinFn::kAssociativeArrayReadAtAMissingEntry:
    case support::BuiltinFn::kAssociativeArrayWrittenAtAnInvalidIndex:
    case support::BuiltinFn::kDefaultPlacementNew:
    case support::BuiltinFn::kQueueUnlink:
    case support::BuiltinFn::kQueueApplyOrder:
    case support::BuiltinFn::kAssociativeArrayConstruct:
    case support::BuiltinFn::kAssociativeArrayFind:
    case support::BuiltinFn::kAssociativeArrayPrepare:
    case support::BuiltinFn::kAssociativeArrayIndexOf:
    case support::BuiltinFn::kAssociativeArrayCommit:
    case support::BuiltinFn::kAssociativeArrayRemove:
    case support::BuiltinFn::kAssociativeArrayClear:
    case support::BuiltinFn::kAssociativeArrayFirst:
    case support::BuiltinFn::kAssociativeArrayLast:
    case support::BuiltinFn::kAssociativeArrayNext:
    case support::BuiltinFn::kAssociativeArrayPrevious:
    case support::BuiltinFn::kAssociativeArrayAfter:
    case support::BuiltinFn::kAssociativeArrayBefore:
    case support::BuiltinFn::kAssociativeArrayPrepareDefault:
    case support::BuiltinFn::kAssociativeArraySetDefault:
    case support::BuiltinFn::kAssociativeArrayStatedDefault:
    case support::BuiltinFn::kAssociativeArrayEnd:
    case support::BuiltinFn::kAssociativeArrayAdopt:
    case support::BuiltinFn::kWildcardIndexHasUnknown:
    case support::BuiltinFn::kDeferredEffectsAllocate:
    case support::BuiltinFn::kDeferredEffectsCommit:
    case support::BuiltinFn::kAssertionControlAllocate:
    case support::BuiltinFn::kAssertionControlCommit:
    case support::BuiltinFn::kEvaluationAttemptsAllocate:
    case support::BuiltinFn::kEvaluationAttemptsCommit:
    case support::BuiltinFn::kAssertionControlDescribe:
    case support::BuiltinFn::kSerializeBits:
    case support::BuiltinFn::kSerializeText:
    case support::BuiltinFn::kSerializeEvent:
    case support::BuiltinFn::kSerializeSize:
    case support::BuiltinFn::kSerializeKey:
    case support::BuiltinFn::kSubscribe:
    case support::BuiltinFn::kSelectionDepth:
    case support::BuiltinFn::kSelectionNumber:
    case support::BuiltinFn::kSelectionKey:
    case support::BuiltinFn::kSettleKey:
    case support::BuiltinFn::kSettlePositions:
    case support::BuiltinFn::kSettleUnfixed:
    case support::BuiltinFn::kSettledFixed:
    case support::BuiltinFn::kSettledPositions:
    case support::BuiltinFn::kDeserializeBits:
    case support::BuiltinFn::kDeserializeReal:
    case support::BuiltinFn::kDeserializeText:
    case support::BuiltinFn::kAssertionControlRegisterIn:
    case support::BuiltinFn::kAssertionControlRegister:
    case support::BuiltinFn::kAssertionControlRegisterWithin:
    case support::BuiltinFn::kAssertionControlAllowed:
    case support::BuiltinFn::kAssertionControlSubmitObserved:
    case support::BuiltinFn::kAssertionControlSubmitFinal:
    case support::BuiltinFn::kAssertControlOfDesign:
    case support::BuiltinFn::kAssertControlOfScope:
    case support::BuiltinFn::kAssertControlOfBlock:
    case support::BuiltinFn::kAssertControlOfLoop:
    case support::BuiltinFn::kFiles:
    case support::BuiltinFn::kCancellationFor:
    case support::BuiltinFn::kIsCancelled:
    case support::BuiltinFn::kFormat:
    case support::BuiltinFn::kFormatRuntime:
    case support::BuiltinFn::kRenderedFormatArgMake:
    case support::BuiltinFn::kPatternedFormatArgMake:
    case support::BuiltinFn::kTextPrintItemMake:
    case support::BuiltinFn::kWrite:
    case support::BuiltinFn::kWriteln:
    case support::BuiltinFn::kDiagnostic:
    case support::BuiltinFn::kEmitInfo:
    case support::BuiltinFn::kEmitWarning:
    case support::BuiltinFn::kEmitError:
    case support::BuiltinFn::kEmitFatal:
    case support::BuiltinFn::kEmitInfoAtCaller:
    case support::BuiltinFn::kEmitWarningAtCaller:
    case support::BuiltinFn::kEmitErrorAtCaller:
    case support::BuiltinFn::kEmitFatalAtCaller:
    case support::BuiltinFn::kRecordCoverage:
    case support::BuiltinFn::kTimeFormat:
    case support::BuiltinFn::kSetTimeFormat:
    case support::BuiltinFn::kResetTimeFormat:
    case support::BuiltinFn::kFileOpen:
    case support::BuiltinFn::kFileOpenMode:
    case support::BuiltinFn::kFileClose:
    case support::BuiltinFn::kFileGetc:
    case support::BuiltinFn::kFileGets:
    case support::BuiltinFn::kFileScan:
    case support::BuiltinFn::kFileRead:
    case support::BuiltinFn::kFileReadWords:
    case support::BuiltinFn::kFileError:
    case support::BuiltinFn::kFileUngetc:
    case support::BuiltinFn::kFileSeek:
    case support::BuiltinFn::kFileRewind:
    case support::BuiltinFn::kFileTell:
    case support::BuiltinFn::kFileEof:
    case support::BuiltinFn::kFileFlush:
    case support::BuiltinFn::kFileFlushAll:
    case support::BuiltinFn::kTestPlusargs:
    case support::BuiltinFn::kValuePlusargs:
    case support::BuiltinFn::kValuePlusargsString:
    case support::BuiltinFn::kDumpFile:
    case support::BuiltinFn::kDumpVarsLevels:
    case support::BuiltinFn::kDumpVarsOf:
    case support::BuiltinFn::kDumpVarsOfVariable:
    case support::BuiltinFn::kDumpOff:
    case support::BuiltinFn::kDumpOn:
    case support::BuiltinFn::kDumpAll:
    case support::BuiltinFn::kDumpLimit:
    case support::BuiltinFn::kDumpFlush:
    case support::BuiltinFn::kDumpPortsOf:
    case support::BuiltinFn::kDumpPortsOff:
    case support::BuiltinFn::kDumpPortsOn:
    case support::BuiltinFn::kDumpPortsAll:
    case support::BuiltinFn::kDumpPortsLimit:
    case support::BuiltinFn::kDumpPortsFlush:
    case support::BuiltinFn::kRunHostCommand:
    case support::BuiltinFn::kRunNullHostCommand:
    case support::BuiltinFn::kDelay:
    case support::BuiltinFn::kDelayReal:
    case support::BuiltinFn::kDelayTicks:
    case support::BuiltinFn::kDelayTicksReal:
    case support::BuiltinFn::kScalarPropagationDelay:
    case support::BuiltinFn::kVectorPropagationDelay:
    case support::BuiltinFn::kObservationOnReaching:
    case support::BuiltinFn::kObservationOfWatch:
    case support::BuiltinFn::kObservationOfWatchQualified:
    case support::BuiltinFn::kObservationFires:
    case support::BuiltinFn::kWaitOn:
    case support::BuiltinFn::kWaitPerChangeOnImplicitList:
    case support::BuiltinFn::kWaitPerIntervalOnImplicitList:
    case support::BuiltinFn::kWaitContinuouslyOn:
    case support::BuiltinFn::kWaitOnPropertyEvaluation:
    case support::BuiltinFn::kAttemptsOf:
    case support::BuiltinFn::kBindPropertyEvaluation:
    case support::BuiltinFn::kParkAt:
    case support::BuiltinFn::kWaitRecollecting:
    case support::BuiltinFn::kWaitUntil:
    case support::BuiltinFn::kReadReportEmpty:
    case support::BuiltinFn::kReadReportForImplicitList:
    case support::BuiltinFn::kReadReportAdd:
    case support::BuiltinFn::kReadReportAddObject:
    case support::BuiltinFn::kReadReportAddWrite:
    case support::BuiltinFn::kReadReportSettleAsImplicitList:
    case support::BuiltinFn::kReadReportEnter:
    case support::BuiltinFn::kReadReportLeave:
    case support::BuiltinFn::kReadReportRunsTheBody:
    case support::BuiltinFn::kReadReportBeginEvaluation:
    case support::BuiltinFn::kReadReportEndEvaluation:
    case support::BuiltinFn::kEvaluationInProgress:
    case support::BuiltinFn::kRefuseReport:
    case support::BuiltinFn::kSimTime:
    case support::BuiltinFn::kSTime:
    case support::BuiltinFn::kRealTime:
    case support::BuiltinFn::kUrandom:
    case support::BuiltinFn::kUrandomSeeded:
    case support::BuiltinFn::kUrandomRange:
    case support::BuiltinFn::kRandom:
    case support::BuiltinFn::kDistUniform:
    case support::BuiltinFn::kDistNormal:
    case support::BuiltinFn::kDistExponential:
    case support::BuiltinFn::kDistPoisson:
    case support::BuiltinFn::kDistChiSquare:
    case support::BuiltinFn::kDistT:
    case support::BuiltinFn::kDistErlang:
    case support::BuiltinFn::kFinish:
    case support::BuiltinFn::kStop:
    case support::BuiltinFn::kExit:
    case support::BuiltinFn::kEnclosingScope:
    case support::BuiltinFn::kLoopBlocksAdd:
    case support::BuiltinFn::kLoopBlocksAt:
    case support::BuiltinFn::kSetScopePlace:
    case support::BuiltinFn::kAttachScope:
    case support::BuiltinFn::kInstantiationEntry:
    case support::BuiltinFn::kInstantiationTable:
    case support::BuiltinFn::kBlockEntry:
    case support::BuiltinFn::kBlockTable:
    case support::BuiltinFn::kExtendSequence:
    // Recovering the object a handle names, a handle naming the object a body
    // runs on, or what the definition of an object's class answers of it. Each
    // is one library function serving every class, so the operation's own name
    // is the whole of what a symbol needs.
    case support::BuiltinFn::kObjectOf:
    case support::BuiltinFn::kSelfHandle:
    case support::BuiltinFn::kDynamicCast:
    case support::BuiltinFn::kIsOfInterfaceOf:
    // What reports a change to an object's properties, one library function
    // each for every class.
    case support::BuiltinFn::kObjectEventSource:
    case support::BuiltinFn::kOpenObjectWrite:
    case support::BuiltinFn::kWrittenObject:
    // A reference to a property holds the property erased, so one function
    // serves every property's type, as one leases the object any such
    // reference names storage of and one answers the reference any leased one
    // carries.
    case support::BuiltinFn::kReferProperty:
    case support::BuiltinFn::kLeaseObject:
    case support::BuiltinFn::kLeasedReference:
    // Binding a reference -- a port's, as a member or as another name, or one
    // an object holds -- and telling one it is driven continuously, act on the
    // reference alone.
    case support::BuiltinFn::kBindMember:
    case support::BuiltinFn::kBindAlias:
    case support::BuiltinFn::kBindReference:
    case support::BuiltinFn::kDrivesContinuously:
    // What an enumeration's member list answers about a value. One routine
    // serves every enumeration, because the list is the receiver and every
    // member is an integral value.
    case support::BuiltinFn::kEnumerationHas:
    case support::BuiltinFn::kEnumerationName:
    case support::BuiltinFn::kEnumerationNext:
    case support::BuiltinFn::kEnumerationPrev:
    case support::BuiltinFn::kForkWaitAll:
    case support::BuiltinFn::kForkWaitFirst:
    case support::BuiltinFn::kSpawnAll:
    case support::BuiltinFn::kWaitFork:
    case support::BuiltinFn::kDisableFork:
    case support::BuiltinFn::kRequireCallerMayScheduleAfterReturn:
    case support::BuiltinFn::kTakeNoThreadContext:
    case support::BuiltinFn::kEnterNoThread:
    case support::BuiltinFn::kDisable:
    case support::BuiltinFn::kRegisterCoroutine:
    case support::BuiltinFn::kRegisterReaction:
    case support::BuiltinFn::kRegisterFinal:
    case support::BuiltinFn::kRunAssertionAction:
    case support::BuiltinFn::kEnterScopeStaticInit:
    case support::BuiltinFn::kEnterNamespaceStaticInit:
    case support::BuiltinFn::kLeaveStaticInit:
    case support::BuiltinFn::kEnterDpiScope:
    case support::BuiltinFn::kLeaveDpiScope:
    case support::BuiltinFn::kExportedTaskReturnValue:
    case support::BuiltinFn::kContextImportTaskReturned:
    case support::BuiltinFn::kContextImportFunctionReturned:
    case support::BuiltinFn::kCheckExportReachable:
    case support::BuiltinFn::kBeforeForeignCodeRuns:
    case support::BuiltinFn::kHandStringToForeignCaller:
    case support::BuiltinFn::kClaimNamespaceInitialize:
    case support::BuiltinFn::kRegisterNamespace:
    case support::BuiltinFn::kCurrentExportScope:
    case support::BuiltinFn::kFindExportEntry:
    case support::BuiltinFn::kRunForeignTaskOnFiber:
    case support::BuiltinFn::kRunExportedTaskToCompletion:
    case support::BuiltinFn::kWeakReferenceDefault:
    case support::BuiltinFn::kWildcardIndexMake:
    case support::BuiltinFn::kHierarchySegmentMake:
    case support::BuiltinFn::kTriggerMake:
    case support::BuiltinFn::kFormatSpecMake:
    case support::BuiltinFn::kIntegralPrintItemMake:
    case support::BuiltinFn::kIntegralFormatArgMake:
    case support::BuiltinFn::kDpiBitBufferMake:
    case support::BuiltinFn::kDpiLogicBufferMake:
    case support::BuiltinFn::kDpiOpenArrayMake:
    case support::BuiltinFn::kEnterTarget:
    case support::BuiltinFn::kLeaveTarget:
    case support::BuiltinFn::kRegionLandsEffect:
    case support::BuiltinFn::kReceiveDeparture:
    case support::BuiltinFn::kProcessSelf:
    case support::BuiltinFn::kProcessStatus:
    case support::BuiltinFn::kProcessKill:
    case support::BuiltinFn::kProcessAwait:
    case support::BuiltinFn::kProcessSuspend:
    case support::BuiltinFn::kProcessResume:
      // A class object's hooks and generator, and the steps of a call of
      // `randomize()`: each is one library function, whatever class it acts on
      // and whatever type a variable it names is of, since every variable
      // crosses with its type.
    case support::BuiltinFn::kBuiltinPreRandomize:
    case support::BuiltinFn::kBuiltinPostRandomize:
    case support::BuiltinFn::kObjectPreRandomize:
    case support::BuiltinFn::kObjectPostRandomize:
    case support::BuiltinFn::kObjectSrandom:
    case support::BuiltinFn::kObjectGetRandstate:
    case support::BuiltinFn::kObjectSetRandstate:
    case support::BuiltinFn::kWeakDowngrade:
    case support::BuiltinFn::kWeakUpgrade:
    case support::BuiltinFn::kWeakRefersTo:
    case support::BuiltinFn::kObjectIdentity:
    case support::BuiltinFn::kRandomizeObject:
    case support::BuiltinFn::kRandomizeObjectListed:
    case support::BuiltinFn::kRandomizeScope:
    case support::BuiltinFn::kRandomizeList:
    case support::BuiltinFn::kRandomizeConstrain:
    case support::BuiltinFn::kRandomizeFinish:
    case support::BuiltinFn::kConstraintMembers:
    case support::BuiltinFn::kConstraintRand:
    case support::BuiltinFn::kConstraintRandc:
    case support::BuiltinFn::kRandomWalkFixedArray:
    case support::BuiltinFn::kRandomWalkResizableArray:
    case support::BuiltinFn::kRandomWalkEnter:
    case support::BuiltinFn::kRandomWalkLeave:
    case support::BuiltinFn::kRandomWalkLeaf:
    case support::BuiltinFn::kRandomWalkEnd:
    case support::BuiltinFn::kConstraintVariable:
    case support::BuiltinFn::kConstraintConstant:
    case support::BuiltinFn::kRandomizeNested:
    case support::BuiltinFn::kConstraintBlockEnabled:
    case support::BuiltinFn::kStaticConstraintBlockEnabled:
    case support::BuiltinFn::kConstraintNegate:
    case support::BuiltinFn::kConstraintBitwiseNot:
    case support::BuiltinFn::kConstraintReduceAnd:
    case support::BuiltinFn::kConstraintReduceOr:
    case support::BuiltinFn::kConstraintReduceXor:
    case support::BuiltinFn::kConstraintLogicalNot:
    case support::BuiltinFn::kConstraintAdd:
    case support::BuiltinFn::kConstraintSubtract:
    case support::BuiltinFn::kConstraintMultiply:
    case support::BuiltinFn::kConstraintDivide:
    case support::BuiltinFn::kConstraintRemainder:
    case support::BuiltinFn::kConstraintBitwiseAnd:
    case support::BuiltinFn::kConstraintBitwiseOr:
    case support::BuiltinFn::kConstraintBitwiseXor:
    case support::BuiltinFn::kConstraintShiftLeft:
    case support::BuiltinFn::kConstraintShiftRight:
    case support::BuiltinFn::kConstraintEqual:
    case support::BuiltinFn::kConstraintLess:
    case support::BuiltinFn::kConstraintLogicalAnd:
    case support::BuiltinFn::kConstraintLogicalOr:
    case support::BuiltinFn::kConstraintConcatenate:
    case support::BuiltinFn::kConstraintSelect:
    case support::BuiltinFn::kConstraintResize:
    case support::BuiltinFn::kConstraintSlice:
    case support::BuiltinFn::kConstraintRequire:
    case support::BuiltinFn::kConstraintPrefer:
    case support::BuiltinFn::kConstraintDisableSoft:
    case support::BuiltinFn::kConstraintSolveBefore:
    case support::BuiltinFn::kConstraintDistribution:
    case support::BuiltinFn::kConstraintDistributionItem:
    case support::BuiltinFn::kConstraintDistributionDefault:
    case support::BuiltinFn::kConstraintUniqueSet:
    case support::BuiltinFn::kConstraintUniqueMember:
    case support::BuiltinFn::kConstraintUniqueArray:
    case support::BuiltinFn::kConstraintUniqueRequire:
    case support::BuiltinFn::kConstraintArraySize:
    case support::BuiltinFn::kConstraintWant:
    case support::BuiltinFn::kRandomizeSettleFirst:
    case support::BuiltinFn::kRandModeSet:
    case support::BuiltinFn::kRandModeGet:
    case support::BuiltinFn::kObjectRandModeSet:
    case support::BuiltinFn::kConstraintModeSet:
    case support::BuiltinFn::kConstraintModeGet:
    case support::BuiltinFn::kObjectConstraintModeSet:
    case support::BuiltinFn::kStaticConstraintModeSet:
    case support::BuiltinFn::kStaticConstraintModeGet:
    case support::BuiltinFn::kHierarchicalPathOf:
    // An assertion's attempts hold machine words and no value of the design,
    // so there is no representation for these to be named by (LRM 16.14.1).
    case support::BuiltinFn::kEvaluationAttemptsInstall:
    case support::BuiltinFn::kEvaluationAttemptsTick:
    case support::BuiltinFn::kEvaluationAttemptsRun:
    case support::BuiltinFn::kEvaluationAttemptsTestDisable:
    case support::BuiltinFn::kEvaluationAttemptsLiveWord:
    case support::BuiltinFn::kEvaluationAttemptsAnswer:
    case support::BuiltinFn::kEvaluationAttemptsActionRuns:
    case support::BuiltinFn::kEvaluationAttemptSharedWord:
    case support::BuiltinFn::kEvaluationAttemptLiveWord:
    case support::BuiltinFn::kEvaluationAttemptNextUnstepped:
    case support::BuiltinFn::kEvaluationAttemptBitsAt:
    case support::BuiltinFn::kEvaluationAttemptSetWord:
    case support::BuiltinFn::kEvaluationAttemptStep:
    case support::BuiltinFn::kEvaluationAttemptSeedWord:
    case support::BuiltinFn::kEvaluationAttemptSeed:
    case support::BuiltinFn::kEvaluationAttemptCount:
    case support::BuiltinFn::kEvaluationAttemptPartOf:
    case support::BuiltinFn::kEvaluationAttemptImplicationAnswer:
    case support::BuiltinFn::kEvaluationAttemptImplicationNonvacuous:
    case support::BuiltinFn::kEvaluationAttemptResolve:
    // A delayed continuous assignment's timer holds a closure and a place in a
    // queue, and no value of the design (LRM 10.3.3).
    case support::BuiltinFn::kTimerBind:
    case support::BuiltinFn::kTimerArm:
    // Reading the C string a string carries. Only a string carries one, so the
    // operation's own name is the whole of what a symbol needs.
    case support::BuiltinFn::kStringCStr:
    // A check before an access reads a machine boolean and a message, so one
    // function serves every check.
    case support::BuiltinFn::kAssert:
    // A value built from a stream of bits (LRM 6.24.3), asked of the type it
    // is told it answers, so one function serves every type.
    case support::BuiltinFn::kFromBitstream:
    // A run of bits is one kind of value with one realization, and a string
    // built out of one is one function, each entry's own name saying which.
    case support::BuiltinFn::kStringFromBitVec:
    case support::BuiltinFn::kBitVecOf:
    case support::BuiltinFn::kBitVecExtend:
    case support::BuiltinFn::kBitVecAppend:
    case support::BuiltinFn::kBitVecInsertBits:
    case support::BuiltinFn::kBitVecReverseBlocks:
    case support::BuiltinFn::kBitVecLen:
    case support::BuiltinFn::kBitVecExtractBits:
    case support::BuiltinFn::kBitVecLoad:
    case support::BuiltinFn::kBitVecPaddedTo:
    // The DPI-C boundary marshaling (LRM 35.5.6, Annex H.7.7, H.10). Each of
    // these is one library function and not a family: what a canonical buffer,
    // an `svLogic` scalar and an open-array image hold is fixed by the C ABI,
    // so the SV value on the far side of one is always an integral value and
    // the buffer itself belongs to no value domain at all.
    case support::BuiltinFn::kToSvLogic:
    case support::BuiltinFn::kWriteCanonicalBitVec:
    case support::BuiltinFn::kWriteCanonicalLogicVec:
    case support::BuiltinFn::kDpiBitBufferData:
    case support::BuiltinFn::kDpiLogicBufferData:
    case support::BuiltinFn::kDpiOpenArrayHandle:
    case support::BuiltinFn::kDpiOpenArrayElementCount:
    case support::BuiltinFn::kDpiOpenArrayOrdinal:
    case support::BuiltinFn::kDpiOpenArrayPut:
    case support::BuiltinFn::kDpiOpenArrayWord:
    // A memory task's words (LRM 21.4, 21.5): what moves between the file and
    // the memory, the same for every memory, a word crossing with its type.
    case support::BuiltinFn::kReadMem:
    case support::BuiltinFn::kReadMemWithin:
    case support::BuiltinFn::kReadEnumeratedMem:
    case support::BuiltinFn::kReadEnumeratedMemWithin:
    case support::BuiltinFn::kWriteMem:
    case support::BuiltinFn::kWriteMemWithin:
    case support::BuiltinFn::kMemoryWordsReadGrid:
    case support::BuiltinFn::kMemoryWordsReadDense:
    case support::BuiltinFn::kMemoryWordsReadKeyed:
    case support::BuiltinFn::kMemoryWordsPlanGrid:
    case support::BuiltinFn::kMemoryWordsPlanDense:
    case support::BuiltinFn::kMemoryWordsCount:
    case support::BuiltinFn::kMemoryWordsOrdinal:
    case support::BuiltinFn::kMemoryWordsKey:
    case support::BuiltinFn::kMemoryWordsBytesRead:
    case support::BuiltinFn::kMemoryWordsWord:
    case support::BuiltinFn::kMemoryWordsPut:
    case support::BuiltinFn::kMemoryWordsPutKeyed:
    case support::BuiltinFn::kMemoryWordsCommit:
    // An operation whose own name says the one kind of value it is over or
    // builds: a string repeated or built out of bits (LRM 11.4.12.1, 6.16), and
    // an unpacked array of byte built out of text or of bits (LRM 5.9).
    case support::BuiltinFn::kReplicateString:
    case support::BuiltinFn::kStringFromBits:
    case support::BuiltinFn::kByteArrayFromString:
    case support::BuiltinFn::kByteArrayFromBits:
      return NamedAlone{};

    // A value of any type made of a value crossing with its type, a reference
    // of any type made of storage crossing with its type, and a value received
    // through such a reference: the library holds each as one object whatever
    // type it is of (LRM 15.4.9).
    case support::BuiltinFn::kAnyMake:
    case support::BuiltinFn::kSeeAsAny:
    case support::BuiltinFn::kReceiveAny:
      return NamedAlone{};
  }
  throw InternalError("llvm codegen: unknown builtin");
}

auto StatesValueType(support::ValueDomain domain) -> bool {
  return std::ranges::any_of(kForStatedValueTypes, [&](const RealizedFor& one) {
    return one.domains.Holds(domain);
  });
}

auto StatesValueOperations(support::ValueDomain domain) -> bool {
  return kValuesComparedWhole.Holds(domain);
}

auto RealizesConversion(
    support::BuiltinFn fn, support::ValueDomain destination,
    support::ValueDomain source) -> bool {
  return std::ranges::any_of(
      Conversions(EntryNamingOf(fn)), [&](const Conversion& one) {
        return one.destination == destination && one.source == source;
      });
}

}  // namespace lyra::backend::llvm_backend
