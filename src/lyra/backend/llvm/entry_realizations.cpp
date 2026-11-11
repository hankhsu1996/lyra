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
// it in its bytes: a tuple, a record and a fixed-size unpacked array, the one
// sequence of fixed extent; the other sequences and containers are objects the
// library holds as its own.
constexpr DomainSet kLaidOutByType{Domain::kLaidOut};
// The kinds of value no other type parameterizes whose operations the library
// states whole.
constexpr DomainSet kValuesComparedWhole{
    Domain::kString, Domain::kReal, Domain::kShortReal, Domain::kUnmanagedRef,
    Domain::kManagedRef};
// What the language admits as an index besides an integral value (LRM 7.8.1
// -- 7.8.4).
constexpr DomainSet kIndexKinds{
    Domain::kString, Domain::kUnmanagedRef, Domain::kManagedRef,
    Domain::kWildcardIndex};
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

constexpr auto Takes(support::LibraryDecl decl, support::LibraryParam param)
    -> bool {
  const std::span<const support::LibraryParam> params =
      support::DeclarationOf(decl).params;
  return std::ranges::find(params, param) != params.end();
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

// The containers the library holds, which take an element type, and of them
// the sequences, which take no index type (LRM 7.5, 7.8, 7.10).
constexpr DomainSet kLibraryContainers =
    DomainsDeclared([](support::LibraryDecl decl) {
      return Takes(decl, support::LibraryParam::kElement);
    });
constexpr DomainSet kLibrarySequences =
    DomainsDeclared([](support::LibraryDecl decl) {
      return Takes(decl, support::LibraryParam::kElement) &&
             !Takes(decl, support::LibraryParam::kIndex);
    });
constexpr DomainSet kSequences = kLibrarySequences | kLaidOutByType;
constexpr DomainSet kContainers = kLibraryContainers | kLaidOutByType;
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
// What a net holds: an integral value, whose positions a connection may name,
// a value laid out by its type, and a real, a string or a class handle, which
// no net type resolves and only an input port written with no port kind
// declares a net of (LRM 6.6.7, 23.2.2.3).
constexpr DomainSet kUnfoldedNetValues =
    kReals | DomainSet{Domain::kString, Domain::kManagedRef};
constexpr DomainSet kNetValues =
    kIntegral | kLaidOutByType | kUnfoldedNetValues;

// What a variable holds besides an integral value and reads and writes
// through an entry: an object of the library's, a real and a reference owning
// nothing, whose domain says what they are where they lie; and, among what a
// variable holds, a tuple, a union and a fixed-size unpacked array, whose
// bytes do not.
// Of those, a holder is built from nothing for all but a container the library
// holds for every element type alike.
constexpr DomainSet kHeldThroughEntriesBuiltFromNothing =
    DomainsDeclared([](support::LibraryDecl decl) {
      return support::DeclarationOf(decl).is_a_data_type &&
             !Takes(decl, support::LibraryParam::kElement);
    }) |
    kScalarsBesideIntegral | DomainSet{Domain::kAny, Domain::kManagedRef};
constexpr DomainSet kHeldThroughEntries =
    kHeldThroughEntriesBuiltFromNothing | kLibraryContainers;
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
// force the standard forbids, a container the library holds, forced whole, and
// a value laid out by its type, forced whole or in part.
constexpr DomainSet kLibraryValuesTakenOver =
    kIntegral | kReals | kLibraryContainers |
    DomainSet{Domain::kString, Domain::kManagedRef};
constexpr DomainSet kVariableValuesTakenOver =
    kLibraryValuesTakenOver | kLaidOutByType;

constexpr std::array kForAssociativeArrays{
    RealizedFor{.domains = {Domain::kAssocArray}}};
// A literal's entries each cross as the pair of the index and the element it
// stores (LRM 7.9.11), a tuple, so the array built from them is told the type
// of that pair after them.
constexpr std::array kForAssociativeArraysFromEntries{RealizedFor{
    .domains = DomainsBuiltBy(support::BuiltinFn::kAssociativeArrayFromEntries),
    .told = ToldEntryType{.after = 0}}};
constexpr std::array kForDynamicArraysAndQueues{
    RealizedFor{.domains = kLibrarySequences}};
constexpr std::array kForDynamicArraysAndQueuesByOperations{
    RealizedFor{.domains = kLibrarySequences, .told = ToldOperations{}}};
constexpr std::array kForWhatDeletesWhole{
    RealizedFor{.domains = kLibraryContainers}};
constexpr std::array kForArraysOfFixedExtent{
    RealizedFor{.domains = {Domain::kDynArray, Domain::kLaidOut}}};
constexpr std::array kForSequences{RealizedFor{.domains = kSequences}};
constexpr std::array kForContainers{RealizedFor{.domains = kContainers}};
constexpr std::array kForDynamicArrays{
    RealizedFor{.domains = {Domain::kDynArray}}};
constexpr std::array kForQueues{RealizedFor{.domains = {Domain::kQueue}}};
constexpr std::array kForReal{RealizedFor{.domains = {Domain::kReal}}};
constexpr std::array kForReals{RealizedFor{.domains = kReals}};
constexpr std::array kForStrings{RealizedFor{.domains = {Domain::kString}}};
// What an entry answers a change of bits for: every value but an integral one,
// which the target answers itself, and one laid out by its type, which that
// type's own operation answers.
constexpr std::array kForWhatAnEntryComparesWhole{
    RealizedFor{.domains = kLibraryContainers | kValuesComparedWhole}};
// The same for `===` and an unknown bit, where a container compares its
// elements by the operations it is told.
constexpr std::array kForWhatAnEntryComparesByOperations{
    RealizedFor{.domains = kLibraryContainers, .told = ToldOperations{}},
    RealizedFor{.domains = kValuesComparedWhole}};
constexpr std::array kForBitstreams{
    RealizedFor{.domains = kLibraryContainers, .told = ToldOperations{}},
    RealizedFor{.domains = DomainSet{Domain::kString, Domain::kManagedRef}}};
// What answers its stream as a run of bits as long as the value makes it: a
// container of the library's and a string. A class handle's stream is the
// object's, which the library does not build.
constexpr std::array kForRunsOfBits{
    RealizedFor{.domains = kLibraryContainers, .told = ToldOperations{}},
    RealizedFor{.domains = DomainSet{Domain::kString}}};
// What a run of a sequence's elements is streamed or merged for, by the
// operations of the sequence's type, which name its element's.
constexpr std::array kForSequencesByOperations{
    RealizedFor{.domains = kSequences, .told = ToldOperations{}}};
constexpr std::array kForUnpackedArrays{RealizedFor{.domains = kLaidOutByType}};
constexpr std::array kForUnpackedArraysAndQueues{
    RealizedFor{.domains = kLaidOutByType | DomainSet{Domain::kQueue}}};
// A step into a component of a designated record, and a reference to one, is
// told the record's type, as any realization for one is. Every member of a
// union lies at the union's own start, so a step into one is told nothing.
constexpr std::array kForComponentsSteppedInto{
    RealizedFor{.domains = kLaidOutByType}};
constexpr std::array kForMembersSteppedInto{
    RealizedFor{.domains = kLaidOutByType, .told = ToldNothing{}}};
constexpr std::array kForComponentsReferredTo{
    RealizedFor{.domains = kLaidOutByType}};
constexpr std::array kForJoinedNets{RealizedFor{.domains = kIntegral}};
// A net attaching a driver, and a variable's cell a reference is built over,
// keep what they hold where it lies, so neither is told its type.
constexpr std::array kForNets{
    RealizedFor{.domains = kNetValues, .told = ToldNothing{}}};
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

// A memory a file fills (LRM 21.4): one reached by a key is told the type its
// keys are built at, after the memory.
constexpr std::array kForLoadedMemories{
    RealizedFor{.domains = kSequences},
    RealizedFor{
        .domains = {Domain::kAssocArray}, .told = ToldKeyType{.after = 1}}};
// A history is filled with a default, which fixes how wide its values are.
constexpr std::array kForHistoriesInstalled{
    RealizedFor{
        .domains = kOneWordIntegral | kContainers | kReals |
                   DomainSet{Domain::kString, Domain::kManagedRef}},
    RealizedFor{
        .domains = kWideIntegral, .told = ToldWidthMetadata{.after = 1}}};
// A history keeps the type of the default it was filled with, so what is
// pushed into it and read back out is told nothing more.
constexpr std::array kForHistories{
    RealizedFor{
        .domains = kIntegral | kLibraryContainers | kReals |
                   DomainSet{Domain::kString, Domain::kManagedRef}},
    RealizedFor{.domains = kLaidOutByType, .told = ToldNothing{}}};
constexpr std::array kForIntegralNets{RealizedFor{.domains = kIntegral}};
// A net laying its value out after itself by the value's type was told that
// type where it was built, and one of a value the library holds whole knows
// its type.
constexpr std::array kForAggregateNets{
    RealizedFor{.domains = kLaidOutByType, .told = ToldNothing{}}};
constexpr std::array kForUnresolvedNets{
    RealizedFor{.domains = kLaidOutByType, .told = ToldNothing{}},
    RealizedFor{.domains = kUnfoldedNetValues}};

// A variable is taken over whether reached through its cell or through storage
// lent for it. A takeover through a reference acts on the variable the
// reference names, whose cell knows its own type, so it is told nothing of a
// value laid out by its type.
constexpr std::array kTakeovers{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell, .domains = kVariableValuesTakenOver},
    ThroughWrapper{.wrapper = WrapperKind::kNet, .domains = kNetValues},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .domains = kVariableValuesTakenOver,
        .told = ToldNothing{}}};
// Beginning one on a value laid out by its type is told how the type takes
// some of its positions, which the takeover shows what it holds by.
constexpr std::array kTakeoversBegun{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell, .domains = kLibraryValuesTakenOver},
    ThroughWrapper{
        .wrapper = WrapperKind::kCell,
        .domains = kLaidOutByType,
        .told = ToldTakePositions{}},
    ThroughWrapper{
        .wrapper = WrapperKind::kNet,
        .domains = kIntegral | kUnfoldedNetValues},
    ThroughWrapper{
        .wrapper = WrapperKind::kNet,
        .domains = kLaidOutByType,
        .told = ToldTakePositions{}},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .domains = kLibraryValuesTakenOver,
        .told = ToldNothing{}},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .domains = kLaidOutByType,
        .told = ToldTakePositions{}}};
// A variable's cell is installed with its declaration's value. A cell of words
// was told its width where it was built, and one of a value laid out by its
// type that type.
constexpr std::array kInstalls{
    ThroughWrapper{.wrapper = WrapperKind::kCell, .domains = kBodyValues}};
// A cell holds the bytes of a value no wider than a word where generated code
// reads them, and the address of any other value of its own laying it out after
// itself; a net keeps the address of what it shows, whatever that is laid out
// by. So only the cells and nets of objects the library keeps have an entry
// reading one. A reference read, or sampled, answers where what it names lies,
// so it is told nothing of it.
constexpr std::array kLoads{
    ThroughWrapper{.wrapper = WrapperKind::kCell, .domains = kBodyObjects},
    ThroughWrapper{.wrapper = WrapperKind::kDriver, .domains = kNetValues},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .domains = kVariableValues,
        .told = ToldNothing{}}};
// What a driver is told of the object it carries, and asked at its turn.
constexpr std::array kDrivesFromAnObject{
    ThroughWrapper{.wrapper = WrapperKind::kDriver, .domains = kNetValues}};
// Where a wait on what a wrapper stands for enrols: a variable is where its
// writes are told, a net keeps the member it shows its value through where its
// value's type puts it, and a reference names the member it was bound into.
constexpr std::array kWaitedOn{
    ThroughWrapper{.wrapper = WrapperKind::kCell, .domains = kVariableValues},
    ThroughWrapper{.wrapper = WrapperKind::kNet, .domains = kNetValues},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .domains = kVariableValues,
        .told = ToldNothing{}}};
// What keeps a sample of what it holds (LRM 16.5.1): armed to take one, and
// read for the one it took.
constexpr std::array kSampled{
    ThroughWrapper{.wrapper = WrapperKind::kCell, .domains = kVariableValues},
    ThroughWrapper{.wrapper = WrapperKind::kNet, .domains = kNetValues},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .domains = kVariableValues,
        .told = ToldNothing{}}};
// What a dump records of a member (LRM 21.7.2): an integral value or a real,
// held by a variable, shown by a net, or stood for by a port.
constexpr std::array kDescribed{
    ThroughWrapper{
        .wrapper = WrapperKind::kCell, .domains = kIntegral | kReals},
    ThroughWrapper{.wrapper = WrapperKind::kNet, .domains = kIntegral | kReals},
    ThroughWrapper{
        .wrapper = WrapperKind::kRef,
        .domains = kIntegral | kReals,
        .told = ToldNothing{}}};
constexpr ThroughWrapper kThroughAReferenceToBytesOrAnObject{
    .wrapper = WrapperKind::kRef,
    .domains = kOneWordIntegral | kHeldThroughEntries};
// A reference to a value laid out by its type names bytes lying in something
// else, which say nothing of what they are, so an access that replaces the
// value or opens a write on it is told the value's type, as any realization
// for such a value is.
constexpr ThroughWrapper kThroughAReferenceToALaidOutValue{
    .wrapper = WrapperKind::kRef, .domains = kLaidOutByType};
// A reference to a value wider than a word names words lying in something
// else, so an access through one that takes or replaces them is told how wide
// they are.
constexpr ThroughWrapper kThroughAReferenceToWords{
    .wrapper = WrapperKind::kRef,
    .domains = kWideIntegral,
    .told = ToldWidthMetadata{.after = 0}};
constexpr std::array kStores{
    ThroughWrapper{.wrapper = WrapperKind::kCell, .domains = kBodyValues},
    ThroughWrapper{.wrapper = WrapperKind::kDriver, .domains = kNetValues},
    kThroughAReferenceToBytesOrAnObject, kThroughAReferenceToWords,
    kThroughAReferenceToALaidOutValue};
constexpr std::array kOpenedWrites{
    ThroughWrapper{.wrapper = WrapperKind::kCell, .domains = kBodyValues},
    ThroughWrapper{.wrapper = WrapperKind::kDriver, .domains = kNetValues},
    kThroughAReferenceToBytesOrAnObject, kThroughAReferenceToWords,
    kThroughAReferenceToALaidOutValue};

constexpr std::array kRealConversions{
    Conversion{.destination = Domain::kReal, .source = Domain::kReal},
    Conversion{.destination = Domain::kReal, .source = Domain::kShortReal},
    Conversion{.destination = Domain::kShortReal, .source = Domain::kReal}};

// A cell the library makes -- what a suspending call answers into, or one a
// counted hold keeps -- is as large as the value it is to hold, so one of
// words is made at their width, and one of a value laid out by its type with
// that type; and it holds a value of its type from then on, so one of a
// container the library holds is made told the container's type.
constexpr std::array kForCellsMade{
    RealizedFor{
        .domains = kOneWordIntegral | kHeldThroughEntriesBuiltFromNothing},
    RealizedFor{.domains = kWideIntegral, .told = ToldWidthMetadataAhead{}},
    RealizedFor{.domains = kLaidOutByType, .told = ToldTypeMetadataAhead{}},
    RealizedFor{
        .domains = kLibraryContainers, .told = ToldContainerTypeAhead{}}};
// A cell whose value lies where the library states is read and written there,
// so only an object of the library's own is read and written through an entry.
constexpr std::array kForCellsReached{
    RealizedFor{.domains = kHeldThroughEntries}};

// A designation of words is told how wide they are where the write lands.
constexpr std::array kForLandings{
    RealizedFor{.domains = kOneWordIntegral | kBodyObjects | kLaidOutByType},
    RealizedFor{
        .domains = kWideIntegral, .told = ToldWidthMetadata{.after = 0}}};
// A step into an element and a write of a run of elements each act on the
// sequence a designation or a reference names where it lies; a designation's
// step into a queue's element is an entry of its own, the one that can report.
constexpr std::array kForSequencesSteppedInto{
    RealizedFor{.domains = kSequences}};
constexpr std::array kForArraysDesignatedInto{
    RealizedFor{.domains = {Domain::kDynArray, Domain::kLaidOut}}};
constexpr std::array kForElementSlicesAssigned{
    RealizedFor{.domains = DomainSet{Domain::kDynArray} | kLaidOutByType}};
constexpr std::array kForQueueSlicesAssigned{
    RealizedFor{.domains = {Domain::kQueue}}};
// A designation of any integral value is told how wide it is where a write of
// some of its bits is reported (LRM 11.5.1), which holds the bits written to
// the value's width.
constexpr std::array kForBitsWritten{
    RealizedFor{.domains = kIntegral, .told = ToldWidthMetadata{.after = 0}}};

// The values the types the library declares start as, each realized for the
// domains whose declarations name its entry for it: a container's told the
// type it builds; and the null handle a class handle referring to nothing is.
constexpr std::array kForDefaults{
    RealizedFor{.domains = DomainsBuiltBy(support::BuiltinFn::kDefault)}};
constexpr std::array kForContainerDefaults{RealizedFor{
    .domains = DomainsBuiltBy(support::BuiltinFn::kContainerDefault)}};
constexpr std::array kForNullHandles{
    RealizedFor{.domains = {Domain::kManagedRef}}};
// What is read as true where it refers to an object and false where it is
// null (LRM 12.4): a class handle.
constexpr std::array kForHandlesReadAsConditions{
    RealizedFor{.domains = {Domain::kManagedRef}}};
constexpr std::array kForWhatFormatsItself{RealizedFor{
    .domains = kScalarsBesideIntegral |
               DomainSet{Domain::kString, Domain::kManagedRef}}};
// The constructors the declarations name, each realized for the domains of
// the declarations naming it: a value built from the host text it holds, and
// a sequence built from an element list.
constexpr std::array kForWhatTextBuilds{RealizedFor{
    .domains = DomainsBuiltBy(support::BuiltinFn::kStringFromText)}};
constexpr std::array kForWhatAListBuilds{RealizedFor{
    .domains = DomainsBuiltBy(support::BuiltinFn::kSequenceFromList)}};
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
    case WrapperKind::kNet:
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
    if (one.wrapper == wrapper && one.domains.Holds(domain)) {
      return ToldThroughTheWrapper(domain, wrapper, one.told);
    }
  }
  throw InternalError(Refusal(
      support::RuntimeEntryOf(fn).name,
      std::format(
          "{} through a {}", support::ValueDomainName(domain),
          WrapperName(wrapper))));
}

auto ToldOfAnOperator(support::ValueDomain domain) -> Told {
  // A container the library holds compares its elements by the operations it
  // is told (LRM 11.4.5).
  if (kLibraryContainers.Holds(domain)) {
    return ToldOperations{};
  }
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
          // value laid out by its type that type and how to fold values of it.
          [](const support::ResolvedNetMember& m) -> Told {
            if (kIntegral.Holds(m.domain)) {
              return ToldWidthMetadataAhead{};
            }
            if (support::IsLaidOutByItsType(m.domain)) {
              return ToldTypeAndNetFoldsAhead{};
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
      return kForUnpackedArrays;
    case RuntimeOp::kToBool:
      return kForHandlesReadAsConditions;
    case RuntimeOp::kCopy:
    case RuntimeOp::kMove:
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
    case lir::OpenWriteTarget::Op::kAssignElementSlice:
      return kForElementSlicesAssigned;
    case lir::OpenWriteTarget::Op::kAssignQueueSlice:
      return kForQueueSlicesAssigned;
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
  constexpr std::string_view kWrittenWhereTheBitsLie =
      "answers a reference to bits, which this target writes through by a "
      "store of the bits where they lie";
  switch (fn) {
    // What a value starts as, and the constructors, realized per kind of
    // value they build, and per kind of value they are built over: what a
    // value formats as is the value's own answer.
    case support::BuiltinFn::kDefault:
      return NamedByResult{.realized = kForDefaults};
    case support::BuiltinFn::kContainerDefault:
      return NamedByResult{.realized = kForContainerDefaults};
    case support::BuiltinFn::kStringFromText:
      return NamedByResult{.realized = kForWhatTextBuilds};
    case support::BuiltinFn::kSequenceFromList:
      return NamedByResult{.realized = kForWhatAListBuilds};
    case support::BuiltinFn::kAssociativeArrayFromEntries:
      return NamedByResult{.realized = kForAssociativeArraysFromEntries};
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
    case support::BuiltinFn::kIntegralResolveTriState:
    case support::BuiltinFn::kIntegralResolveWiredAnd:
    case support::BuiltinFn::kIntegralResolveWiredOr:
    case support::BuiltinFn::kIntegralDominating:
    case support::BuiltinFn::kIntegralTakePositions:
    case support::BuiltinFn::kIntegralMergeConditional:
    case support::BuiltinFn::kIntegralFromInt:
    case support::BuiltinFn::kIntegralConvert:
      return OverIntegralValues{};

    case support::BuiltinFn::kExists:
    case support::BuiltinFn::kAssocFirst:
    case support::BuiltinFn::kAssocLast:
    case support::BuiltinFn::kAssocNext:
    case support::BuiltinFn::kAssocPrev:
    case support::BuiltinFn::kAssocMinIndex:
    case support::BuiltinFn::kAssocMaxIndex:
    case support::BuiltinFn::kAssocWithDefault:
    case support::BuiltinFn::kAssocHasStatedDefault:
    case support::BuiltinFn::kAssocAbsentValue:
      return NamedByValue{.realized = kForAssociativeArrays};

    // LRM 7.6 assignment between unpacked array kinds, one entry per
    // destination kind, reads its source through the representation the source
    // has.
    case support::BuiltinFn::kUnpackedArrayFromArray:
      return NamedByValue{.realized = kForDynamicArraysAndQueues};
    case support::BuiltinFn::kQueueFromArray:
      return NamedByValue{.realized = kForSequences};
    case support::BuiltinFn::kElementSlice:
    case support::BuiltinFn::kAssignElementSlice:
      return NamedByValue{.realized = kForArraysOfFixedExtent};
    case support::BuiltinFn::kDynamicArrayFromArray:
      return NamedByValue{.realized = kForUnpackedArraysAndQueues};

    case support::BuiltinFn::kDelete:
      return NamedByValue{.realized = kForWhatDeletesWhole};

    case support::BuiltinFn::kReverse:
    case support::BuiltinFn::kSort:
      return NamedByValue{.realized = kForSequences};
    // An element of a fixed-size unpacked array lies where arithmetic on the
    // array's address says, so no entry is asked for one, to read it or to
    // write it.
    case support::BuiltinFn::kElement:
      return NamedByValue{.realized = kForDynamicArraysAndQueues};
    case support::BuiltinFn::kElementMut:
      return NamedByValue{.realized = kForDynamicArrays};

    case support::BuiltinFn::kSize:
      return NamedByValue{.realized = kForContainers};

    case support::BuiltinFn::kInsert:
    case support::BuiltinFn::kPopFront:
    case support::BuiltinFn::kPopBack:
    case support::BuiltinFn::kPushFront:
    case support::BuiltinFn::kPushBack:
    case support::BuiltinFn::kDeleteIndex:
      return NamedByValue{.realized = kForQueues};

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
    case support::BuiltinFn::kRound:
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

    case support::BuiltinFn::kBitIdentical:
      return NamedByValue{.realized = kForWhatAnEntryComparesWhole};
    case support::BuiltinFn::kCaseEqual:
    case support::BuiltinFn::kHasUnknown:
      return NamedByValue{.realized = kForWhatAnEntryComparesByOperations};
    case support::BuiltinFn::kBitstreamWidth:
    case support::BuiltinFn::kCountBits:
      return NamedByValue{.realized = kForBitstreams};
    case support::BuiltinFn::kToBitVec:
      return NamedByValue{.realized = kForRunsOfBits};
    // LRM 11.4.14.4: a run of an array's elements, which an array of the
    // library's own answers and a fixed-size one is told its type for.
    case support::BuiltinFn::kWindowToBitVec:
      return NamedByValue{.realized = kForSequencesByOperations};
    case support::BuiltinFn::kWithWindow:
      return NamedByValue{.realized = kForSequences};
    // What a value laid out by its type answers -- a structure, a union, a
    // fixed-size array -- which code compiled for the type answers, and
    // nothing the library holds does; an integral value's is its own entry.
    case support::BuiltinFn::kToBitstream:
    case support::BuiltinFn::kResolveTriState:
    case support::BuiltinFn::kResolveWiredAnd:
    case support::BuiltinFn::kResolveWiredOr:
    case support::BuiltinFn::kDominating:
    case support::BuiltinFn::kFilledLike:
    case support::BuiltinFn::kTakePositions:
    // A value holding a bounded queue below its outermost dimension, taken
    // into the form of its type with no queue bounded, which the unit's body
    // for the type answers whatever the outer kind.
    case support::BuiltinFn::kToBoundFree:
      return NamedByValue{.realized = {}};
    case support::BuiltinFn::kMergeConditional:
      return NamedByValue{.realized = kForSequencesByOperations};
    // What a structure's declaration states, which the structure's own
    // generated body answers and nothing the library holds does.
    case support::BuiltinFn::kDeclaredDefault:
    case support::BuiltinFn::kPositionCount:
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
    // LRM 6.24.3: a dynamic array or a queue holding as many elements as fill
    // a run of bits, which is told the type of the array it builds.
    case support::BuiltinFn::kFromBitVec:
      return NamedByResult{.realized = kForDynamicArraysAndQueuesByOperations};
    case support::BuiltinFn::kFromBits:
    case support::BuiltinFn::kFromInt:
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
    // What a dump starting is told of a member (LRM 21.7.1.2): the wrapper
    // decides where the value lies and where a change to it is reported, and
    // the constant beside it states the declaration, its width included, so
    // nothing more is told.
    case support::BuiltinFn::kDescribe:
      return NamedByWrapper{.realized = kDescribed};
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
    // A driver carrying the whole of an object is told so, and asked at its
    // turn whether its net now shows that object; the driver it acts on names
    // the entry.
    case support::BuiltinFn::kDriveFromVariable:
    case support::BuiltinFn::kDriveFromNet:
    case support::BuiltinFn::kDriveFromReference:
    case support::BuiltinFn::kShowSource:
      return NamedByWrapper{.realized = kDrivesFromAnObject};
    // Each wrapper answers where a wait on it enrols, so the wrapper names the
    // entry.
    case support::BuiltinFn::kReportsTo:
      return NamedByWrapper{.realized = kWaitedOn};

    // A driver is attached by the net that issues it, a fold is installed on
    // the net that applies it, and a join takes two nets that resolve in the
    // same representation, so in each case what names the entry is the
    // representation those nets resolve in. A history's three operations
    // likewise take the storage they act on and are named by the one domain
    // every value in it is realized in (LRM 16.9.3).
    case support::BuiltinFn::kAttachDriver:
      return NamedByStorageDomain{.realized = kForNets};
    case support::BuiltinFn::kNetJoin:
      return NamedByStorageDomain{.realized = kForJoinedNets};
    case support::BuiltinFn::kNetInitializeTriState:
    case support::BuiltinFn::kNetInitializeWiredAnd:
    case support::BuiltinFn::kNetInitializeWiredOr:
    case support::BuiltinFn::kNetInitializeRetaining:
      return NamedByStorageDomain{.realized = kForIntegralNets};
    case support::BuiltinFn::kAggregateNetInitializeTriState:
    case support::BuiltinFn::kAggregateNetInitializeWiredAnd:
    case support::BuiltinFn::kAggregateNetInitializeWiredOr:
    case support::BuiltinFn::kAggregateNetInitializeRetaining:
      return NamedByStorageDomain{.realized = kForAggregateNets};
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
      return NamedByStorageDomain{.realized = kForArraysDesignatedInto};
    case support::BuiltinFn::kReferElement:
      return NamedByStorageDomain{.realized = kForSequencesSteppedInto};
    // A call's binding leases what it steps into only where the program can
    // let it go: an element of a sequence the library holds, never one laid
    // out where its array lies.
    case support::BuiltinFn::kReferElementLeased:
      return NamedByStorageDomain{.realized = kForDynamicArraysAndQueues};
    case support::BuiltinFn::kDesignateComponent:
      return NamedByStorageDomain{.realized = kForComponentsSteppedInto};
    case support::BuiltinFn::kDesignateMember:
      return NamedByStorageDomain{.realized = kForMembersSteppedInto};
    case support::BuiltinFn::kReferComponent:
      return NamedByStorageDomain{.realized = kForComponentsReferredTo};

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

    // The lowering reads the closure as the erased one in place of this call.
    case support::BuiltinFn::kUnsize:
      return NotRealized{.shape = kErasedWhereItLies};

    // The runtime leads, and the memory whose addressing names the entry
    // follows.
    case support::BuiltinFn::kReadMem:
    case support::BuiltinFn::kReadMemWithin:
    case support::BuiltinFn::kReadEnumeratedMem:
    case support::BuiltinFn::kReadEnumeratedMemWithin:
      return NamedByValue{.operand = 1, .realized = kForLoadedMemories};
    case support::BuiltinFn::kWriteMem:
    case support::BuiltinFn::kWriteMemWithin:
      return NamedByValue{.operand = 1, .realized = kForContainers};

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
    case support::BuiltinFn::kDeferredEffectsAllocate:
    case support::BuiltinFn::kDeferredEffectsCommit:
    case support::BuiltinFn::kAssertionControlAllocate:
    case support::BuiltinFn::kAssertionControlCommit:
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
    case support::BuiltinFn::kFileReadMemory:
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
    case support::BuiltinFn::kDescribeEvent:
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
    case support::BuiltinFn::kWaitOnImplicitList:
    case support::BuiltinFn::kWaitOnPropertyEvaluation:
    case support::BuiltinFn::kAttemptsOf:
    case support::BuiltinFn::kBindPropertyEvaluation:
    case support::BuiltinFn::kDisablePropertyEvaluation:
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
    case support::BuiltinFn::kRegisterAlways:
    case support::BuiltinFn::kRegisterObserving:
    case support::BuiltinFn::kRegisterContinuousDriver:
    case support::BuiltinFn::kRegisterInitial:
    case support::BuiltinFn::kRegisterTriggered:
    case support::BuiltinFn::kRegisterFinal:
    case support::BuiltinFn::kRunAssertionAction:
    case support::BuiltinFn::kEnterReactionRunningFirst:
    case support::BuiltinFn::kEnterReactionStoppingFirst:
    case support::BuiltinFn::kEnterRunToCompletion:
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
    case support::BuiltinFn::kDynamicArrayNew:
    case support::BuiltinFn::kDynamicArrayNewCopy:
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
    case support::BuiltinFn::kEvaluationAttemptsSeedWord:
    case support::BuiltinFn::kEvaluationAttemptsOpenAttempt:
    case support::BuiltinFn::kEvaluationAttemptsBeginTick:
    case support::BuiltinFn::kEvaluationAttemptsDisableTick:
    case support::BuiltinFn::kEvaluationAttemptsLiveWord:
    case support::BuiltinFn::kEvaluationAttemptsNextUnstepped:
    case support::BuiltinFn::kEvaluationAttemptsBitsAt:
    case support::BuiltinFn::kEvaluationAttemptsSetWord:
    case support::BuiltinFn::kEvaluationAttemptsStep:
    case support::BuiltinFn::kEvaluationAttemptsSeed:
    case support::BuiltinFn::kEvaluationAttemptsCount:
    case support::BuiltinFn::kEvaluationAttemptsPartOf:
    case support::BuiltinFn::kEvaluationAttemptsImplicationAnswer:
    case support::BuiltinFn::kEvaluationAttemptsImplicationNonvacuous:
    case support::BuiltinFn::kEvaluationAttemptsResolve:
    case support::BuiltinFn::kEvaluationAttemptsSettle:
    case support::BuiltinFn::kEvaluationAttemptsAnswer:
    case support::BuiltinFn::kEvaluationAttemptsActionRuns:
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
    case support::BuiltinFn::kDpiOpenArrayValue:
    // An operation whose own name says the one kind of value it is over or
    // builds: a queue's slice, read or written (LRM 7.10.1, 7.6), a string
    // repeated or built out of bits (LRM 11.4.12.1, 6.16), an unpacked array of
    // byte built out of text or of bits (LRM 5.9); a write of a queue's
    // element (LRM 7.10.5); an access into an associative array by its key
    // (LRM 7.8).
    case support::BuiltinFn::kQueueElementMut:
    case support::BuiltinFn::kQueueDesignateElement:
    case support::BuiltinFn::kAssocElement:
    case support::BuiltinFn::kAssocElementMut:
    case support::BuiltinFn::kAssocDesignateElement:
    case support::BuiltinFn::kAssocReferElement:
    case support::BuiltinFn::kAssocReferElementLeased:
    case support::BuiltinFn::kAssocDeleteIndex:
    case support::BuiltinFn::kQueueSlice:
    case support::BuiltinFn::kAssignQueueSlice:
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

auto StatesOperationFunctions(support::ValueDomain domain) -> bool {
  return kLibraryContainers.Holds(domain);
}

auto StatesValueOperations(support::ValueDomain domain) -> bool {
  return kValuesComparedWhole.Holds(domain);
}

auto IndexOrderOf(support::ValueDomain domain) -> std::optional<IndexOrder> {
  if (kIntegral.Holds(domain)) {
    return IndexOrder::kIntegral;
  }
  if (kIndexKinds.Holds(domain)) {
    return IndexOrder::kOwn;
  }
  return std::nullopt;
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
