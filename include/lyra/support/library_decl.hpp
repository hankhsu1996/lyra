#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <type_traits>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/support/movability.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::support {

// A type the runtime library declares, which the IR names together with the
// arguments the language gives it, as rustc's `Adt(def, args)` names a
// definition of a library crate: nothing the IR builds makes, takes apart or
// lays out a value of one, and what a rule of the language reads of one -- a
// container's element type, its index type, a queue's bound -- is an argument
// its declaration names. Every layer asks what it needs of one of its
// declaration, which states it once.
//
// Two sides name it: the compiler's layers, which name one and read its
// declaration, and the runtime, which defines the class each declaration
// stands for and checks the storage the declaration states against it. So the
// enumeration lives beside both rather than in either.
enum class LibraryDecl : std::uint8_t {
  // The values of the design whose representation is the library's: a string
  // (LRM 6.16), a value of any type kept with the identity it is tested by when
  // it is asked to become a value of another (LRM 15.4.9), and the containers
  // whose size changes as the program runs -- a dynamic array (LRM 7.5), a
  // queue and the bound it may hold up to (LRM 7.10), and an associative array
  // and the type it is indexed by (LRM 7.8).
  kString,
  kAny,
  kDynamicArray,
  kQueue,
  kAssociativeArray,
  // A run of bits whose length is part of the value rather than of the type,
  // the first most significant: the stream LRM 6.24.3 and 11.4.14.1 make of a
  // value whose bit count only the running program has. It is a value, copied
  // and handed on as any value is, as Rust's `BitVec` and LLVM's `BitVector`
  // are; how far a read of it has reached is kept by the reader, never by the
  // run.
  kBitVec,
  // LRM 7.8.1 the index of an associative array indexed by any integral value,
  // identified by magnitude whatever the width of the expression naming it,
  // and so kept with the type that expression carried.
  kWildcardIndex,
  // A weak reference to a garbage-collected object (LRM 8.30), as Rust's
  // `Weak` and C++'s `std::weak_ptr` are to their strong pointers: an edge the
  // collector does not follow, which reads no object once its target is
  // unreachable. Its argument is the class it refers to an object of.
  kWeakReference,

  // What a print is assembled from: a value read where it lies with the
  // conversion it is printed by, and that conversion.
  kPrintItem,
  kFormatSpec,
  // LRM 21.3.3 an operand of a format string known only where the program
  // runs, which binds each operand to a conversion as it is parsed, so it is
  // the value alone.
  kFormatArg,
  // LRM 20.4.3 how `%t` prints, as `$timeformat` last set it.
  kTimeFormat,
  // LRM 23.3.3.5 / 27.6 the identity a child of the hierarchy carries from
  // its construction: the declaration it fills and its index in each
  // dimension.
  kHierarchySegment,
  // LRM 9.4.2 one leaf of a value-change wait: the cell, the bits of its
  // encoding the wait reads, and what decides whether a change there is an
  // event.
  kTrigger,
  // LRM 9.4.2 what an event control watches while a procedure waits at it:
  // the expression deciding the event and the value it had when the wait
  // began.
  kObservation,
  // LRM 9.4.2 what one evaluation a waiting process makes reached, which every
  // function it calls reports what it reads into; an `always_comb` or
  // `always_latch` collects its implicit list into one (LRM 9.2.2.2.1).
  kReadReport,
  // LRM 9.4 a timing control as the body holds it, built so a stop reached
  // again stops at the same wait.
  kWait,
  // LRM 13.5.2 / 8.29 a reference a call binds where what it names lies in
  // something the program can let go of while the call runs, with a lease
  // keeping that thing in existence until at least the call returns.
  kLeasedReference,
  // LRM 21.3.2 the token a deferred write on a channel holds, which closing
  // the channel cancels.
  kChannelCancellation,
  // What a deferred body that evaluates what the source wrote takes where it
  // is made, so that it runs on no thread once its region runs it (LRM 21.2.2,
  // 18.14).
  kNoThreadContext,
  // The buffers a packed vector crosses the DPI-C boundary in (LRM 35.5.6,
  // Annex H.10.1.2), of `svBitVecVal` and of `svLogicVecVal` chunks, and the
  // image of a whole actual a DPI-C open array is (LRM 35.5.6.1, Annex H.12).
  kDpiBitBuffer,
  kDpiLogicBuffer,
  kDpiOpenArray,

  // The members an enumeration declares, each its whole value at the base
  // type and its name, in declared order, a table the unit states once.
  kEnumeration,
  // One canonical packed-vector chunk a value crosses the foreign boundary as
  // (LRM 35.5.6, Annex H.10.1.2), named as the pointee of the pointer an
  // export's C entry receives: a 32-bit value-plane word, or a two-plane
  // `{ aval, bval }` record. Never a value of the design.
  kDpiBitChunk,
  kDpiLogicChunk,
  // The handle the foreign side receives in place of an open array's image
  // (LRM Annex H.12), the only one of the two a prototype names.
  kDpiOpenArrayHandle,
  // LRM 9.4.2 a place a wait watches, or the member a reference was bound
  // into where the place was reached through one, so a wait follows the
  // member as a force rebinds it (LRM 10.6.2). One word.
  kWatchedPlace,
  // LRM 9.6.2 the effect raised by leaving a disabled scope, which the region
  // naming that scope binds and consumes.
  kControlEffect,

  // LRM 9.6.2 what a scope is named through by `disable`, carrying the
  // generation an execution captures on entry and `disable` advances.
  kCancellationTarget,
  // LRM 10.3.3 the one future event a continuous assignment carrying a delay
  // propagates through, armed again whenever its right-hand side changes.
  kTimer,
  // LRM 4.4 the effects one region of one time slot holds pending; a closure
  // a region keeps is made in the storage it hands out and kept there.
  kDeferredEffects,
  // LRM 16.9.3 what the ticks of one clocking event settled for one
  // expression. Its argument is the type of the values it keeps; how far back
  // it reaches is fixed by filling it, so it is no argument.
  kSampledHistory,
  // LRM 16.14.1 what one concurrent assertion has in flight: an attempt per
  // tick its clock reached and has not answered, and those matured and not
  // yet begun. An attempt's closure is made in the storage it hands out and
  // kept there, as a region's deferred effects keep theirs.
  kEvaluationAttempts,
  // LRM 16.14.1 one of those attempts, which its closure is handed while
  // it steps it and reaches only where it lies.
  kEvaluationAttempt,
  // LRM 20.11 what the assertion control tasks set for one assertion, expect
  // statement or violation report. It is also where the statement's violation
  // reports are kept until they mature (LRM 12.4.2.1), as a region's deferred
  // effects keep theirs.
  kAssertionControl,
  // LRM 27.4 the blocks one loop generate built, each under the value its
  // index stood at, which a name reaching one asks it for (LRM 23.6).
  kLoopBlocks,

  // What every object of one class carries, the parts a class's definition is
  // made of, what a constructor of the design hierarchy is handed and how
  // each of its objects is made, what a value dump may record (LRM 21.7.1.2),
  // and where in the source an operation that can fail the design was written
  // (LRM 20.10): the structures a unit states constants of, which the library
  // reads.
  kObjectDefinition,
  kScopeInfo,
  kDefinitionRecord,
  kElaboratedScopeRecord,
  kLexicalScopeRecord,
  kScopeDeclarations,
  kScopeCallable,
  kClassObjectInfo,
  kInterfaceRow,
  kInterfaceReach,
  kConstructionTable,
  kInstantiation,
  kGeneratePlaces,
  kDumpEntry,
  kSourceLocation,
  kSourceLocationRecord,

  // The facilities the runtime holds for the whole run, each reached as a
  // live reference: the effects a body runs under, the open files, and the
  // stream a diagnostic is written to.
  kRuntimeEffects,
  kFileTable,
  kDiagnosticDispatcher,

  // The classes of the runtime's whose objects generated code reaches but
  // never lays out: the scope an instance of the design hierarchy is (LRM
  // 23.3), the root every object the program makes starts with, the part an
  // object of a class starts with beyond it, its own generator (LRM 8.3,
  // 18.14.1), a process (LRM 9.7), the object an event names (LRM 6.17), and
  // the problem a call of `randomize()` fills (LRM 18.6). A class of the
  // source extends only the first three.
  kScope,
  kObjectRoot,
  kClassObject,
  kProcess,
  kSynchronizationObject,
  kConstraintProblem,
};

// What one parameter of a declaration takes. Most take a type. A container's
// say what a rule of the language reads its argument as: the type of the
// elements it holds a sequence of, the type an associative array is indexed
// by (LRM 7.8), and how many elements a queue may hold (LRM 7.10), which is a
// value rather than a type.
enum class LibraryParam : std::uint8_t {
  kType,
  kElement,
  kIndex,
  kBound,
};

// How many elements a queue may hold (LRM 7.10): no bound, or at most a
// stated count. It is part of the queue's type, so a type naming a queue
// carries it as the argument its declaration's bound parameter takes.
struct UnboundedQueue {
  auto operator==(const UnboundedQueue&) const -> bool = default;
};
struct BoundedQueue {
  std::uint64_t count;

  auto operator==(const BoundedQueue&) const -> bool = default;
};
using QueueBound = std::variant<UnboundedQueue, BoundedQueue>;

// The bound as the library's queue class is written over it, where a count
// below zero is no bound.
constexpr auto LibraryBoundOf(const QueueBound& bound) -> std::int64_t {
  return std::visit(
      Overloaded{
          [](const UnboundedQueue&) -> std::int64_t { return -1; },
          [](const BoundedQueue& most) {
            return static_cast<std::int64_t>(most.count);
          }},
      bound);
}

// How a holder holds a value of one, and the storage a holder gives it where
// it gives it storage of its own. A value of the design is an object of the
// value domain it is realized in, which the library builds there, copies and
// ends through that domain's entries. An object of the library's own is built,
// copied and ended through entries carrying the name it states, by value in
// storage a holder gives it, or only as a member of an object holding it
// whole. Storage elsewhere -- a table the run keeps, the run's own state,
// storage an owner holds, a facility -- is held as where it lies.
struct DomainObject {
  ValueDomain domain;
  ObjectLayout storage;
};
struct OwnObject {
  std::string_view entries;
  std::optional<ObjectLayout> by_value;
};
struct StorageElsewhere {};
using LibraryHolding = std::variant<DomainObject, OwnObject, StorageElsewhere>;

// What a member of an object holding one is. Most are no member's: a value
// built for one call, or one the run keeps. A value of the design is a member
// as every value is, in the slot its role asks for. An object of the
// declaration's own is a member as that object, built and ended by its own
// entries. A table outliving every object is a member that points at it. A
// history is a member as storage over the values it holds -- what each tick of
// one clocking event settled (LRM 16.9.3) -- whose type is the argument of the
// parameter it names.
struct NoMember {};
struct ValueMember {};
struct OwnObjectMember {};
struct BorrowedMember {};
struct HistoryMember {
  LibraryParam values;
};
using LibraryMember = std::variant<
    NoMember, ValueMember, OwnObjectMember, BorrowedMember, HistoryMember>;

// The constructor a construction of one reaches, and, for one built over a
// value it is handed a reference to, the constructor it reaches instead where
// that value is integral, which reads the value as a number (LRM 21.2.1).
struct LibraryConstructor {
  BuiltinFn entry;
  std::optional<BuiltinFn> integral_entry = std::nullopt;
};

// How one keeper of deferred effects keeps: the allocation function handing
// out storage for one effect, and the entry committing the effect made in it.
struct KeeperEntries {
  BuiltinFn allocate;
  BuiltinFn commit;
};

// How long an object lives: for the whole run, as an instance of the design
// hierarchy does, owned by the tree built at time zero (LRM 23.3), or while a
// handle refers to it, as an object the program makes does (LRM 8.29). A
// borrow of the first is what Rust writes `&'static T`, a borrow that is valid
// at every use; a borrow of the second is valid only while something keeps the
// object.
enum class ObjectLifetime : std::uint8_t { kTheRun, kWhileReferenced };

// A class of the runtime's whose objects generated code reaches but never lays
// out, which a class of the source may extend, placing its own storage after
// it: each is an object of the root every object the library holds is, which
// starts with its class's definition and is ended through it. Which one it is
// is the whole of its identity, so its declaration names it; what the
// declaration states besides is how long an object of it lives.
struct RuntimeClass {
  ObjectLifetime lifetime;
};

// Everything any layer asks of a type the library declares, stated once, as
// rustc's `AdtDefData` is what an `AdtDef` names.
//
// The semantic part says what the IR may do with a value of one: what it is
// parameterized by, whether a variable may be declared to hold one, whether a
// temporary may stand in for it, the entry building the value one starts as,
// and the entry a construction of one reaches.
//
// The ABI part says how a target holds and spells one: how a holder holds it,
// the storage it takes where it is held by value, which the runtime checks
// against every class a backend builds there, and the name the entries over an
// object of its own carry; what a member holding one is, and of a history the
// values it holds; the C++ type a holder names it by and the C spelling a
// DPI-C prototype names it by; the class of the runtime's it is, where it is
// an object of one; and how it keeps deferred effects, where it is a keeper.
struct LibraryDeclData {
  // What a reader is told the type is, in a dump or a message.
  std::string_view name;
  std::span<const LibraryParam> params;

  // A data type of the program (LRM 6.2): what a variable declaration names,
  // whose value answers every operation a value of the program does.
  bool is_a_data_type;
  Movability movability;
  // The entry building, out of no operands, the value one starts as (LRM
  // Table 6-7, Table 7-1), where one starts as a value rather than holding
  // nothing a declaration could ask for.
  std::optional<BuiltinFn> default_value;
  // What a construction of one reaches, where something constructs one.
  std::optional<LibraryConstructor> constructor;

  LibraryHolding holding;
  LibraryMember member;
  // The C++ type a holder names one by: its class, which is a class template
  // its arguments are written after where the arguments make it different
  // classes, and one class whatever its arguments otherwise -- as a class
  // handle is one class whatever class it refers to; or a reference to it,
  // where every holder reaches one facility.
  std::string_view cpp_spelling;
  bool cpp_spelling_takes_args;
  // The C spelling a DPI-C prototype names it by, where one does (LRM 35.5.6,
  // Annex H).
  std::optional<std::string_view> c_spelling;
  std::optional<RuntimeClass> runtime_class;
  std::optional<KeeperEntries> keeper;
};

// The declaration of `decl`, and nothing for a value of the enumeration
// naming no declaration, which only a walk over every declaration meets. It is
// a constant expression, so the runtime and every program built against it
// check at compile time that each class the library defines fits the storage
// its declaration states.
constexpr auto FindDeclaration(LibraryDecl decl) -> const LibraryDeclData*;

// The same of a value naming a declaration.
constexpr auto DeclarationOf(LibraryDecl decl) -> const LibraryDeclData& {
  const LibraryDeclData* declaration = FindDeclaration(decl);
  if (declaration == nullptr) {
    throw InternalError("support: unknown library declaration");
  }
  return *declaration;
}

// Hands `visit` every declaration, in the order they are enumerated, as a
// constant expression where `visit` is one, so a check over every declaration
// is made where the code holding it compiles.
template <typename Visit>
constexpr void ForEachLibraryDecl(const Visit& visit) {
  for (std::underlying_type_t<LibraryDecl> position = 0;
       FindDeclaration(static_cast<LibraryDecl>(position)) != nullptr;
       ++position) {
    visit(static_cast<LibraryDecl>(position));
  }
}

// The storage a holder gives an object of the library's by value -- a value
// of the design or an object of its own -- and nothing for a type held some
// other way.
constexpr auto ObjectStorageOf(LibraryDecl decl)
    -> std::optional<ObjectLayout> {
  using Stored = std::optional<ObjectLayout>;
  return std::visit(
      Overloaded{
          [](const DomainObject& held) -> Stored { return held.storage; },
          [](const OwnObject& held) -> Stored { return held.by_value; },
          [](const StorageElsewhere&) -> Stored { return std::nullopt; }},
      DeclarationOf(decl).holding);
}

// What the entries over an object of the declaration's own carry in their
// names. Only a declaration holding one is asked.
constexpr auto OwnObjectEntries(LibraryDecl decl) -> std::string_view {
  const auto* own = std::get_if<OwnObject>(&DeclarationOf(decl).holding);
  if (own == nullptr) {
    throw InternalError(
        "support: the entries of an object of its own are asked of a "
        "declaration stating none -- please report this as a bug");
  }
  return own->entries;
}

// Whether a value of one lies in storage of its own its holder keeps a claim
// on -- an object of a value domain -- as opposed to a payload built for the
// call reading it, storage the runtime keeps, or a reference.
auto IsDomainObject(LibraryDecl decl) -> bool;

// Whether one is storage with no value to read, consumed only where it lies:
// held by no holder by value, and never moved.
auto IsAddressOnly(LibraryDecl decl) -> bool;

// The position of the parameter of `decl` that takes `param`, and nothing
// where none does: the element type of a container, the index type of an
// associative array, a queue's bound.
auto ParamOf(LibraryDecl decl, LibraryParam param)
    -> std::optional<std::size_t>;

// Whether a parameter taking `param` takes a type, rather than a value.
auto TakesAType(LibraryParam param) -> bool;

constexpr auto FindDeclaration(LibraryDecl decl) -> const LibraryDeclData* {
  static constexpr std::array<LibraryParam, 1> kOneType{LibraryParam::kType};
  static constexpr std::array<LibraryParam, 1> kElements{
      LibraryParam::kElement};
  static constexpr std::array<LibraryParam, 2> kElementsUpToABound{
      LibraryParam::kElement, LibraryParam::kBound};
  static constexpr std::array<LibraryParam, 2> kElementsByIndex{
      LibraryParam::kIndex, LibraryParam::kElement};

  // A payload an entry builds for the one call that reads it, held by value in
  // the frame that makes it and ended there, which no member holds.
  static constexpr auto kPayload =
      [](std::string_view name, std::string_view entries, ObjectLayout storage,
         std::string_view cpp,
         LibraryMember member = NoMember{}) -> LibraryDeclData {
    return LibraryDeclData{
        .name = name,
        .params = {},
        .is_a_data_type = false,
        .movability = Movability::kMovable,
        .default_value = std::nullopt,
        .constructor = std::nullopt,
        .holding = OwnObject{.entries = entries, .by_value = storage},
        .member = member,
        .cpp_spelling = cpp,
        .cpp_spelling_takes_args = false,
        .c_spelling = std::nullopt,
        .runtime_class = std::nullopt,
        .keeper = std::nullopt};
  };

  // Something named by where it lies -- a table the run keeps, a structure a
  // unit states as a constant, the run's own state, what another entry
  // answers with -- which is handed on by moving the address.
  static constexpr auto kNamedByAddress =
      [](std::string_view name, std::string_view cpp,
         LibraryMember member = NoMember{}) -> LibraryDeclData {
    return LibraryDeclData{
        .name = name,
        .params = {},
        .is_a_data_type = false,
        .movability = Movability::kMovable,
        .default_value = std::nullopt,
        .constructor = std::nullopt,
        .holding = StorageElsewhere{},
        .member = member,
        .cpp_spelling = cpp,
        .cpp_spelling_takes_args = false,
        .c_spelling = std::nullopt,
        .runtime_class = std::nullopt,
        .keeper = std::nullopt};
  };

  // Storage an owner or the run keeps, reached only where it lies and never
  // moved, as rustc calls a value that may not be moved once made immovable.
  static constexpr auto kKeptStorage =
      [](std::string_view name, std::string_view cpp,
         LibraryMember member = NoMember{}) -> LibraryDeclData {
    LibraryDeclData declaration = kNamedByAddress(name, cpp, member);
    declaration.movability = Movability::kImmovable;
    return declaration;
  };

  // An object of its own an object holds as a member, which the library
  // builds and ends there through entries carrying `entries`, and which no
  // holder holds by value.
  static constexpr auto kKeptObject =
      [](std::string_view name, std::string_view entries,
         std::string_view cpp) -> LibraryDeclData {
    return LibraryDeclData{
        .name = name,
        .params = {},
        .is_a_data_type = false,
        .movability = Movability::kImmovable,
        .default_value = std::nullopt,
        .constructor = std::nullopt,
        .holding = OwnObject{.entries = entries, .by_value = std::nullopt},
        .member = OwnObjectMember{},
        .cpp_spelling = cpp,
        .cpp_spelling_takes_args = false,
        .c_spelling = std::nullopt,
        .runtime_class = std::nullopt,
        .keeper = std::nullopt};
  };

  // A facility the runtime holds for the whole run, reached as a live
  // reference any number of holders copy.
  static constexpr auto kFacility =
      [](std::string_view name, std::string_view cpp) -> LibraryDeclData {
    LibraryDeclData declaration = kNamedByAddress(name, cpp);
    declaration.movability = Movability::kCopyable;
    return declaration;
  };

  // An object of a class of the runtime's, which is an object of the root and
  // ended through its class's definition, so it is reached where it lies.
  static constexpr auto kObjectOfClass =
      [](std::string_view name, RuntimeClass klass,
         std::string_view cpp) -> LibraryDeclData {
    LibraryDeclData declaration = kKeptStorage(name, cpp);
    declaration.runtime_class = klass;
    return declaration;
  };

  // A keeper a deferred closure is emplaced in, which the run keeps.
  static constexpr auto kKeeper = [](std::string_view name,
                                     std::string_view cpp,
                                     KeeperEntries entries) -> LibraryDeclData {
    LibraryDeclData declaration = kKeptStorage(name, cpp);
    declaration.keeper = entries;
    return declaration;
  };

  // `declaration`, which a construction of it builds by `constructor`.
  static constexpr auto kBuiltBy =
      [](LibraryDeclData declaration,
         LibraryConstructor constructor) -> LibraryDeclData {
    declaration.constructor = constructor;
    return declaration;
  };

  // A value of the design the library holds as an object of its value domain:
  // what a variable may be declared to hold, copied whole, a member holding
  // it as the value it is, and starting as what `default_value` builds.
  static constexpr auto kDesignValue =
      [](std::string_view name, std::span<const LibraryParam> params,
         ValueDomain domain, ObjectLayout storage, std::string_view cpp,
         BuiltinFn default_value) -> LibraryDeclData {
    return LibraryDeclData{
        .name = name,
        .params = params,
        .is_a_data_type = true,
        .movability = Movability::kCopyable,
        .default_value = default_value,
        .constructor = std::nullopt,
        .holding = DomainObject{.domain = domain, .storage = storage},
        .member = ValueMember{},
        .cpp_spelling = cpp,
        .cpp_spelling_takes_args = !params.empty(),
        .c_spelling = std::nullopt,
        .runtime_class = std::nullopt,
        .keeper = std::nullopt};
  };

  // A chunk a canonical buffer's pointer addresses, spelled as the C side
  // spells it.
  static constexpr auto kDpiNamed =
      [](std::string_view name, std::string_view spelling) -> LibraryDeclData {
    LibraryDeclData declaration = kNamedByAddress(name, spelling);
    declaration.c_spelling = spelling;
    return declaration;
  };

  // Storage of `size` bytes at the alignment of a word, which every object of
  // the library's but one takes, where ending an object does something, and
  // where it does nothing.
  static constexpr auto kWordsNeedingEnd = [](std::size_t size) {
    return ObjectLayout{
        .layout = {.size = size, .align = 8}, .needs_end = true};
  };
  static constexpr auto kWordsNeedingNoEnd = [](std::size_t size) {
    return ObjectLayout{
        .layout = {.size = size, .align = 8}, .needs_end = false};
  };

  switch (decl) {
    // LRM Table 6-7: a string starts empty, and a dynamic array, a queue and
    // an associative array start with no elements (LRM Table 7-1); a string
    // is built from its text, and a container from its elements or its
    // entries. A queue's bound is a value of its class template, and an
    // associative array is written over its index type first, as a map is
    // over its key.
    case LibraryDecl::kString: {
      static constexpr LibraryDeclData kString = kBuiltBy(
          kDesignValue(
              "string", {}, ValueDomain::kString, kWordsNeedingEnd(32),
              "lyra::value::String", BuiltinFn::kDefault),
          LibraryConstructor{.entry = BuiltinFn::kStringFromText});
      return &kString;
    }
    case LibraryDecl::kDynamicArray: {
      static constexpr LibraryDeclData kDynamicArray = kBuiltBy(
          kDesignValue(
              "dynamic array", kElements, ValueDomain::kDynArray,
              kWordsNeedingEnd(32), "lyra::value::DynamicArray",
              BuiltinFn::kContainerDefault),
          LibraryConstructor{.entry = BuiltinFn::kSequenceFromList});
      return &kDynamicArray;
    }
    case LibraryDecl::kQueue: {
      static constexpr LibraryDeclData kQueue = kBuiltBy(
          kDesignValue(
              "queue", kElementsUpToABound, ValueDomain::kQueue,
              kWordsNeedingEnd(56), "lyra::value::Queue",
              BuiltinFn::kContainerDefault),
          LibraryConstructor{.entry = BuiltinFn::kSequenceFromList});
      return &kQueue;
    }
    case LibraryDecl::kAssociativeArray: {
      static constexpr LibraryDeclData kAssociativeArray = kBuiltBy(
          kDesignValue(
              "associative array", kElementsByIndex, ValueDomain::kAssocArray,
              kWordsNeedingEnd(72), "lyra::value::AssociativeArray",
              BuiltinFn::kContainerDefault),
          LibraryConstructor{.entry = BuiltinFn::kAssociativeArrayFromEntries});
      return &kAssociativeArray;
    }
    // A value of any type is what a mailbox's own messages are where it
    // states none (LRM 15.4.9), so only the mailbox holds one and no
    // declaration names one. Before one is put it holds no value of any type.
    case LibraryDecl::kAny: {
      static constexpr LibraryDeclData kAny = [] {
        LibraryDeclData declaration = kDesignValue(
            "value of any type", {}, ValueDomain::kAny, kWordsNeedingEnd(24),
            "lyra::value::Any", BuiltinFn::kDefault);
        declaration.is_a_data_type = false;
        return declaration;
      }();
      return &kAny;
    }
    // A run of bits is built where a stream is made of a value, starting from
    // one of no bits, so no declaration names one.
    case LibraryDecl::kBitVec: {
      static constexpr LibraryDeclData kBitVec = [] {
        LibraryDeclData declaration = kDesignValue(
            "bit vector", {}, ValueDomain::kBitVec, kWordsNeedingEnd(32),
            "lyra::value::BitVec", BuiltinFn::kDefault);
        declaration.is_a_data_type = false;
        return declaration;
      }();
      return &kBitVec;
    }
    // An index is never moved into a temporary: it is built where the access
    // reading it is made, out of the integral value it is.
    case LibraryDecl::kWildcardIndex: {
      static constexpr LibraryDeclData kWildcardIndex{
          .name = "wildcard index",
          .params = {},
          .is_a_data_type = false,
          .movability = Movability::kImmovable,
          .default_value = std::nullopt,
          .constructor =
              LibraryConstructor{.entry = BuiltinFn::kWildcardIndexMake},
          .holding =
              DomainObject{
                  .domain = ValueDomain::kWildcardIndex,
                  .storage = kWordsNeedingEnd(80)},
          .member = NoMember{},
          .cpp_spelling = "lyra::value::WildcardKey",
          .cpp_spelling_takes_args = false,
          .c_spelling = std::nullopt,
          .runtime_class = std::nullopt,
          .keeper = std::nullopt};
      return &kWildcardIndex;
    }
    // A weak reference is a hold of the library's rather than a value of the
    // design, held by whatever holds it as a counted hold is, and it starts
    // out referring to nothing (LRM 8.30).
    case LibraryDecl::kWeakReference: {
      static constexpr LibraryDeclData kWeakReference = [] {
        LibraryDeclData declaration = kPayload(
            "weak reference", "weak_reference", kWordsNeedingEnd(16),
            "lyra::runtime::WeakObjectRef", OwnObjectMember{});
        declaration.params = kOneType;
        declaration.movability = Movability::kCopyable;
        declaration.default_value = BuiltinFn::kWeakReferenceDefault;
        return declaration;
      }();
      return &kWeakReference;
    }

    case LibraryDecl::kPrintItem: {
      static constexpr LibraryDeclData kPrintItem = kBuiltBy(
          kPayload(
              "print item", "print_item", kWordsNeedingNoEnd(72),
              "lyra::value::PrintItem"),
          LibraryConstructor{
              .entry = BuiltinFn::kPrintItemMake,
              .integral_entry = BuiltinFn::kIntegralPrintItemMake});
      return &kPrintItem;
    }
    case LibraryDecl::kFormatSpec: {
      static constexpr LibraryDeclData kFormatSpec = kBuiltBy(
          kPayload(
              "format specification", "format_spec",
              ObjectLayout{
                  .layout = {.size = 20, .align = 4}, .needs_end = false},
              "lyra::value::FormatSpec"),
          LibraryConstructor{.entry = BuiltinFn::kFormatSpecMake});
      return &kFormatSpec;
    }
    case LibraryDecl::kFormatArg: {
      static constexpr LibraryDeclData kFormatArg = kBuiltBy(
          kPayload(
              "format argument", "format_arg", kWordsNeedingNoEnd(40),
              "lyra::value::FormatArg"),
          LibraryConstructor{
              .entry = BuiltinFn::kFormatArgMake,
              .integral_entry = BuiltinFn::kIntegralFormatArgMake});
      return &kFormatArg;
    }
    // The time format is the run's own state, which a print reads where the
    // run keeps it.
    case LibraryDecl::kTimeFormat: {
      static constexpr LibraryDeclData kTimeFormat =
          kNamedByAddress("time format", "lyra::value::TimeFormat");
      return &kTimeFormat;
    }
    case LibraryDecl::kHierarchySegment: {
      static constexpr LibraryDeclData kHierarchySegment = kBuiltBy(
          kPayload(
              "hierarchy segment", "hierarchy_segment", kWordsNeedingEnd(32),
              "lyra::runtime::HierarchySegment"),
          LibraryConstructor{.entry = BuiltinFn::kHierarchySegmentMake});
      return &kHierarchySegment;
    }
    case LibraryDecl::kTrigger: {
      static constexpr LibraryDeclData kTrigger = kBuiltBy(
          kPayload(
              "trigger", "trigger", kWordsNeedingEnd(80),
              "lyra::runtime::Trigger"),
          LibraryConstructor{.entry = BuiltinFn::kTriggerMake});
      return &kTrigger;
    }
    case LibraryDecl::kObservation: {
      static constexpr LibraryDeclData kObservation = kPayload(
          "observation", "observation", kWordsNeedingEnd(16),
          "lyra::runtime::Observation");
      return &kObservation;
    }
    case LibraryDecl::kReadReport: {
      static constexpr LibraryDeclData kReadReport = kPayload(
          "read report", "read_report", kWordsNeedingEnd(120),
          "lyra::runtime::ReadReport");
      return &kReadReport;
    }
    case LibraryDecl::kWait: {
      static constexpr LibraryDeclData kWait =
          kPayload("wait", "wait", kWordsNeedingEnd(8), "lyra::runtime::Wait");
      return &kWait;
    }
    // A reference a call binds is copied together with the lease keeping
    // what it names, which a local of the frame keeps as the binding would
    // (LRM 13.5.2).
    case LibraryDecl::kLeasedReference: {
      static constexpr LibraryDeclData kLeasedReference = [] {
        LibraryDeclData declaration = kPayload(
            "leased reference", "leased_reference", kWordsNeedingEnd(112),
            "lyra::runtime::LeasedReference");
        declaration.movability = Movability::kCopyable;
        return declaration;
      }();
      return &kLeasedReference;
    }
    // The cancel state a deferred write holds is a member of the scope owning
    // the write, as storage of its own.
    case LibraryDecl::kChannelCancellation: {
      static constexpr LibraryDeclData kChannelCancellation = kPayload(
          "channel cancellation", "channel_cancellation", kWordsNeedingEnd(24),
          "lyra::runtime::ChannelCancellation", OwnObjectMember{});
      return &kChannelCancellation;
    }
    case LibraryDecl::kNoThreadContext: {
      static constexpr LibraryDeclData kNoThreadContext = kPayload(
          "no-thread context", "no_thread_context", kWordsNeedingNoEnd(88),
          "lyra::runtime::NoThreadContext", OwnObjectMember{});
      return &kNoThreadContext;
    }
    case LibraryDecl::kDpiBitBuffer: {
      static constexpr LibraryDeclData kDpiBitBuffer = kBuiltBy(
          kPayload(
              "DPI canonical bit buffer", "dpi_bit_buffer",
              kWordsNeedingEnd(24), "lyra::value::DpiBitBuffer"),
          LibraryConstructor{.entry = BuiltinFn::kDpiBitBufferMake});
      return &kDpiBitBuffer;
    }
    case LibraryDecl::kDpiLogicBuffer: {
      static constexpr LibraryDeclData kDpiLogicBuffer = kBuiltBy(
          kPayload(
              "DPI canonical logic buffer", "dpi_logic_buffer",
              kWordsNeedingEnd(24), "lyra::value::DpiLogicBuffer"),
          LibraryConstructor{.entry = BuiltinFn::kDpiLogicBufferMake});
      return &kDpiLogicBuffer;
    }
    case LibraryDecl::kDpiOpenArray: {
      static constexpr LibraryDeclData kDpiOpenArray = kBuiltBy(
          kPayload(
              "DPI open array", "dpi_open_array", kWordsNeedingEnd(64),
              "lyra::value::DpiOpenArray"),
          LibraryConstructor{.entry = BuiltinFn::kDpiOpenArrayMake});
      return &kDpiOpenArray;
    }
    // A table a unit states as a constant, which outlives every reader, so a
    // member naming one points at it rather than owning a copy.
    case LibraryDecl::kEnumeration: {
      static constexpr LibraryDeclData kEnumeration = kNamedByAddress(
          "enumeration", "lyra::value::Enumeration", BorrowedMember{});
      return &kEnumeration;
    }
    case LibraryDecl::kDpiBitChunk: {
      static constexpr LibraryDeclData kDpiBitChunk =
          kDpiNamed("DPI bit chunk", "svBitVecVal");
      return &kDpiBitChunk;
    }
    case LibraryDecl::kDpiLogicChunk: {
      static constexpr LibraryDeclData kDpiLogicChunk =
          kDpiNamed("DPI logic chunk", "svLogicVecVal");
      return &kDpiLogicChunk;
    }
    // LRM Annex H.8.6: an open array is passed by handle in either direction,
    // and the handle carries a `const` qualifier because the foreign side may
    // not modify it.
    case LibraryDecl::kDpiOpenArrayHandle: {
      static constexpr LibraryDeclData kDpiOpenArrayHandle =
          kDpiNamed("DPI open array handle", "const svOpenArrayHandle");
      return &kDpiOpenArrayHandle;
    }
    // One word, crossing to the library as that word, which a copy holds
    // whole.
    case LibraryDecl::kWatchedPlace: {
      static constexpr LibraryDeclData kWatchedPlace = [] {
        LibraryDeclData declaration =
            kNamedByAddress("watched place", "lyra::runtime::WatchedPlace");
        declaration.movability = Movability::kCopyable;
        return declaration;
      }();
      return &kWatchedPlace;
    }
    case LibraryDecl::kControlEffect: {
      static constexpr LibraryDeclData kControlEffect =
          kNamedByAddress("control effect", "lyra::runtime::ControlEffect");
      return &kControlEffect;
    }

    case LibraryDecl::kCancellationTarget: {
      static constexpr LibraryDeclData kCancellationTarget = kKeptObject(
          "cancellation target", "cancellation_target",
          "lyra::runtime::CancellationTarget");
      return &kCancellationTarget;
    }
    case LibraryDecl::kTimer: {
      static constexpr LibraryDeclData kTimer =
          kKeptObject("timer", "timer", "lyra::runtime::Timer");
      return &kTimer;
    }
    case LibraryDecl::kDeferredEffects: {
      static constexpr LibraryDeclData kDeferredEffects = kKeeper(
          "deferred effects", "lyra::runtime::DeferredEffects",
          KeeperEntries{
              .allocate = BuiltinFn::kDeferredEffectsAllocate,
              .commit = BuiltinFn::kDeferredEffectsCommit});
      return &kDeferredEffects;
    }
    case LibraryDecl::kSampledHistory: {
      static constexpr LibraryDeclData kSampledHistory = [] {
        LibraryDeclData declaration = kKeptStorage(
            "sampled value history", "lyra::runtime::SampledHistory",
            HistoryMember{.values = LibraryParam::kType});
        declaration.params = kOneType;
        declaration.cpp_spelling_takes_args = true;
        return declaration;
      }();
      return &kSampledHistory;
    }
    case LibraryDecl::kEvaluationAttempts: {
      static constexpr LibraryDeclData kEvaluationAttempts = [] {
        LibraryDeclData declaration = kKeptObject(
            "concurrent assertion attempts", "evaluation_attempts",
            "lyra::runtime::EvaluationAttempts");
        declaration.keeper = KeeperEntries{
            .allocate = BuiltinFn::kEvaluationAttemptsAllocate,
            .commit = BuiltinFn::kEvaluationAttemptsCommit};
        return declaration;
      }();
      return &kEvaluationAttempts;
    }
    case LibraryDecl::kEvaluationAttempt: {
      static constexpr LibraryDeclData kEvaluationAttempt = kKeptStorage(
          "concurrent assertion attempt", "lyra::runtime::EvaluationAttempt");
      return &kEvaluationAttempt;
    }
    case LibraryDecl::kAssertionControl: {
      static constexpr LibraryDeclData kAssertionControl = [] {
        LibraryDeclData declaration = kKeptObject(
            "assertion control", "assertion_control",
            "lyra::runtime::AssertionControl");
        declaration.keeper = KeeperEntries{
            .allocate = BuiltinFn::kAssertionControlAllocate,
            .commit = BuiltinFn::kAssertionControlCommit};
        return declaration;
      }();
      return &kAssertionControl;
    }
    case LibraryDecl::kLoopBlocks: {
      static constexpr LibraryDeclData kLoopBlocks = kKeptObject(
          "loop generate blocks", "loop_blocks", "lyra::runtime::LoopBlocks");
      return &kLoopBlocks;
    }

    // A class's definition is one per class for the whole run and every
    // object of it shares it, so a member naming one points at storage
    // outliving it.
    case LibraryDecl::kObjectDefinition: {
      static constexpr LibraryDeclData kObjectDefinition = kNamedByAddress(
          "object definition", "lyra::abi::ObjectDefinition", BorrowedMember{});
      return &kObjectDefinition;
    }
    case LibraryDecl::kScopeInfo: {
      static constexpr LibraryDeclData kScopeInfo =
          kNamedByAddress("scope info", "lyra::abi::ScopeInfo");
      return &kScopeInfo;
    }
    case LibraryDecl::kDefinitionRecord: {
      static constexpr LibraryDeclData kDefinitionRecord =
          kNamedByAddress("definition record", "lyra::abi::DefinitionRecord");
      return &kDefinitionRecord;
    }
    case LibraryDecl::kElaboratedScopeRecord: {
      static constexpr LibraryDeclData kElaboratedScopeRecord = kNamedByAddress(
          "elaborated scope record", "lyra::abi::ElaboratedScopeRecord");
      return &kElaboratedScopeRecord;
    }
    case LibraryDecl::kLexicalScopeRecord: {
      static constexpr LibraryDeclData kLexicalScopeRecord = kNamedByAddress(
          "lexical scope record", "lyra::abi::LexicalScopeRecord");
      return &kLexicalScopeRecord;
    }
    case LibraryDecl::kScopeDeclarations: {
      static constexpr LibraryDeclData kScopeDeclarations =
          kNamedByAddress("scope declarations", "lyra::abi::ScopeDeclarations");
      return &kScopeDeclarations;
    }
    case LibraryDecl::kScopeCallable: {
      static constexpr LibraryDeclData kScopeCallable =
          kNamedByAddress("scope callable", "lyra::abi::ScopeCallable");
      return &kScopeCallable;
    }
    case LibraryDecl::kClassObjectInfo: {
      static constexpr LibraryDeclData kClassObjectInfo =
          kNamedByAddress("class object info", "lyra::abi::ClassObjectInfo");
      return &kClassObjectInfo;
    }
    case LibraryDecl::kInterfaceRow: {
      static constexpr LibraryDeclData kInterfaceRow =
          kNamedByAddress("interface row", "lyra::abi::InterfaceRow");
      return &kInterfaceRow;
    }
    case LibraryDecl::kInterfaceReach: {
      static constexpr LibraryDeclData kInterfaceReach =
          kNamedByAddress("interface reach", "lyra::abi::InterfaceReach");
      return &kInterfaceReach;
    }
    case LibraryDecl::kConstructionTable: {
      static constexpr LibraryDeclData kConstructionTable =
          kNamedByAddress("construction table", "lyra::abi::ConstructionTable");
      return &kConstructionTable;
    }
    case LibraryDecl::kInstantiation: {
      static constexpr LibraryDeclData kInstantiation =
          kNamedByAddress("instantiation", "lyra::abi::Instantiation");
      return &kInstantiation;
    }
    case LibraryDecl::kGeneratePlaces: {
      static constexpr LibraryDeclData kGeneratePlaces =
          kNamedByAddress("generate places", "lyra::abi::GeneratePlaces");
      return &kGeneratePlaces;
    }
    case LibraryDecl::kDumpEntry: {
      static constexpr LibraryDeclData kDumpEntry =
          kNamedByAddress("dump entry", "lyra::abi::DumpEntry");
      return &kDumpEntry;
    }
    // One word, the address of the constant a unit states for the location,
    // which a copy holds whole and a member holds as that word.
    case LibraryDecl::kSourceLocation: {
      static constexpr LibraryDeclData kSourceLocation = [] {
        LibraryDeclData declaration = kNamedByAddress(
            "source location", "lyra::base::SourceLocation", BorrowedMember{});
        declaration.movability = Movability::kCopyable;
        return declaration;
      }();
      return &kSourceLocation;
    }
    // What such a value names: the file, the line and the column, which a unit
    // states as a constant of its own.
    case LibraryDecl::kSourceLocationRecord: {
      static constexpr LibraryDeclData kSourceLocationRecord = kNamedByAddress(
          "source location record", "lyra::base::SourceLocation::Impl");
      return &kSourceLocationRecord;
    }

    case LibraryDecl::kRuntimeEffects: {
      static constexpr LibraryDeclData kRuntimeEffects =
          kFacility("runtime effects", "lyra::runtime::RuntimeEffects&");
      return &kRuntimeEffects;
    }
    case LibraryDecl::kFileTable: {
      static constexpr LibraryDeclData kFileTable =
          kFacility("file table", "lyra::runtime::FileTable&");
      return &kFileTable;
    }
    case LibraryDecl::kDiagnosticDispatcher: {
      static constexpr LibraryDeclData kDiagnosticDispatcher = kFacility(
          "diagnostic dispatcher", "lyra::runtime::DiagnosticDispatcher&");
      return &kDiagnosticDispatcher;
    }

    case LibraryDecl::kScope: {
      static constexpr LibraryDeclData kScope = kObjectOfClass(
          "scope", RuntimeClass{.lifetime = ObjectLifetime::kTheRun},
          "lyra::runtime::Scope");
      return &kScope;
    }
    case LibraryDecl::kObjectRoot: {
      static constexpr LibraryDeclData kObjectRoot = kObjectOfClass(
          "object", RuntimeClass{.lifetime = ObjectLifetime::kWhileReferenced},
          "lyra::runtime::GcObject");
      return &kObjectRoot;
    }
    case LibraryDecl::kClassObject: {
      static constexpr LibraryDeclData kClassObject = kObjectOfClass(
          "class object",
          RuntimeClass{.lifetime = ObjectLifetime::kWhileReferenced},
          "lyra::runtime::ClassObject");
      return &kClassObject;
    }
    case LibraryDecl::kProcess: {
      static constexpr LibraryDeclData kProcess = kObjectOfClass(
          "process", RuntimeClass{.lifetime = ObjectLifetime::kWhileReferenced},
          "lyra::runtime::RuntimeProcess");
      return &kProcess;
    }
    case LibraryDecl::kSynchronizationObject: {
      static constexpr LibraryDeclData kSynchronizationObject = kObjectOfClass(
          "synchronization object",
          RuntimeClass{.lifetime = ObjectLifetime::kWhileReferenced},
          "lyra::runtime::SynchronizationObject");
      return &kSynchronizationObject;
    }
    case LibraryDecl::kConstraintProblem: {
      static constexpr LibraryDeclData kConstraintProblem = kObjectOfClass(
          "constraint problem",
          RuntimeClass{.lifetime = ObjectLifetime::kWhileReferenced},
          "lyra::runtime::ConstraintProblem");
      return &kConstraintProblem;
    }
  }
  return nullptr;
}

// Whether what a member holding one is suits how a holder holds one: the value
// it is only where it is a value of a domain; its own object only where it
// states one, and every object of its own no holder keeps by value is held as
// a member, or nothing would hold it; and a handle or a history only over
// storage kept elsewhere. A declaration stating another pair fails to compile.
consteval auto EveryMemberSuitsItsHolding() -> bool {
  bool suits = true;
  ForEachLibraryDecl([&](LibraryDecl decl) {
    const LibraryDeclData& declaration = DeclarationOf(decl);
    const auto* own = std::get_if<OwnObject>(&declaration.holding);
    const bool elsewhere =
        std::holds_alternative<StorageElsewhere>(declaration.holding);
    suits =
        suits && std::visit(
                     Overloaded{
                         [&](const NoMember&) {
                           return own == nullptr || own->by_value.has_value();
                         },
                         [&](const ValueMember&) {
                           return std::holds_alternative<DomainObject>(
                               declaration.holding);
                         },
                         [&](const OwnObjectMember&) { return own != nullptr; },
                         [&](const BorrowedMember&) { return elsewhere; },
                         [&](const HistoryMember&) { return elsewhere; }},
                     declaration.member);
  });
  return suits;
}
static_assert(
    EveryMemberSuitsItsHolding(),
    "a declaration of the library's states a member its holding cannot be");

}  // namespace lyra::support
