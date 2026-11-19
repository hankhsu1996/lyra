#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <tuple>

#include "lyra/base/source_location.hpp"
#include "lyra/base/time.hpp"
#include "lyra/support/data_object.hpp"
#include "lyra/support/port_direction.hpp"
#include "lyra/support/random_qualifier.hpp"
#include "lyra/support/scope_execution.hpp"
#include "lyra/support/scope_kind.hpp"
#include "lyra/support/start_kind.hpp"
#include "lyra/value/constant_span.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/object_ref.hpp"

// The structures the runtime library reads out of constants a compiled design
// states: what each class is, what a scope of the design hierarchy is and the
// data objects it declares, how each object of the design is constructed, each
// static process an instance starts, where an operation was written, and an
// enumeration's members. Each is
// declared once, here, or for the last two in the layer that reads them -- the
// base layer's reports and the value layer -- and both sides read the
// declaration: the library to read a constant, and the compiler, which is built
// against it, to state one -- the type of each member it fills and where the
// member lies. A record's members are the ones `ForEachField` hands over, in
// the order the record declares them; a member that is a structure of its own
// is handed over whole.

namespace lyra::runtime {
class GcObject;
class Scope;
class Selection;
class SelectionSettler;
class Subscriber;
class ValueDeserializer;
class ValueSerializer;
}  // namespace lyra::runtime

namespace lyra::abi {

struct ObjectDefinition;
struct ConstructionTable;

// What a definition is, as every scope of the run that comes from it answers
// (LRM 37.10): what kind of definition, its name, the library it was read
// from (LRM 33.3) and the cell its text lies in (LRM 33.2.1), its time unit
// and precision as powers of ten (LRM Table 20-2) -- its own timescale, the
// one it inherits, or the default where the source states none (LRM
// 3.14.2.3) -- and whether the source stated them, since only a stated one
// takes part in the design's global precision (LRM 3.14.3), and the
// directives in force where it was written: whether `celldefine marks it
// (LRM 22.10), the net type of an implicit net (LRM 22.8), the pull of an
// unconnected input (LRM 22.9), and whether its subroutines are automatic
// (LRM 23.2.1). A generate block's class answers its definition's. Each
// enumeration is held as the number a constant record writes.
struct DefinitionRecord {
  const char* name = nullptr;
  const char* library = nullptr;
  const char* cell = nullptr;
  std::int8_t time_unit_power = kDefaultTimeUnitPower;
  std::int8_t time_precision_power = kDefaultTimePrecisionPower;
  std::uint8_t time_stated = 0;
  std::uint8_t kind = 0;
  std::uint8_t default_net_type = 0;
  std::uint8_t unconnected_drive = 0;
  std::uint8_t celldefine = 0;
  std::uint8_t automatic = 0;

  [[nodiscard]] auto Kind() const -> support::DefinitionKind {
    return static_cast<support::DefinitionKind>(kind);
  }
  [[nodiscard]] auto TimeStated() const -> bool {
    return time_stated != 0;
  }
  [[nodiscard]] auto DefaultNetType() const -> support::DefaultNetType {
    return static_cast<support::DefaultNetType>(default_net_type);
  }
  [[nodiscard]] auto UnconnectedDrive() const -> support::UnconnectedDrive {
    return static_cast<support::UnconnectedDrive>(unconnected_drive);
  }
  [[nodiscard]] auto Execution() const -> support::ScopeExecution {
    return support::ExecutionOf(Kind());
  }
};

// A scope one scope declares that the run builds -- an instance, an array of
// them, a generate block, the blocks a loop generate counts out -- or the
// implicit root of the design hierarchy, as a constant of the declaring
// scope's class: what kind of declaration it is, the name it declares as a
// hierarchical name writes it (LRM 23.6), and whether that name is the one
// LRM 27.6 gives a generate block the source did not name. A scope a
// construction builds is handed the declaration it fills, and is named by it.
struct ElaboratedScopeRecord {
  const char* name = nullptr;
  std::uint8_t kind = 0;
  std::uint8_t implicitly_named = 0;

  [[nodiscard]] auto Kind() const -> support::ElaboratedScopeKind {
    return static_cast<support::ElaboratedScopeKind>(kind);
  }
  // Whether its name is LRM 27.6's rather than the source's (LRM 37.85).
  [[nodiscard]] auto ImplicitlyNamed() const -> bool {
    return implicitly_named != 0;
  }
};

// A scope one scope declares that the run builds nothing for, the declaration
// being the whole of it (LRM 37.12), as a constant of the declaring scope's
// class: what kind of declaration it is, the name it declares as a
// hierarchical name writes it (LRM 23.6) -- empty for a block the source did
// not name, which no written name can be -- the one it lies inside, if any,
// and where it stands among the class's declarations. A unit holds each
// distinct constant once, so the position is what keeps two declarations alike
// in all else two records.
struct LexicalScopeRecord {
  const char* name = nullptr;
  const LexicalScopeRecord* enclosing = nullptr;
  std::uint32_t position = 0;
  std::uint8_t kind = 0;

  [[nodiscard]] auto Kind() const -> support::LexicalScopeKind {
    return static_cast<support::LexicalScopeKind>(kind);
  }
  // Whether the source named the scope.
  [[nodiscard]] auto Named() const -> bool {
    return *name != '\0';
  }
};

// A body as a table of the runtime holds it: a code address with its prototype
// erased, so entries of every prototype share one table. It stays a function
// pointer rather than becoming a data pointer, because converting between the
// two is not something the language guarantees.
//
// An erased entry is only ever called after being restored to the exact type
// its definition was generated with. Both the definition and the restoring call
// site are generated from one description of the body, so the two cannot
// disagree -- which is what makes the erasure safe rather than conventional.
using ErasedEntry = void (*)();

// One DPI-C export a scope answers (LRM 35.5.3): the C identifier the foreign
// side calls, and the entry adapting that call to this scope's own subroutine.
// The caller supplies the scope as the entry's first argument, the way every
// callable takes its receiver. The name is the declaring unit's own
// NUL-terminated constant.
struct ScopeCallable {
  const char* name = nullptr;
  ErasedEntry entry = nullptr;
};

// The body a class of the design hierarchy runs on an instance in one phase.
using ScopePhase = void (*)(runtime::GcObject*);

struct TypespecMember;

// A data object's type as the source declared it (LRM 37.25), a constant of
// the unit describing the object: the typedef that names it, empty text where
// none does, and the typespec of the type that typedef aliases where that is
// named too; what kind of type it is, how many bits a value of it holds and
// whether it is signed where it is packed, its range -- a vector's outermost
// packed range, a fixed-size unpacked array's -- and what it is built from:
// the typespec of one element of an array, packed or unpacked, a single bit's
// for a vector of bits (LRM 37.25 detail 8), an associative array's index
// (none for a wildcard index), a structure's or a union's members in declared
// order, and an enumeration's base type (LRM 6.19) and members; and whether a
// union is tagged and whether it is soft (LRM 7.3). Each enumeration is held
// as the number a constant record writes.
struct Typespec {
  const char* name = nullptr;
  const Typespec* alias = nullptr;
  const Typespec* base = nullptr;
  const Typespec* element = nullptr;
  const Typespec* index = nullptr;
  const value::Enumeration* enumeration = nullptr;
  value::ConstantSpan<const TypespecMember*> members;
  std::int64_t left = 0;
  std::int64_t right = 0;
  std::uint32_t width = 0;
  std::uint8_t kind = 0;
  std::uint8_t has_range = 0;
  std::uint8_t is_signed = 0;
  std::uint8_t tagged = 0;
  std::uint8_t soft = 0;

  [[nodiscard]] auto Kind() const -> support::TypespecKind {
    return static_cast<support::TypespecKind>(kind);
  }
  [[nodiscard]] auto Range() const -> std::optional<support::Range> {
    if (has_range == 0) {
      return std::nullopt;
    }
    return support::Range{.left = left, .right = right};
  }
  [[nodiscard]] auto IsSigned() const -> bool {
    return is_signed != 0;
  }
  [[nodiscard]] auto Tagged() const -> bool {
    return tagged != 0;
  }
  [[nodiscard]] auto Soft() const -> bool {
    return soft != 0;
  }
};

// A member of a structure or a union a typespec lists (LRM 37.25, 37.26), as a
// constant of the unit: its identifier, its own typespec, and how it is
// declared random.
struct TypespecMember {
  const char* identifier = nullptr;
  const Typespec* typespec = nullptr;
  std::uint8_t random = 0;

  [[nodiscard]] auto Random() const -> support::RandomQualifier {
    return static_cast<support::RandomQualifier>(random);
  }
};

// One data object a scope of the design hierarchy, a package or the
// compilation unit's scope declares that a reader reaching the design by name
// finds -- a variable, a net or a named event (LRM 21.7.1.2) -- as a constant
// of its unit, the same for every instance: its name, the named block, task
// or function it lies in, if any, the net type it is declared with, none for
// a variable, its type, whether it is a variable declared `const` (LRM 6.20.6),
// and whether it is the name a `ref` port gives the variable connected to it
// (LRM 37.15). Each enumeration is held as the number a constant record
// writes.
struct DataObjectRecord {
  const char* identifier = nullptr;
  const LexicalScopeRecord* enclosing = nullptr;
  const Typespec* typespec = nullptr;
  std::uint8_t net_type = 0;
  std::uint8_t has_net_type = 0;
  std::uint8_t constant = 0;
  std::uint8_t reference = 0;

  [[nodiscard]] auto NetType() const -> std::optional<support::NetType> {
    if (has_net_type == 0) {
      return std::nullopt;
    }
    return static_cast<support::NetType>(net_type);
  }
  [[nodiscard]] auto Constant() const -> bool {
    return constant != 0;
  }
  [[nodiscard]] auto Reference() const -> bool {
    return reference != 0;
  }
};

// One port of a scope of the design hierarchy that connects a data object the
// scope declares (LRM 23.2.2), as a constant of its unit: the port's name, the
// position among the scope's data objects of the one it connects, and the
// direction data flows across it. Two ports connecting one object are two of
// these.
struct DescribedPort {
  const char* name = nullptr;
  std::uint32_t connects = 0;
  std::uint8_t direction = 0;

  [[nodiscard]] auto Direction() const -> support::PortDirection {
    return static_cast<support::PortDirection>(direction);
  }
};

// What a scope of the design hierarchy states of itself (LRM 37.10, 37.12): the
// definition it comes from, the scopes it declares, the data objects it
// declares, in the order it declares them, the ports connecting them, in the
// order the declaration gives the ports (LRM 21.7.4.2), and the bodies
// reaching any of the data objects, compiled where each one's type is known,
// each handed the object, the data object's position among those listed, and
// what the reader gave: one settles a selection by the types alone, into the
// key each entry it names is kept by and the positions a takeover counts
// before the part (LRM 7.8.4, 10.6.2), one hands the serializer the value of
// the part a selection names, one writes that part what the deserializer holds
// as the design's own write of it does, one forces the positions of the part a
// selection names to what the deserializer holds -- the first and how many of
// them, in the part's own numbering -- and one releases them (LRM 38.34,
// 10.6.2), each answering what it came to as the build reads the
// standard, and one hands the subscriber the trigger a wait for a change of it
// would enrol. Each is handed last where the reader's own call was made, where
// what fails in it is reported. A class of the hierarchy states it of its
// instances, and its bodies are entered on the instance; a package and the
// compilation unit's scope, each one object of the run, state it once, as a
// constant handed to the library as its storage is installed, have no ports,
// and have bodies that take an object and leave it unread.
struct ScopeDeclarations {
  const DefinitionRecord* definition = nullptr;
  value::ConstantSpan<const ElaboratedScopeRecord*> elaborated;
  value::ConstantSpan<const LexicalScopeRecord*> lexical;
  value::ConstantSpan<const DataObjectRecord*> data_objects;
  value::ConstantSpan<const DescribedPort*> ports;
  void (*settle)(
      runtime::GcObject*, std::int64_t, const runtime::Selection&,
      runtime::SelectionSettler&, base::SourceLocation) = nullptr;
  void (*read)(
      runtime::GcObject*, std::int64_t, const runtime::Selection&,
      runtime::ValueSerializer&, base::SourceLocation) = nullptr;
  void (*write)(
      runtime::GcObject*, std::int64_t, const runtime::Selection&,
      runtime::ValueDeserializer&, base::SourceLocation) = nullptr;
  std::int64_t (*force)(
      runtime::GcObject*, std::int64_t, const runtime::Selection&,
      runtime::SelectionSettler&, std::int64_t, std::int64_t,
      runtime::ValueDeserializer&, base::SourceLocation) = nullptr;
  std::int64_t (*release)(
      runtime::GcObject*, std::int64_t, const runtime::Selection&,
      runtime::SelectionSettler&, std::int64_t, std::int64_t,
      base::SourceLocation) = nullptr;
  void (*subscribe)(
      runtime::GcObject*, std::int64_t, runtime::Subscriber&,
      base::SourceLocation) = nullptr;
};

// What a class of the design hierarchy tells the library about its instances:
// what they state of themselves, the DPI-C exports an instance answers, and
// what an instance does in each phase the library drives it through. A DPI-C
// export's name is the program-global C identifier the foreign side calls
// (LRM 35.4), so the foreign side has no type to reach the body through and
// finds it by that name. The phases are listed in the order they run -- every
// route and alias is bound, then every cell takes the value its declaration
// gives it (LRM 10.5), then every process is created (LRM 9.2); each is
// entered on the instance, as every entry of a class's definition is. What a
// unit publishes of its object has none of this, since no instance is built of
// it.
struct ScopeInfo {
  ScopeDeclarations scope_declarations;
  value::ConstantSpan<ScopeCallable> exports;
  ScopePhase resolve = nullptr;
  ScopePhase initialize = nullptr;
  ScopePhase create_processes = nullptr;
};

// One interface class an object's class implements (LRM 8.26), directly or
// through what it extends, and the table of the bodies answering what that
// interface class declares itself, which only the code calling one reads. An
// interface class's own definition is its identity.
struct InterfaceRow {
  const ObjectDefinition* of = nullptr;
  const void* table = nullptr;
};

// One interface class a class's declaration names -- what a class implements
// or an interface class extends (LRM 8.26.2) -- that is a type of an instance
// (LRM 6.22), and the body answering which instance's: handed the instance the
// declaring class belongs to, it answers the one the name reaches from there.
// Two names of one interface class reaching two instances are two of these.
struct InterfaceReach {
  const ObjectDefinition* of = nullptr;
  runtime::Scope* (*reach)(runtime::Scope*) = nullptr;
};

// What `randomize()` asks of an object of a class (LRM 18.6): to
// declare its random variables (LRM 18.4) and state its constraints (LRM 18.5)
// to the call's problem, to name the random objects its handles reach, and the
// hooks before and after (LRM 18.6.2). Each entry is the body the nearest class
// of the object's lineage declaring one gives, which does what its base does
// and then what it adds; none where no class does, which does nothing.
struct ClassObjectInfo {
  void (*rand_variables)(runtime::GcObject*, const value::ObjectRef&) = nullptr;
  void (*constrain)(runtime::GcObject*, const value::ObjectRef&) = nullptr;
  void (*rand_objects)(runtime::GcObject*, const value::ObjectRef&) = nullptr;
  void (*pre_randomize)(runtime::GcObject*) = nullptr;
  void (*post_randomize)(runtime::GcObject*) = nullptr;
};

// The definition of one class: what it extends, what a class whose values
// stand in the design hierarchy or are class objects (LRM 8.3) states of them,
// how an object of it ends, the interface classes it implements, the body each
// of its virtual methods runs (LRM 8.20), and, for a class that is a type of an
// instance (LRM 6.22), which instance an object of it is of and which instance
// each interface class its declaration names is of. Which body initializes a
// value is settled where the value is asked for rather than by the class it is
// of (LRM 8.7), so what a definition carries is what every value of the class
// shares and nothing about any one of them.
//
// Every entry is a body entered on an object whose class the caller does not
// know, or on an instance. One driving or randomizing the object, or answering
// the instance the class belongs to, is handed it as the root every object is
// and views it as the class whose body it calls; the one ending it is handed
// its storage, which is what the class's destructor takes; one answering the
// instance an interface class is of is handed the instance the class belongs
// to. The unit declaring the class emits the definition as one constant, which
// every unit naming the class reaches by the symbol it is linked under; nothing
// is declared to the library while the program runs.
struct ObjectDefinition {
  // The class this one extends (LRM 8.13), or nothing where it extends none.
  const ObjectDefinition* base = nullptr;
  // What only a class whose values stand in the design hierarchy has, absent
  // for every other class.
  const ScopeInfo* scope = nullptr;
  // What only a class of the language has, whether the source or the library
  // defines it, absent for every other class.
  const ClassObjectInfo* class_object = nullptr;
  // Ends an object of the class: every member, the class's own and then what
  // it extends, and then the storage it was created in. Absent for an
  // interface class, of which no object is created.
  void (*end)(void*) = nullptr;
  // Every interface class the class implements, each once however many ways
  // it is reached (LRM 8.26.6.3).
  value::ConstantSpan<InterfaceRow> interfaces;
  // The table of the body each behavior runs on an object of the class (LRM
  // 8.20), laid out by the code that dispatches on one and read by nothing
  // here. Absent for an interface class.
  const void* methods = nullptr;
  // The instance of the design hierarchy the class belongs to (LRM 6.22), as an
  // object of it keeps it. Absent where the class belongs to none, and for an
  // interface class, which keeps nothing.
  runtime::Scope* (*declaring_instance)(runtime::GcObject*) = nullptr;
  // Each interface class of an instance the class's own declaration names, in
  // the order written; what the class it extends names is that class's own.
  value::ConstantSpan<InterfaceReach> interface_reaches;
};

// How the object at one place of a scope's construction is made: one object
// an instantiation stands for (LRM 23.3.2), or a block at a place of a
// generate (LRM 27): the entry that makes an object of the body built there,
// and the table that object is constructed with in turn. Which body that is
// depends on the object being built, never on the code compiled for it, so it
// is stated where the program is assembled and handed to the code.
//
// The entry's prototype is erased so entries of every prototype share one
// array, and a construction restores it to the prototype of the class it
// reads at that place.
struct Instantiation {
  ErasedEntry entry = nullptr;
  const ConstructionTable* table = nullptr;
};

// How the block at each place of one generate is made (LRM 27): a loop's
// places in the order the loop counts its blocks out, as many as it counted
// for the object being built, a conditional's one per alternative the source
// wrote, and a block standing alone at its one place. A generate whose places
// all hold one entry lists it once, so a loop of blocks built alike states as
// much however many it counts out.
struct GeneratePlaces {
  value::ConstantSpan<Instantiation> places;
};

// What a constructor of a scope of the design hierarchy is handed: how each
// object of each instantiation the scope declares is made, the instantiations
// in the order the scope declares them and each one's objects in row-major
// order, and the places of each of its generates, in the order it declares
// them. A scope's code names only positions of its own table, so it is the
// same code wherever the scope stands. The tables are constants of the
// assembled program.
struct ConstructionTable {
  value::ConstantSpan<Instantiation> instantiations;
  value::ConstantSpan<GeneratePlaces> generates;
};

// A static process as compiled (Annex P): what kind of process it is, which
// says when it starts with its instance and what created its thread (LRM 9.2,
// 9.7), and where the source writes it. Every instance starting the process
// shares it, as does every process alike in kind and place, since a unit
// holds each distinct constant once. The kind is held as the number a constant
// record writes.
struct ProcessDefinition {
  std::uint8_t kind = 0;
  const base::SourceLocation::Impl* written_at = nullptr;

  [[nodiscard]] auto Kind() const -> support::StartKind {
    return static_cast<support::StartKind>(kind);
  }
};

// A part of a process that stops only at the head of its loop, entered on the
// object the process belongs to and on the wait the run keeps for it, both
// erased, since one run serves the processes of every class. A pass answers
// whether the wait has still to end; a setup answers what its form says.
using ReactionPart = bool (*)(void* receiver, void* wait);

// How a static process whose body runs to its end each time it is triggered
// is compiled: the setup that builds the wait it stops at, the pass that runs
// the loop's statements once (each a part), and the form saying how the run
// calls them. A body that never stops is its setup alone, and states no pass.
// The form is held as the number a constant record writes.
struct ReactionDefinition {
  ReactionPart setup = nullptr;
  ReactionPart pass = nullptr;
  std::uint8_t form = 0;

  [[nodiscard]] auto Form() const -> support::ReactionForm {
    return static_cast<support::ReactionForm>(form);
  }
};

// Hands each member of a record to `visit`, in the order the record declares
// them, as Boost.PFR's `for_each_field` does. A structured binding names every
// member, so a record gaining or losing one stops these from compiling until
// they say so.

template <typename Visit>
constexpr void ForEachField(
    const DefinitionRecord& record, const Visit& visit) {
  const auto& [name, library, cell, time_unit_power, time_precision_power, time_stated, kind, default_net_type, unconnected_drive, celldefine, automatic] =
      record;
  visit(name);
  visit(library);
  visit(cell);
  visit(time_unit_power);
  visit(time_precision_power);
  visit(time_stated);
  visit(kind);
  visit(default_net_type);
  visit(unconnected_drive);
  visit(celldefine);
  visit(automatic);
}

template <typename Visit>
constexpr void ForEachField(
    const ElaboratedScopeRecord& record, const Visit& visit) {
  const auto& [name, kind, implicitly_named] = record;
  visit(name);
  visit(kind);
  visit(implicitly_named);
}

template <typename Visit>
constexpr void ForEachField(
    const LexicalScopeRecord& record, const Visit& visit) {
  const auto& [name, enclosing, position, kind] = record;
  visit(name);
  visit(enclosing);
  visit(position);
  visit(kind);
}

template <typename Visit>
constexpr void ForEachField(
    const ScopeDeclarations& record, const Visit& visit) {
  const auto& [definition, elaborated, lexical, data_objects, ports, settle, read, write, force, release, subscribe] =
      record;
  visit(definition);
  visit(elaborated);
  visit(lexical);
  visit(data_objects);
  visit(ports);
  visit(settle);
  visit(read);
  visit(write);
  visit(force);
  visit(release);
  visit(subscribe);
}

template <typename Visit>
constexpr void ForEachField(const Typespec& record, const Visit& visit) {
  const auto& [name, alias, base, element, index, enumeration, members, left, right, width, kind, has_range, is_signed, tagged, soft] =
      record;
  visit(name);
  visit(alias);
  visit(base);
  visit(element);
  visit(index);
  visit(enumeration);
  visit(members);
  visit(left);
  visit(right);
  visit(width);
  visit(kind);
  visit(has_range);
  visit(is_signed);
  visit(tagged);
  visit(soft);
}

template <typename Visit>
constexpr void ForEachField(const TypespecMember& record, const Visit& visit) {
  const auto& [identifier, typespec, random] = record;
  visit(identifier);
  visit(typespec);
  visit(random);
}

template <typename Visit>
constexpr void ForEachField(
    const DataObjectRecord& record, const Visit& visit) {
  const auto& [identifier, enclosing, typespec, net_type, has_net_type, constant, reference] =
      record;
  visit(identifier);
  visit(enclosing);
  visit(typespec);
  visit(net_type);
  visit(has_net_type);
  visit(constant);
  visit(reference);
}

template <typename Visit>
constexpr void ForEachField(const DescribedPort& record, const Visit& visit) {
  const auto& [name, connects, direction] = record;
  visit(name);
  visit(connects);
  visit(direction);
}

template <typename Visit>
constexpr void ForEachField(const ScopeCallable& record, const Visit& visit) {
  const auto& [name, entry] = record;
  visit(name);
  visit(entry);
}

template <typename Visit>
constexpr void ForEachField(
    const ProcessDefinition& record, const Visit& visit) {
  const auto& [kind, written_at] = record;
  visit(kind);
  visit(written_at);
}

template <typename Visit>
constexpr void ForEachField(
    const ReactionDefinition& record, const Visit& visit) {
  const auto& [setup, pass, form] = record;
  visit(setup);
  visit(pass);
  visit(form);
}

template <typename Visit>
constexpr void ForEachField(const ScopeInfo& record, const Visit& visit) {
  const auto& [scope_declarations, exports, resolve, initialize, create_processes] =
      record;
  visit(scope_declarations);
  visit(exports);
  visit(resolve);
  visit(initialize);
  visit(create_processes);
}

template <typename Visit>
constexpr void ForEachField(const InterfaceRow& record, const Visit& visit) {
  const auto& [of, table] = record;
  visit(of);
  visit(table);
}

template <typename Visit>
constexpr void ForEachField(const InterfaceReach& record, const Visit& visit) {
  const auto& [of, reach] = record;
  visit(of);
  visit(reach);
}

template <typename Visit>
constexpr void ForEachField(const ClassObjectInfo& record, const Visit& visit) {
  const auto& [rand_variables, constrain, rand_objects, pre_randomize, post_randomize] =
      record;
  visit(rand_variables);
  visit(constrain);
  visit(rand_objects);
  visit(pre_randomize);
  visit(post_randomize);
}

template <typename Visit>
constexpr void ForEachField(
    const ObjectDefinition& record, const Visit& visit) {
  const auto& [base, scope, class_object, end, interfaces, methods, declaring_instance, interface_reaches] =
      record;
  visit(base);
  visit(scope);
  visit(class_object);
  visit(end);
  visit(interfaces);
  visit(methods);
  visit(declaring_instance);
  visit(interface_reaches);
}

template <typename Visit>
constexpr void ForEachField(const Instantiation& record, const Visit& visit) {
  const auto& [entry, table] = record;
  visit(entry);
  visit(table);
}

template <typename Visit>
constexpr void ForEachField(const GeneratePlaces& record, const Visit& visit) {
  const auto& [places] = record;
  visit(places);
}

template <typename Visit>
constexpr void ForEachField(
    const ConstructionTable& record, const Visit& visit) {
  const auto& [instantiations, generates] = record;
  visit(instantiations);
  visit(generates);
}

template <typename Visit>
constexpr void ForEachField(
    const base::SourceLocation::Impl& record, const Visit& visit) {
  const auto& [file, line, column] = record;
  visit(file);
  visit(line);
  visit(column);
}

template <typename Visit>
constexpr void ForEachField(
    const value::Enumeration& record, const Visit& visit) {
  const auto& [planes, names, width, four_state] = record;
  visit(planes);
  visit(names);
  visit(width);
  visit(four_state);
}

// A structure the library states its members' order of itself.
template <typename Record, typename Visit>
  requires requires(const Record& record) { Record::Tie(record); }
constexpr void ForEachField(const Record& record, const Visit& visit) {
  std::apply(
      [&](const auto&... field) { (visit(field), ...); }, Record::Tie(record));
}

// Where `member` stands among the members of `Record`: the one `ForEachField`
// hands over at the member's own address, which a member of the record always
// is. Members are told apart by where they lie rather than by their types,
// which several share, and only among one record's own members, since a nested
// record lies where its first member does.
template <typename Record, typename Member>
[[nodiscard]] auto PositionOf(Member Record::* member) -> std::size_t {
  static const Record kProbe{};
  const void* const wanted = &(kProbe.*member);
  std::size_t at = 0;
  std::size_t found = 0;
  ForEachField(kProbe, [&](const auto& field) {
    if (static_cast<const void*>(&field) == wanted) {
      found = at;
    }
    ++at;
  });
  return found;
}

}  // namespace lyra::abi
