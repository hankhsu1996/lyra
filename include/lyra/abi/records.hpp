#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>

#include "lyra/support/dump_kind.hpp"
#include "lyra/support/scope_execution.hpp"
#include "lyra/value/object_ref.hpp"

// The structures the runtime library reads out of constants a compiled design
// states: what each class is, what a scope of the design hierarchy is, how each
// object of the design is constructed, and what a value dump records. Each is
// declared once, here, and both sides read the declaration: the library to
// read a constant, and the compiler, which is built against it, to state one
// -- the type of each member it fills and where the member lies. A record's
// members are the ones `ForEachField` hands over, in the order the record
// declares them; a member that is a structure of its own is handed over whole.

namespace lyra::runtime {
class GcObject;
}  // namespace lyra::runtime

namespace lyra::abi {

struct ObjectDefinition;
struct ConstructionTable;

// A borrowed view of the entries a unit holds as a constant array, laid out as
// the library reads it: where the first entry is and how many there are, which
// is what a table is in a C program, a `std::span` is in C++, and a slice is in
// Rust. The array is what owns the entries; a view of all of it is made from
// it, so the count is the array's own and no view of none points anywhere.
template <typename T>
struct ConstantSpan {
  const T* data = nullptr;
  std::uint64_t size = 0;

  constexpr ConstantSpan() = default;

  template <std::size_t N>
  constexpr explicit ConstantSpan(const std::array<T, N>* entries) noexcept
      : data(N == 0 ? nullptr : entries->data()), size(N) {
  }

  [[nodiscard]] auto Entries() const -> std::span<const T> {
    return {data, size};
  }
};

// Whether `T` is a view the library lays out as `ConstantSpan`: the one
// structure of the library whose meaning is a view of entries rather than the
// members it declares.
template <typename T>
inline constexpr bool kIsConstantSpan = false;

template <typename T>
inline constexpr bool kIsConstantSpan<ConstantSpan<T>> = true;

// The time unit or precision power a scope reports when it declares no
// timescale of its own (the synthetic `$root`). The engine's design-global
// precision minimum ignores it, so a purely structural node does not pull the
// simulation tick finer.
inline constexpr std::int8_t kUnspecifiedTimePower = 127;

// A scope's immutable constant properties, known when its class is declared
// and never computed by running generated code: its effective time unit and
// precision as powers of ten (LRM Table 20-2), each the scope's own timescale
// or the one it inherits (LRM 3.14.2.3), how what an instance starts is run
// (LRM 24.3.1), and the kind of scope a value dump records it as (LRM
// 21.7.2.1 `scope_type`), each held as the number a constant record writes.
struct ScopeMetadata {
  std::int8_t time_unit_power = kUnspecifiedTimePower;
  std::int8_t time_precision_power = kUnspecifiedTimePower;
  std::uint8_t execution =
      static_cast<std::uint8_t>(support::ScopeExecution::kDesign);
  std::uint8_t dumped_as =
      static_cast<std::uint8_t>(support::DumpScopeKind::kModule);

  [[nodiscard]] auto Execution() const -> support::ScopeExecution {
    return static_cast<support::ScopeExecution>(execution);
  }
  [[nodiscard]] auto DumpedAs() const -> support::DumpScopeKind {
    return static_cast<support::DumpScopeKind>(dumped_as);
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

// What a class of the design hierarchy tells the library about its instances:
// its timescale, the DPI-C exports an instance answers, and what an instance
// does in each phase the library drives it through. A DPI-C export's name is
// the program-global C identifier the foreign side calls (LRM 35.4), so the
// foreign side has no type to reach the body through and finds it by that
// name. The phases are listed in the order they run -- every route and alias
// is bound, then every cell takes the value its declaration gives it (LRM
// 10.5), then every process is created (LRM 9.2) -- and then what an instance
// describes to a value dump starting (LRM 21.7.1.2); each is entered on the
// instance, as every entry of a class's definition is. What a unit publishes of
// its object has none of this, since no instance is built of it.
struct ScopeInfo {
  ScopeMetadata metadata;
  ConstantSpan<ScopeCallable> exports;
  ScopePhase resolve = nullptr;
  ScopePhase initialize = nullptr;
  ScopePhase create_processes = nullptr;
  ScopePhase describe = nullptr;
};

// One interface class an object's class implements (LRM 8.26), directly or
// through what it extends, and the table of the bodies answering what that
// interface class declares itself, which only the code calling one reads. An
// interface class's own definition is its identity.
struct InterfaceRow {
  const ObjectDefinition* of = nullptr;
  const void* table = nullptr;
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
// how an object of it ends, the interface classes it implements, and the
// body each of its virtual methods runs (LRM 8.20). Which body initializes a
// value is settled where the value is asked for rather than by the class it is
// of (LRM 8.7), so what a definition carries is what every value of the class
// shares and nothing about any one of them.
//
// Every entry is a body entered on an object whose class the caller does not
// know. One driving or randomizing the object is handed it as the root every
// object is and views it as the class whose body it calls; the one ending it
// is handed its storage, which is what the class's destructor takes. The unit
// declaring the class emits the definition as one constant, which every unit
// naming the class reaches by the symbol it is linked under; nothing is
// declared to the library while the program runs.
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
  ConstantSpan<InterfaceRow> interfaces;
  // The table of the body each behavior runs on an object of the class (LRM
  // 8.20), laid out by the code that dispatches on one and read by nothing
  // here. Absent for an interface class.
  const void* methods = nullptr;
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
  ConstantSpan<Instantiation> places;
};

// What a constructor of a scope of the design hierarchy is handed: how each
// object of each instantiation the scope declares is made, the instantiations
// in the order the scope declares them and each one's objects in row-major
// order, and the places of each of its generates, in the order it declares
// them. A scope's code names only positions of its own table, so it is the
// same code wherever the scope stands. The tables are constants of the
// assembled program.
struct ConstructionTable {
  ConstantSpan<Instantiation> instantiations;
  ConstantSpan<GeneratePlaces> generates;
};

// What a class of the design hierarchy states of one thing a dump may record:
// a variable, a net, a port or a named event it declares (LRM 21.7.1.2). The
// unit declaring the class emits one as a constant per such declaration, which
// is what an instance is described with and what a format declares the thing
// by. A range is the declared one of a vector, which a dump writes beside its
// name (LRM 21.7.2.3 `$var`); a port is where the thing is what a port of the
// class connects, with the port's direction, which an extended dump reads its
// values by, and its position among the ports, which it records them in (LRM
// 21.7.3.1, 21.7.4.2).
struct DumpEntry {
  const char* identifier = nullptr;
  std::int64_t left = 0;
  std::int64_t right = 0;
  std::uint32_t width = 0;
  std::uint8_t kind = 0;
  std::uint8_t has_range = 0;
  std::uint8_t has_port = 0;
  std::uint8_t port_direction = 0;
  std::uint32_t port_position = 0;

  struct Bounds {
    std::int64_t left;
    std::int64_t right;
  };
  struct Connection {
    support::DumpPortDirection direction;
    std::uint32_t position;
  };

  [[nodiscard]] auto Kind() const -> support::DumpVarKind {
    return static_cast<support::DumpVarKind>(kind);
  }
  [[nodiscard]] auto Range() const -> std::optional<Bounds> {
    if (has_range == 0) {
      return std::nullopt;
    }
    return Bounds{.left = left, .right = right};
  }
  [[nodiscard]] auto Port() const -> std::optional<Connection> {
    if (has_port == 0) {
      return std::nullopt;
    }
    return Connection{
        .direction = static_cast<support::DumpPortDirection>(port_direction),
        .position = port_position};
  }
};

// A named block, task or function a class declares, whose static variables
// are the class's own storage (LRM 21.7.2.3 `$scope`).
struct DumpBlock {
  const char* identifier = nullptr;
  std::uint8_t kind = 0;

  [[nodiscard]] auto Kind() const -> support::DumpScopeKind {
    return static_cast<support::DumpScopeKind>(kind);
  }
};

// Hands each member of a record to `visit`, in the order the record declares
// them, as Boost.PFR's `for_each_field` does. A structured binding names every
// member, so a record gaining or losing one stops these from compiling until
// they say so.

template <typename Visit>
constexpr void ForEachField(const ScopeMetadata& record, const Visit& visit) {
  const auto& [time_unit_power, time_precision_power, execution, dumped_as] =
      record;
  visit(time_unit_power);
  visit(time_precision_power);
  visit(execution);
  visit(dumped_as);
}

template <typename Visit>
constexpr void ForEachField(const ScopeCallable& record, const Visit& visit) {
  const auto& [name, entry] = record;
  visit(name);
  visit(entry);
}

template <typename Visit>
constexpr void ForEachField(const ScopeInfo& record, const Visit& visit) {
  const auto& [metadata, exports, resolve, initialize, create_processes, describe] =
      record;
  visit(metadata);
  visit(exports);
  visit(resolve);
  visit(initialize);
  visit(create_processes);
  visit(describe);
}

template <typename Visit>
constexpr void ForEachField(const InterfaceRow& record, const Visit& visit) {
  const auto& [of, table] = record;
  visit(of);
  visit(table);
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
  const auto& [base, scope, class_object, end, interfaces, methods] = record;
  visit(base);
  visit(scope);
  visit(class_object);
  visit(end);
  visit(interfaces);
  visit(methods);
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
constexpr void ForEachField(const DumpEntry& record, const Visit& visit) {
  const auto& [identifier, left, right, width, kind, has_range, has_port, port_direction, port_position] =
      record;
  visit(identifier);
  visit(left);
  visit(right);
  visit(width);
  visit(kind);
  visit(has_range);
  visit(has_port);
  visit(port_direction);
  visit(port_position);
}

template <typename Visit>
constexpr void ForEachField(const DumpBlock& record, const Visit& visit) {
  const auto& [identifier, kind] = record;
  visit(identifier);
  visit(kind);
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
