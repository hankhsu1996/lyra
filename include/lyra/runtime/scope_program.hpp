#pragma once

#include <cstdint>
#include <span>
#include <variant>

#include "lyra/support/net_resolution.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::runtime {

class Scope;

// The time unit or precision power a scope reports when it declares no
// timescale of its own (the synthetic `$root`). The engine's design-global
// precision minimum ignores it, so a purely structural node does not pull the
// simulation tick finer.
inline constexpr std::int8_t kUnspecifiedTimePower = 127;

// The backend-neutral entry a generated body is reached through: a native
// function over the generic scope receiver. A backend realizes it as a thunk
// over its own body (C++) or as an ABI-compatible free function (LLVM).
using ScopeEntry = void (*)(Scope*);

// The canonical no-op a required lifecycle entry points at when a scope has no
// work for that phase, so a null entry always means "not supplied" (a build /
// link error) rather than "nothing to do".
void ScopeNoOp(Scope* scope);

// A string that crosses the generated-runtime boundary as plain data -- a
// pointer plus a length, not a C++ `std::string_view`, so a non-C++ backend
// fills it without depending on a C++ type's layout. The pointed-at bytes are
// owned elsewhere (an emitted string literal, or the lowered unit) and outlive
// every scope that names them.
struct AbiStringRef {
  const char* data = nullptr;
  std::uint32_t size = 0;

  constexpr AbiStringRef() = default;
  constexpr AbiStringRef(const char* data, std::uint32_t size)
      : data(data), size(size) {
  }
};

// A scope's immutable constant properties, known when its definition is built
// and never computed by running generated code: its effective time unit and
// precision as powers of ten (LRM Table 20-2), each the scope's own timescale
// or the one it inherits (LRM 3.14.2.3). These are data, not entries -- a
// backend supplies the values, it does not supply a function that returns them.
struct ScopeMetadata {
  std::int8_t time_unit_power = kUnspecifiedTimePower;
  std::int8_t time_precision_power = kUnspecifiedTimePower;

  constexpr ScopeMetadata() = default;
  constexpr ScopeMetadata(
      std::int8_t time_unit_power, std::int8_t time_precision_power)
      : time_unit_power(time_unit_power),
        time_precision_power(time_precision_power) {
  }
};

// A DPI-C export entry as the table holds it: a code address with its prototype
// erased, so entries of every prototype share one table. It stays a function
// pointer rather than becoming a data pointer, because converting between the
// two is not something the language guarantees.
//
// An erased entry is only ever called after being restored to the exact type
// its definition was generated with. Both the definition and the restoring call
// site are generated from one foreign-linkage description, so the two cannot
// disagree -- which is what makes the erasure safe rather than conventional.
using ErasedScopeExportEntry = void (*)();

// One DPI-C export this scope publishes (LRM 35.4): the program-global name the
// foreign side calls, and the entry adapting that call to this scope's own
// subroutine. One name reaches whichever scope the foreign call chain
// established (LRM 35.5.3) -- including a different specialization of the scope
// that declared it, whose subroutine is separately compiled code.
struct ScopeExport {
  AbiStringRef name;
  ErasedScopeExportEntry entry = nullptr;

  constexpr ScopeExport() = default;
  constexpr ScopeExport(AbiStringRef name, ErasedScopeExportEntry entry)
      : name(name), entry(entry) {
  }
};

// The exports a scope publishes, crossing the generated-runtime boundary as
// plain data. Empty for a scope that declares none, which is nearly all of
// them.
struct ScopeExportTable {
  const ScopeExport* data = nullptr;
  std::uint32_t size = 0;

  constexpr ScopeExportTable() = default;
  constexpr ScopeExportTable(const ScopeExport* data, std::uint32_t size)
      : data(data), size(size) {
  }

  [[nodiscard]] constexpr auto Entries() const -> std::span<const ScopeExport> {
    return {data, size};
  }
};

// One scope's generated behavior plus its constant metadata. Every scope -- a
// unit instance or a generate scope -- has one. The lifecycle entries run this
// scope's own work only; the runtime owns child traversal and phase ordering.
struct ScopeProgram {
  ScopeMetadata metadata;
  ScopeEntry resolve_state = &ScopeNoOp;
  ScopeEntry initialize_state = &ScopeNoOp;
  ScopeEntry create_processes = &ScopeNoOp;
  ScopeExportTable exports;

  constexpr ScopeProgram() = default;
  constexpr ScopeProgram(
      ScopeMetadata metadata, ScopeEntry resolve_state,
      ScopeEntry initialize_state, ScopeEntry create_processes,
      ScopeExportTable exports)
      : metadata(metadata),
        resolve_state(resolve_state),
        initialize_state(initialize_state),
        create_processes(create_processes),
        exports(exports) {
  }
};

// How a member's storage is realized. Each alternative carries exactly what
// realizing it needs: one that holds a value of its own names the domain that
// value is realized in, and one that holds none names nothing.

// A box holding a pointer the owner does not own: the storage behind a
// reference reaching another scope.
struct BorrowedHandleStorage {};

// The subscribable variable a process reads, writes, and waits on.
struct ObservableCellStorage {
  support::ValueDomain domain;
};

// A value the owner takes a copy of once and never writes again -- a chandle
// (LRM 6.14), and every value a closure snapshots into a capture. Reading it
// hands back the storage itself, which is safe exactly because nothing writes
// it afterwards.
struct InlineValueStorage {
  support::ValueDomain domain;
};

// A net's resolution node (LRM 6.5, 6.6): the storage its drivers'
// contributions fold into. Like the observable cell it is reached only through
// its own access, and unlike it nothing ever writes it -- a value reaches a net
// only through a driver. It names the domain its value is realized in and the
// fold its net type picked.
struct ResolvedNetStorage {
  support::ValueDomain domain;
  support::NetResolution resolution;
};

// A variable the owner holds: written and read through its own storage, so a
// write lands at the representation the declaration gave it (LRM 6.9) and a
// read copies out rather than aliasing. No process subscribes to it, which is
// what separates it from the cell above.
struct ValueCellStorage {
  support::ValueDomain domain;
};

// A scope's cancellation target (LRM 9.6.2). Every scope owns one, so every
// backend realizes the storage; whether a backend can also raise and consume
// the control effect a `disable` sends through it is a separate question.
struct CancellationTargetStorage {};

// The joint cancel state of the channels a deferred file write targets (LRM
// 21.3.2), snapshotted into the closure that will perform that write so the
// write short-circuits if a descriptor is closed before its region runs.
struct ChannelCancellationStorage {};

using MemberStorageDescriptor = std::variant<
    BorrowedHandleStorage, ObservableCellStorage, InlineValueStorage,
    ValueCellStorage, CancellationTargetStorage, ChannelCancellationStorage,
    ResolvedNetStorage>;

// One declaration's member storage schema, in its own member order: what a
// generic value of it must realize for each member the declaration holds. It
// crosses as a pointer plus a length rather than a C++ container, so a
// definition can name a schema that outlives whatever built it. The pointed-at
// descriptors are owned by whoever built the definition and outlive every value
// built from it.
struct MemberStorageSchema {
  const MemberStorageDescriptor* data = nullptr;
  std::uint32_t size = 0;

  [[nodiscard]] constexpr auto Descriptors() const
      -> std::span<const MemberStorageDescriptor> {
    return {data, size};
  }
};

// The immutable definition of one scope class: its program (held by value, so a
// class's whole generated behavior is one constant), its
// structural-construction entry, and the storage schema of its members. Every
// scope class has one, whether it is the class a compilation unit publishes or
// a class that unit keeps to itself -- what differs is only how a constructing
// site names the definition, by linkage symbol across the unit boundary and by
// in-artifact constant within it. Construction is bootstrap-called (a JIT
// design) or realized by the backend's own constructor (the C++ backend),
// distinct from the per-phase lifecycle dispatch. The schema tells a generic
// instance what storage to own for each member; a backend that lays members out
// natively leaves it empty.
struct ScopeDefinition {
  ScopeProgram program;
  ScopeEntry construct = &ScopeNoOp;
  MemberStorageSchema members;

  constexpr ScopeDefinition() = default;
  constexpr ScopeDefinition(ScopeProgram program, ScopeEntry construct)
      : program(program), construct(construct) {
  }
};

}  // namespace lyra::runtime
