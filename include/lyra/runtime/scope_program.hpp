#pragma once

#include <cstdint>
#include <span>

namespace lyra::runtime {

class Scope;

// Returned by a scope that declares no timescale of its own (the synthetic
// `$root`). The engine's design-global precision minimum ignores it, so a
// purely structural node does not pull the simulation tick finer.
inline constexpr std::int8_t kUnspecifiedTimePrecisionPower = 127;

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
// and never computed by running generated code: its def name (LRM 23.8; empty
// for a generate scope or `$root`) and its declared time precision as a power
// of ten (LRM Table 20-2). These are data, not entries -- a backend supplies
// the values, it does not supply a function that returns them.
struct ScopeMetadata {
  AbiStringRef def_name;
  std::int8_t time_precision_power = kUnspecifiedTimePrecisionPower;

  constexpr ScopeMetadata() = default;
  constexpr ScopeMetadata(
      AbiStringRef def_name, std::int8_t time_precision_power)
      : def_name(def_name), time_precision_power(time_precision_power) {
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

  constexpr ScopeProgram() = default;
  constexpr ScopeProgram(
      ScopeMetadata metadata, ScopeEntry resolve_state,
      ScopeEntry initialize_state, ScopeEntry create_processes)
      : metadata(metadata),
        resolve_state(resolve_state),
        initialize_state(initialize_state),
        create_processes(create_processes) {
  }
};

// The runtime value type a storage cell carries. It enumerates the value types
// the runtime library has, not the type kinds the source language has: several
// source types share one domain, and a source type the runtime cannot realize
// has no domain at all. It grows when the runtime gains a value type, never to
// mirror the source language. `kNone` is the domain of storage that carries no
// value of its own, such as a box holding a borrowed handle.
enum class ValueDomain : std::uint8_t {
  kNone,
  kPacked,
  kString,
  kReal,
  kShortReal,
  kChandle,
  kTuple,
  kDynArray,
};

// How a member's storage is realized. A borrowed handle is a box holding a
// pointer the instance does not own, the storage behind a companion reference
// to another scope. An observable cell is the subscribable variable a process
// reads, writes, and waits on. An inline value is a value the instance owns but
// no process subscribes to -- a chandle (LRM 6.14), whose pointer-sized value
// lives in the member slot and is read and written directly.
enum class MemberStorageKind : std::uint8_t {
  kBorrowedHandle,
  kObservableCell,
  kInlineValue,
};

struct MemberStorageDescriptor {
  MemberStorageKind kind = MemberStorageKind::kBorrowedHandle;
  ValueDomain domain = ValueDomain::kNone;
};

// A unit's member storage schema, in class-local member order: what a generic
// instance must realize for each member its class declares. Crosses the
// generated-runtime boundary as plain data -- a pointer plus a length, not a
// C++ container -- so a non-C++ backend supplies it without depending on a C++
// type's layout. The pointed-at descriptors are owned by whoever built the
// definition and outlive every instance of it.
struct MemberStorageSchema {
  const MemberStorageDescriptor* data = nullptr;
  std::uint32_t size = 0;

  [[nodiscard]] constexpr auto Descriptors() const
      -> std::span<const MemberStorageDescriptor> {
    return {data, size};
  }
};

// The immutable per-specialization definition of a design unit: the root
// scope's program (held by value, so a unit's whole generated behavior is one
// constant), the structural-construction entry, and the storage schema of its
// members. Construction is bootstrap-called (a JIT design) or realized by the
// backend's own constructor (the C++ backend), distinct from the per-phase
// lifecycle dispatch. The schema tells a generic instance what storage to own
// for each member; a backend that lays members out natively (the C++ backend,
// whose subclass holds real fields) leaves it empty.
struct UnitDefinition {
  ScopeProgram root;
  ScopeEntry construct = &ScopeNoOp;
  MemberStorageSchema members;

  constexpr UnitDefinition() = default;
  constexpr UnitDefinition(ScopeProgram root, ScopeEntry construct)
      : root(root), construct(construct) {
  }
};

}  // namespace lyra::runtime
