#pragma once

#include <cstdint>
#include <span>
#include <string_view>

namespace lyra::runtime {

struct ObjectDefinition;

// A run of entries a unit holds as a constant array: where the array is and how
// many entries it has, which is what a table is in a C program and what a slice
// is in Rust. The array is named as storage rather than as its first entry, so
// a producer states its address with no conversion of its own.
template <typename T>
struct ConstantRun {
  const void* data = nullptr;
  std::uint64_t size = 0;

  [[nodiscard]] auto Entries() const -> std::span<const T> {
    return {static_cast<const T*>(data), size};
  }
};

// The time unit or precision power a scope reports when it declares no
// timescale of its own (the synthetic `$root`). The engine's design-global
// precision minimum ignores it, so a purely structural node does not pull the
// simulation tick finer.
inline constexpr std::int8_t kUnspecifiedTimePower = 127;

// A scope's immutable constant properties, known when its class is declared
// and never computed by running generated code: its effective time unit and
// precision as powers of ten (LRM Table 20-2), each the scope's own timescale
// or the one it inherits (LRM 3.14.2.3).
struct ScopeMetadata {
  std::int8_t time_unit_power = kUnspecifiedTimePower;
  std::int8_t time_precision_power = kUnspecifiedTimePower;
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

// One callable a scope answers for by name: the name a caller spells, and the
// entry adapting that call to this scope's own subroutine. The caller supplies
// the scope as the entry's first argument, the way every callable takes its
// receiver. The name is the declaring unit's own NUL-terminated constant.
struct ScopeCallable {
  const char* name = nullptr;
  ErasedEntry entry = nullptr;
};

// The entry published under `name`, or null when the table holds none. One
// scan, shared by every namespace a scope answers names in.
[[nodiscard]] inline auto FindInCallableTable(
    ConstantRun<ScopeCallable> table, std::string_view name) -> ErasedEntry {
  for (const ScopeCallable& published : table.Entries()) {
    if (published.name == name) {
      return published.entry;
    }
  }
  return nullptr;
}

// One class a scope answers for by name: the identifier the source gave the
// class, and the definition every object of it is built from. A class declared
// inside a design element is a distinct type per instance of that element (LRM
// 6.22) and is nameable only inside the scope declaring it (LRM 23.9), so a
// referrer outside reaches it the way it reaches anything else past a signature
// -- by walking to the scope and asking.
struct ScopeClass {
  const char* name = nullptr;
  const ObjectDefinition* definition = nullptr;
};

// The definition published under `name`, or null when the table holds none.
[[nodiscard]] inline auto FindInClassTable(
    ConstantRun<ScopeClass> table, std::string_view name)
    -> const ObjectDefinition* {
  for (const ScopeClass& published : table.Entries()) {
    if (published.name == name) {
      return published.definition;
    }
  }
  return nullptr;
}

// What a class of the design hierarchy tells this library about its instances
// beyond what its type information does: its timescale, and the names an
// instance answers while references resolve. What an instance does in each
// phase, and how it ends, are virtual functions of its class. What a unit
// promises of its object has none of this, since no instance is built of it.
//
// A scope holds one callable table per namespace it answers names in, because a
// DPI-C export's name is the program-global C identifier the foreign side calls
// (LRM 35.4) while a subroutine's is the SV identifier a hierarchical name
// spells (LRM 23.6), and one declaration may carry both under different
// spellings. The classes its unit declares are a third name space, holding
// class definitions rather than entries.
struct ScopeInfo {
  ScopeMetadata metadata;
  ConstantRun<ScopeCallable> exports;
  ConstantRun<ScopeCallable> subroutines;
  ConstantRun<ScopeClass> classes;
};

}  // namespace lyra::runtime
