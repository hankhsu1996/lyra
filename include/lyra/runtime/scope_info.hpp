#pragma once

#include <cstdint>
#include <span>
#include <string_view>

namespace lyra::runtime {

// The entries a unit holds as a constant array: where the array is and how
// many entries it has, which is what a table is in a C program, a `std::span`
// is in C++, and a slice is in Rust. The array is named as storage rather than
// as its first entry, so a producer states its address with no conversion of
// its own.
template <typename T>
struct ConstantSpan {
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

// One DPI-C export a scope answers (LRM 35.5.3): the C identifier the foreign
// side calls, and the entry adapting that call to this scope's own subroutine.
// The caller supplies the scope as the entry's first argument, the way every
// callable takes its receiver. The name is the declaring unit's own
// NUL-terminated constant.
struct ScopeCallable {
  const char* name = nullptr;
  ErasedEntry entry = nullptr;
};

// The entry published under `name`, or null when the table holds none.
[[nodiscard]] inline auto FindInCallableTable(
    ConstantSpan<ScopeCallable> table, std::string_view name) -> ErasedEntry {
  for (const ScopeCallable& published : table.Entries()) {
    if (published.name == name) {
      return published.entry;
    }
  }
  return nullptr;
}

// What a class of the design hierarchy tells this library about its instances
// beyond what its type information does: its timescale, and the DPI-C exports
// an instance answers. A DPI-C export's name is the program-global C identifier
// the foreign side calls (LRM 35.4), so the foreign side has no type to reach
// the body through and finds it by that name. What an instance does in each
// phase, and how it ends, are virtual functions of its class. What a unit
// publishes of its object has none of this, since no instance is built of it.
struct ScopeInfo {
  ScopeMetadata metadata;
  ConstantSpan<ScopeCallable> exports;
};

}  // namespace lyra::runtime
