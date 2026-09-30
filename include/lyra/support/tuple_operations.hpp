#pragma once

#include <cstdint>

#include "lyra/support/value_domain.hpp"

namespace lyra::support {

struct TupleOperations;

// Where one component of a tuple sits in the tuple's storage, and what it is: a
// value of the domain named, or, where that domain is the tuple one, a tuple of
// the type `tuple` describes.
struct TupleComponent {
  std::uint32_t offset;
  ValueDomain domain;
  const TupleOperations* tuple;
};

// What one tuple type is and can do. A tuple's storage opens with the address
// of its type's table, the way an object of a C++ class with virtual functions
// opens with its vtable pointer: a library compiled once, before any tuple type
// existed, holds every tuple through one type and reaches the operations of the
// one it holds through that word.
//
// The first four are the storage's own -- copying a value, moving one, ending
// one, and writing into one already there -- and are compiled for the type by
// whoever laid it out. The rest are the operations the language defines on the
// whole value (LRM 11.4.5, 20.6.2, 20.9, 6.24.3, 6.6, 28.12.1), which are
// functions the program states for a structure type; they are called here with
// the value handles and the storage an answer is built in, the shape every
// entry answering a value has. One the type does not have is absent: a real
// member leaves no case equality and no bit stream, a stream is built only
// where the type fixes its width, and a type never a net's resolves nothing.
//
// Code compiled for the type writes its table's address into `out` whenever it
// builds a tuple, so what it answers is a tuple wherever `out` lies; the
// library, which has no type of its own to write, builds into storage whose
// caller wrote the table first. Copying and moving answer with a value the
// source still has to end; `assign` writes into a tuple that is already there,
// which goes on being that tuple.
struct TupleOperations {
  std::uint32_t size;
  std::uint32_t align;
  std::uint32_t count;
  const TupleComponent* components;

  void (*copy)(const void* value, void* out);
  void (*move)(void* value, void* out) noexcept;
  void (*destroy)(void* value) noexcept;
  void (*assign)(void* storage, const void* value);

  void* (*equal)(const void* lhs, const void* rhs, void* out);
  void* (*case_equal)(const void* lhs, const void* rhs, void* out);
  bool (*bit_identical)(const void* lhs, const void* rhs);
  bool (*has_unknown)(const void* value);

  void* (*bitstream_width)(const void* value, void* out);
  void* (*count_bits)(const void* value, const void* control_bits, void* out);
  void* (*to_bitstream)(const void* value, void* out);
  void* (*from_bitstream)(const void* bits, const void* prototype, void* out);

  void* (*resolve_tri_state)(const void* lhs, const void* rhs, void* out);
  void* (*resolve_wired_and)(const void* lhs, const void* rhs, void* out);
  void* (*resolve_wired_or)(const void* lhs, const void* rhs, void* out);
  void* (*dominating)(const void* stronger, const void* weaker, void* out);
  void* (*filled_like)(const void* prototype, const void* fill, void* out);
};

// How large the word a tuple's storage opens with is, which holds its type's
// table; every component sits after it.
inline constexpr std::uint32_t kTupleOperationsSize =
    sizeof(const TupleOperations*);

}  // namespace lyra::support
