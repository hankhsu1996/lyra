#pragma once

#include "lyra/value/value_type.hpp"

namespace lyra::value {

// What an element type keeps for the two answers LRM 7.4.5 gives an index
// naming no element: the type's default (LRM Table 7-1), which a read returns,
// and storage no read reaches, which a write lands in. Each is built the first
// time it is asked for and is never given back, since the constant naming it
// lasts as long as the program does. A type stated as constant data cannot
// hold them itself, so it names one of these beside it.
struct ElementScratch {
  void* element_default = nullptr;
  void* discarded_write = nullptr;
};

// The type of an array's elements as code compiled before any design existed
// is handed it: the type, and how a value of it starts (LRM Table 7-1). What a
// position of an array holds before anything is written to it is a fact of the
// element type alone, so no array, of whatever kind and size, holds a default
// of its own.
//
// Each unit of generated code states one of these for each element type it
// names, as constant data of this class, so its layout is part of what the two
// sides agree on.
class ElementType {
 public:
  // Builds the type's default in the storage it is handed, and answers that
  // storage.
  using BuildDefault = void* (*)(void* out);

  constexpr ElementType(
      const ValueType& type, BuildDefault build_default,
      ElementScratch& scratch)
      : type_(&type), build_default_(build_default), scratch_(&scratch) {
  }

  [[nodiscard]] auto Type() const -> const ValueType& {
    return *type_;
  }

  // LRM 7.4.5: what a read through an index naming no element returns, and
  // where a write through one lands. The second holds the default again each
  // time it is handed out, so a discarded write never shows through a later
  // one.
  [[nodiscard]] auto Default() const -> const void*;
  [[nodiscard]] auto DiscardedWrite() const -> void*;

  // Where the type states each of its own parts, which code stating one as
  // constant data places them by.
  [[nodiscard]] auto TypeStatedAt() const -> StatedAt;
  [[nodiscard]] auto BuildDefaultStatedAt() const -> StatedAt;
  [[nodiscard]] auto ScratchStatedAt() const -> StatedAt;

 private:
  const ValueType* type_;
  BuildDefault build_default_;
  ElementScratch* scratch_;
};

}  // namespace lyra::value
