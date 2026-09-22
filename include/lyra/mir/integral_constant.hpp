#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/base/interner.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// The bits of an integral constant, and nothing else. How wide the value is,
// whether it is signed, and whether it has an unknown plane at all are the
// type's to state, and every expression carries one -- so a consumer reads them
// there and the two can never disagree.
//
// Word layout is LSB-first and 4-state encoding is (v=0,s=0)=0, (v=1,s=0)=1,
// (v=0,s=1)=Z, (v=1,s=1)=X. Nothing else is promised here: this is what a
// caller hands over, and a caller with one word in hand for a narrower type
// writes one word. What the run of words has to look like to be an entry of a
// unit's pool is on the entry below, which is where it is established.
struct IntegralConstant {
  std::vector<std::uint64_t> value_words;
  std::vector<std::uint64_t> state_words;

  auto operator==(const IntegralConstant&) const -> bool = default;
};

// A constant integral value one compilation unit holds: the bits an occurrence
// wrote and the type it wrote them at. Both belong to its identity -- the same
// bits at two types are two values, because the type decides how wide they are
// read, whether they are read as signed, and whether they carry an unknown
// plane at all.
//
// The bits are canonical for that type: one word per 64 bits of its width, the
// top word's bits above the width cleared, and the state plane empty unless the
// type carries one. Two spellings of one value would otherwise be two entries,
// and a consumer reading a plane would see bits the type does not have.
struct IntegralConstantDecl {
  TypeId type;
  IntegralConstant value;

  auto operator==(const IntegralConstantDecl&) const -> bool = default;

  struct Hash {
    auto operator()(const IntegralConstantDecl& decl) const -> std::size_t;
  };
};

// The constant integral values one compilation unit was written with. Two
// occurrences of the same value at the same type reach one entity, which is
// what lets the unit state it once and every use name which one.
using IntegralConstantPool = base::Interner<
    IntegralConstantDecl, IntegralConstantId, IntegralConstantDecl::Hash>;

}  // namespace lyra::mir
