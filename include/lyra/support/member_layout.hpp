#pragma once

#include <cstdint>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

// Where the members of a value sit, for a value of a class whose members the
// runtime holds. Two sides read it: code generation reaches a member at the
// distance this states, and the runtime lays each value out so that the
// distance is true -- asserting every figure here against the types it builds,
// which is what holds the two sides together.
//
// A value is one allocation: the kind of value first, then one slot per member
// its lineage carries, in lineage order. Every slot is one size whatever its
// member's storage kind, so a member's place is its position in the lineage
// times that size.

// The kind of value a class's members hang off. What a value is decides how
// large the part ahead of its members is, and which kind a class's values are
// follows from its lineage: a class standing in the design hierarchy has a
// scope, and every other class a plain object.
enum class ValueHolder : std::uint8_t {
  kObject,
  kScope,
};

inline constexpr std::uint32_t kMemberSlotSize = 296;

// The distance from a value's address to its first member slot.
constexpr auto MembersAt(ValueHolder holder) -> std::uint32_t {
  switch (holder) {
    case ValueHolder::kObject:
      return 56;
    case ValueHolder::kScope:
      return 192;
  }
  throw InternalError("member layout: unknown value holder");
}

// Where a class definition records how many members its lineage carries ahead
// of the ones the class declares itself, as a 32-bit count. A class whose
// lineage passes through another unit cannot know that count where it is
// compiled, so the runtime fixes it when it realizes the class and generated
// code reads it here.
inline constexpr std::uint32_t kFirstMemberAt = 144;

}  // namespace lyra::support
