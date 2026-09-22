#pragma once

#include <compare>
#include <cstdint>

#include "lyra/base/pool_id.hpp"

namespace lyra::lir {

// Identity of one run-time description within a LIR compilation unit. A
// description is settled before the program runs and shared by every use
// naming it, so the unit holds it once and an operand says which one -- the
// same relation a constant of the unit has to the uses that reach it.
//
// It is an identity of its own and not a type's, because a type here carries
// what execution needs rather than what a declaration said: an array counted up
// and the same array counted down are one storage shape, and the descriptions
// that tell them apart have to stay two things.
struct TypeDescriptorId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const TypeDescriptorId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::lir
