#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>

#include "lyra/base/pool_id.hpp"

namespace lyra::lir {

// Identity of one enumeration's member table within a LIR compilation unit. A
// table is settled before the program runs and shared by every use naming it,
// so the unit holds it once and an operand says which one -- the same relation
// a constant of the unit has to the uses that reach it.
//
// It is an identity of its own and not a type's, because an enumeration is its
// base type here: a type says nothing about which members a use meant.
struct EnumTableId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const EnumTableId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir

// An `EnumTableId` is a value identity, so it keys hashed containers directly
// rather than being unwrapped to its raw integer at the use site.
template <>
struct std::hash<lyra::lir::EnumTableId> {
  auto operator()(lyra::lir::EnumTableId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
