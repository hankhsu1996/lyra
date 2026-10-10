#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>

#include "lyra/base/pool_id.hpp"

namespace lyra::lir {

// Identity of a constant value within a LIR compilation unit. A constant is
// settled before the program runs and shared by every use naming it, so the
// unit holds it once and an operand says which one -- the same relation a
// type's run-time description has to the uses that name it.
struct IntegralConstantId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const IntegralConstantId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::lir

// An `IntegralConstantId` is a value identity, so it keys hashed containers
// directly rather than being unwrapped to its raw integer at the use site.
template <>
struct std::hash<lyra::lir::IntegralConstantId> {
  auto operator()(lyra::lir::IntegralConstantId id) const noexcept
      -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
