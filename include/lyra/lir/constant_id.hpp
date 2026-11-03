#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>

#include "lyra/base/pool_id.hpp"

namespace lyra::lir {

// Identity of one value a LIR compilation unit settled before the program
// runs: the same position the semantic unit holds it at, so an operand and a
// constant naming another say which one.
struct ConstantId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ConstantId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir

// A `ConstantId` keys hashed containers directly rather than being unwrapped
// to its raw integer at the use site.
template <>
struct std::hash<lyra::lir::ConstantId> {
  auto operator()(lyra::lir::ConstantId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
