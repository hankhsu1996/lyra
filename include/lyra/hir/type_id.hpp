#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>

#include "lyra/base/pool_id.hpp"

namespace lyra::hir {

struct TypeId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const TypeId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::hir

// A `TypeId` is a value identity, so it keys hashed containers directly rather
// than being unwrapped to its raw integer at the use site.
template <>
struct std::hash<lyra::hir::TypeId> {
  auto operator()(lyra::hir::TypeId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
