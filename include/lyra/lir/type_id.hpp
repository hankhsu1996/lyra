#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>

namespace lyra::lir {

struct TypeId {
  std::uint32_t value;

  auto operator<=>(const TypeId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::lir

// A `TypeId` is a value identity, so it keys hashed containers directly rather
// than being unwrapped to its raw integer at the use site.
template <>
struct std::hash<lyra::lir::TypeId> {
  auto operator()(lyra::lir::TypeId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
