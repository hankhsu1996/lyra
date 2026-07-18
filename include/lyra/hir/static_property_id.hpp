#pragma once

#include <compare>
#include <cstdint>
#include <functional>

namespace lyra::hir {

// The declaration-order position of a class's static property (LRM 8.9)
// within its owning class's static-property arena. A static property is a
// type-associated storage cell, separate from an instance field; its arena is
// distinct from `ClassDecl.fields`, and a `StaticPropertyId` never crosses
// from one to the other.
struct StaticPropertyId {
  std::uint32_t value;

  auto operator<=>(const StaticPropertyId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::hir

template <>
struct std::hash<lyra::hir::StaticPropertyId> {
  auto operator()(lyra::hir::StaticPropertyId id) const noexcept
      -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
