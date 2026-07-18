#pragma once

#include <compare>
#include <cstdint>
#include <functional>

namespace lyra::hir {

// The declaration-order position of a class or aggregate field within its
// owning declaration's field arena. Two `FieldId` values compare on the raw
// position; the arena that mints them decides which declaration the id
// belongs to, so an id from one arena is never a valid handle in another.
struct FieldId {
  std::uint32_t value;

  auto operator<=>(const FieldId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::hir

template <>
struct std::hash<lyra::hir::FieldId> {
  auto operator()(lyra::hir::FieldId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
