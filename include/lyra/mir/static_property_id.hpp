#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Identity of a class static property (LRM 8.9) -- a named, mutable
// type-associated storage cell the class owns, shared by every instance.
// Scoped to the class that declares it (`Class::static_properties`), peer of
// `FieldId` on the instance-member axis and of `StaticConstantId` on the
// type-associated axis. Distinct from `StaticConstantId`: a static constant is
// immutable and built once at compile time (the data dual of a static
// method); a static property is a run-time cell writable through ordinary
// assignment (LRM 8.9).
struct StaticPropertyId {
  std::uint32_t value;

  auto operator<=>(const StaticPropertyId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
