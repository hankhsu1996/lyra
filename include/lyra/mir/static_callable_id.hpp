#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Identity of a class-level static callable -- a named, receiver-less callable
// the class owns in its associated namespace. Scoped to the class that declares
// it, the callable peer of a static-constant identity.
struct StaticCallableId {
  std::uint32_t value;

  auto operator<=>(const StaticCallableId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
