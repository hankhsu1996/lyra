#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Identity of a class-level static constant -- a named, immutable value the
// class owns with static storage, built once at compile time. Scoped to the
// class that declares it (`Class::static_constants`), the data dual of a static
// method. A generated scope's behavior record is one such constant.
struct StaticConstantId {
  std::uint32_t value;

  auto operator<=>(const StaticConstantId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
