#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Identity for a deferred-check site (`$assert final` / unique-cascade
// observed submit). Compile-time-allocated per site by the compilation unit;
// the engine indexes its per-scope pending-closure table by the same id.
struct DeferredCheckSiteId {
  std::uint32_t value;

  auto operator<=>(const DeferredCheckSiteId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
