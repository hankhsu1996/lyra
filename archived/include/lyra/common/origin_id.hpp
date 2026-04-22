#pragma once

#include <cstdint>

namespace lyra::common {

// Opaque identifier for tracing errors back to source locations.
// Allocated by OriginMap, resolved against HIR/MIR artifacts.
struct OriginId {
  uint32_t value;

  static constexpr auto Invalid() -> OriginId {
    return {UINT32_MAX};
  }
  [[nodiscard]] auto IsValid() const -> bool {
    return value != UINT32_MAX;
  }

  auto operator==(const OriginId&) const -> bool = default;
};

}  // namespace lyra::common
