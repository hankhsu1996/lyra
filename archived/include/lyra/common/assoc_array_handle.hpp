#pragma once

#include <cstdint>

namespace lyra::runtime {

struct AssocArrayHandle {
  uint32_t id = UINT32_MAX;
  auto operator==(const AssocArrayHandle&) const -> bool = default;
  explicit operator bool() const {
    return id != UINT32_MAX;
  }
};

constexpr AssocArrayHandle kNullAssocHandle{};

}  // namespace lyra::runtime
