#pragma once

#include <cstdint>
#include <utility>

namespace lyra::common {

// Design-global slot identity. Used across MIR, codegen, and runtime layers.
struct SlotId {
  uint32_t value = UINT32_MAX;

  auto operator==(const SlotId&) const -> bool = default;
  auto operator<=>(const SlotId&) const = default;
  explicit operator bool() const {
    return value != UINT32_MAX;
  }

  template <typename H>
  friend auto AbslHashValue(H h, SlotId id) -> H {
    return H::combine(std::move(h), id.value);
  }
};

constexpr SlotId kInvalidSlotId{UINT32_MAX};

}  // namespace lyra::common
