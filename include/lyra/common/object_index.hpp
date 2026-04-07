#pragma once

#include <cstdint>
#include <functional>

namespace lyra::common {

// Typed identity for a module object in the construction topology.
// Indexes into ConstructionInput::objects.
// Must be used instead of raw uint32_t for object identity across all
// topology, binding, and resolution APIs.
struct ObjectIndex {
  uint32_t value = 0;
  auto operator==(const ObjectIndex&) const -> bool = default;
};

struct ObjectIndexHash {
  auto operator()(ObjectIndex idx) const noexcept -> size_t {
    return std::hash<uint32_t>{}(idx.value);
  }
};

}  // namespace lyra::common
