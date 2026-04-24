#pragma once

#include <cstdint>

namespace lyra::common {

// Instance-relative byte offset within an instance's owned storage.
// Used across codegen and runtime layers for instance-owned storage resolution.
struct InstanceByteOffset {
  uint64_t value = 0;
  auto operator==(const InstanceByteOffset&) const -> bool = default;
  auto operator<=>(const InstanceByteOffset&) const = default;
};

}  // namespace lyra::common
