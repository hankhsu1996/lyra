#pragma once

#include <cstdint>
#include <format>
#include <optional>

#include "lyra/common/byte_offset.hpp"
#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::mir_to_llvm {

// Arena-absolute byte offset in DesignState.
// This is the canonical offset kind for design-global storage positions.
struct ArenaByteOffset {
  uint64_t value = 0;
  auto operator==(const ArenaByteOffset&) const -> bool = default;
  auto operator<=>(const ArenaByteOffset&) const = default;
};

// Body-relative byte offset (from a body's first slot).
// Used in init descriptors, observable descriptors, and body-shaped templates.
struct BodyByteOffset {
  uint64_t value = 0;
  auto operator==(const BodyByteOffset&) const -> bool = default;
  auto operator<=>(const BodyByteOffset&) const = default;
};

// Storage binding for a slot that owns its own storage.
// Every body-local slot has one of these after R2.
struct OwnedLocalStorage {
  ArenaByteOffset abs_byte_offset;
};

// Explicit per-instance storage base.
// Computed from the first slot in the instance's slot range.
// nullopt: the instance has zero slots.
struct InstanceStorageBase {
  std::optional<ArenaByteOffset> abs_byte_offset;
};

// Convert an arena-absolute offset to an instance-relative offset.
// Precondition: abs.value >= instance_base.value.
[[nodiscard]] inline auto ToInstanceOffset(
    ArenaByteOffset abs, ArenaByteOffset instance_base)
    -> common::InstanceByteOffset {
  if (abs.value < instance_base.value) {
    throw common::InternalError(
        "ToInstanceOffset",
        std::format(
            "abs {} < instance_base {}", abs.value, instance_base.value));
  }
  return common::InstanceByteOffset{abs.value - instance_base.value};
}

// Convert an arena-absolute offset to a body-relative offset.
// Precondition: abs.value >= body_base.value.
[[nodiscard]] inline auto ToBodyOffset(
    ArenaByteOffset abs, ArenaByteOffset body_base) -> BodyByteOffset {
  if (abs.value < body_base.value) {
    throw common::InternalError(
        "ToBodyOffset",
        std::format("abs {} < body_base {}", abs.value, body_base.value));
  }
  return BodyByteOffset{abs.value - body_base.value};
}

}  // namespace lyra::lowering::mir_to_llvm
