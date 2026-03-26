#pragma once

#include <cstdint>
#include <format>
#include <limits>
#include <optional>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

struct DesignLayout;

// Arena-absolute byte offset in DesignState.
// This is the canonical offset kind for design-global storage positions.
struct ArenaByteOffset {
  uint64_t value = 0;
  auto operator==(const ArenaByteOffset&) const -> bool = default;
  auto operator<=>(const ArenaByteOffset&) const = default;
};

// Instance-relative byte offset (from an instance's owned-local storage base).
// Only meaningful for storage-owning slots within a specific instance.
struct InstanceByteOffset {
  uint64_t value = 0;
  auto operator==(const InstanceByteOffset&) const -> bool = default;
  auto operator<=>(const InstanceByteOffset&) const = default;
};

// Body-relative byte offset (from a body's first owned-local slot).
// Used in init descriptors, observable descriptors, and body-shaped templates.
struct BodyByteOffset {
  uint64_t value = 0;
  auto operator==(const BodyByteOffset&) const -> bool = default;
  auto operator<=>(const BodyByteOffset&) const = default;
};

// Canonical storage owner slot identity.
// Construction is checked: FromVerified() takes a DesignLayout and performs
// IsStorageOwner() before constructing. This is not a renamed SlotId -- the
// type carries the proof that the wrapped slot is a storage owner.
class StorageOwnerSlotId {
 public:
  // Checked construction. Throws InternalError if the slot is a forwarded
  // alias in the given layout.
  static auto FromVerified(mir::SlotId slot, const DesignLayout& layout)
      -> StorageOwnerSlotId;

  [[nodiscard]] auto Value() const -> mir::SlotId {
    return value_;
  }

  auto operator==(const StorageOwnerSlotId&) const -> bool = default;

 private:
  explicit StorageOwnerSlotId(mir::SlotId slot) : value_(slot) {
  }
  mir::SlotId value_{};
};

// Storage binding for a slot that owns its own storage.
// The absolute arena offset is the canonical location of this slot's bytes.
struct OwnedLocalStorage {
  ArenaByteOffset abs_byte_offset;
};

// Storage binding for a forwarded alias slot.
// The slot does not own storage; it aliases the canonical owner's storage.
// No local offset of any kind is representable here.
struct ForwardedStorageAlias {
  StorageOwnerSlotId owner;
};

// Semantic source of truth for whether a slot owns storage or aliases another
// slot's storage. Callers must pattern-match the variant to access the offset,
// making it impossible to accidentally treat a forwarded alias as if it had
// local storage.
using SlotStorageBinding =
    std::variant<OwnedLocalStorage, ForwardedStorageAlias>;

// Explicit per-instance storage base.
// Computed from the first owned-local slot in the instance's slot range.
// Never derived from a forwarded alias.
// nullopt: the instance has no owned-local storage region (all slots are
// forwarded aliases or the instance has zero slots).
struct InstanceStorageBase {
  std::optional<ArenaByteOffset> abs_byte_offset;
};

// Convert an arena-absolute offset to an instance-relative offset.
// Precondition: abs.value >= instance_base.value.
[[nodiscard]] inline auto ToInstanceOffset(
    ArenaByteOffset abs, ArenaByteOffset instance_base) -> InstanceByteOffset {
  if (abs.value < instance_base.value) {
    throw common::InternalError(
        "ToInstanceOffset",
        std::format(
            "abs {} < instance_base {}", abs.value, instance_base.value));
  }
  return InstanceByteOffset{abs.value - instance_base.value};
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

// Convert an instance-relative offset back to an arena-absolute offset.
// Precondition: no overflow.
[[nodiscard]] inline auto ToArenaOffset(
    ArenaByteOffset instance_base, InstanceByteOffset rel) -> ArenaByteOffset {
  if (rel.value > std::numeric_limits<uint64_t>::max() - instance_base.value) {
    throw common::InternalError(
        "ToArenaOffset", std::format(
                             "overflow: instance_base {} + rel {}",
                             instance_base.value, rel.value));
  }
  return ArenaByteOffset{instance_base.value + rel.value};
}

}  // namespace lyra::lowering::mir_to_llvm
