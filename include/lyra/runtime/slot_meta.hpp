#pragma once

#include <cstdint>
#include <span>
#include <vector>

#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {

class OutputDispatcher;

// Storage layout classification for a design slot.
// These are STORAGE categories, not SV syntax categories:
//   kPacked2:   snapshotable bytes, one plane, no unknown plane
//               (2-state int, real, shortreal)
//   kPacked4:   snapshotable bytes, two planes (value + unknown)
//               as laid out in DesignState (4-state int)
//   kString:    pointer-sized handle, deep snapshot via LyraStringAsView
//   kHandle:    pointer-sized opaque handle (dynamic array, queue)
//               identity tracking only (raw pointer bytes)
//   kAggregate: composite blob, snapshot entire total_bytes,
//               planes not applicable (unpacked array/struct/union)
enum class SlotStorageKind : uint8_t {
  kPacked2 = 0,
  kPacked4 = 1,
  kString = 2,
  kHandle = 3,
  kAggregate = 4,
};

// Byte-level layout of value and unknown planes within a kPacked4 slot.
// All offsets are relative to the slot's base_off.
// INVARIANT: must be all-zero for non-kPacked4 kinds.
struct PackedPlanes {
  uint32_t value_off = 0;
  uint32_t value_bytes = 0;
  uint32_t unk_off = 0;
  uint32_t unk_bytes = 0;
};

// Storage domain for a design slot.
enum class SlotStorageDomain : uint8_t {
  // Package/global slot: bytes live in the design-global arena at
  // design_base_off.
  kDesignGlobal = 0,
  // Owned module-local slot: bytes live in the owning RuntimeInstance's
  // heap-allocated inline storage at instance_rel_off.
  kInstanceOwned = 1,
};

// Metadata for a single design slot's storage location and byte layout.
struct SlotMeta {
  SlotStorageDomain domain = SlotStorageDomain::kDesignGlobal;

  // For kDesignGlobal: arena-absolute byte offset within design_state.
  uint32_t design_base_off = 0;
  // For kInstanceOwned: instance_id of the owning RuntimeInstance.
  InstanceId owner_instance_id = InstanceId{0};
  // For kInstanceOwned: body-relative byte offset within the instance's
  // inline storage.
  uint32_t instance_rel_off = 0;

  uint32_t total_bytes = 0;
  SlotStorageKind kind = SlotStorageKind::kPacked2;
  PackedPlanes planes;
  // Canonical storage owner slot id. Self for storage owners.
  // For forwarded aliases, points to the canonical owner.
  uint32_t storage_owner_slot_id = 0;
};

struct RuntimeInstance;

// Resolve a body-relative byte offset against instance's two-region storage.
// Offsets in [0, inline_size) map to inline_base.
// Offsets in [inline_size, inline_size + appendix_size) map to appendix_base.
// Throws InternalError if the access range exceeds total storage.
[[nodiscard]] auto ResolveInstanceStorageOffset(
    const RuntimeInstance& instance, uint32_t rel_off, uint32_t access_size,
    const char* caller) -> uint8_t*;

// Resolve the byte address of a slot's storage given its metadata.
// For kDesignGlobal: returns design_state_base + design_base_off.
// For kInstanceOwned: dispatches through ResolveInstanceStorageOffset.
[[nodiscard]] auto ResolveSlotBase(
    const SlotMeta& meta, const void* design_state_base,
    std::span<const RuntimeInstance* const> instances) -> const uint8_t*;

[[nodiscard]] auto ResolveSlotBaseMut(
    const SlotMeta& meta, void* design_state_base,
    std::span<const RuntimeInstance* const> instances) -> uint8_t*;

// Dense registry of slot metadata, indexed by slot_id.
// One-time initialized from the ABI word table passed to LyraRunSimulation.
class SlotMetaRegistry {
 public:
  SlotMetaRegistry() = default;

  // Parse and validate the flat uint32_t word table.
  // Throws InternalError on: version mismatch, unknown kind, zero total_bytes,
  // non-zero plane fields for non-kPacked4, plane consistency violations.
  explicit SlotMetaRegistry(const uint32_t* words, uint32_t count);

  // Access metadata. Throws InternalError if slot_id >= Size().
  [[nodiscard]] auto Get(uint32_t slot_id) const -> const SlotMeta& {
    if (slot_id >= slots_.size()) [[unlikely]] {
      ThrowOutOfRange(slot_id);
    }
    return slots_[slot_id];
  }

  // Resolve to canonical storage owner slot id.
  // For storage owners, returns the input slot_id.
  // For forwarded aliases, returns the canonical owner.
  [[nodiscard]] auto GetStorageOwnerSlotId(uint32_t slot_id) const -> uint32_t {
    return Get(slot_id).storage_owner_slot_id;
  }

  [[nodiscard]] auto Size() const -> uint32_t {
    return static_cast<uint32_t>(slots_.size());
  }
  [[nodiscard]] auto IsPopulated() const -> bool {
    return !slots_.empty();
  }
  [[nodiscard]] auto MaxExtent() const -> uint32_t {
    return max_extent_;
  }

  // R4: Reserve capacity for incremental building from bundles.
  void Reserve(uint32_t count) {
    slots_.reserve(count);
  }

  // R4: Append a fully-formed slot entry. Used by engine bundle init
  // to build the registry from structured inputs instead of word tables.
  void AppendSlot(SlotMeta meta);

  // Machine-stable dump via WriteProtocolRecord. Includes version and count
  // header.
  // Called right after registry construction, before simulation runs.
  void DumpSummary(OutputDispatcher& out) const;

 private:
  [[noreturn]] void ThrowOutOfRange(uint32_t slot_id) const;

  std::vector<SlotMeta> slots_;
  uint32_t max_extent_ = 0;
};

}  // namespace lyra::runtime
