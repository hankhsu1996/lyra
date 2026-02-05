#pragma once

#include <cstdint>
#include <vector>

namespace lyra::runtime {

// Storage layout classification for a design slot.
// These are STORAGE categories, not SV syntax categories:
//   kPacked2:   snapshotable bytes, one plane, no unknown plane
//               (2-state int, real, shortreal)
//   kPacked4:   snapshotable bytes, two planes (value + unknown)
//               as laid out in DesignState (4-state int)
//   kHandle:    pointer-sized opaque handle (string, dynamic array, queue)
//   kAggregate: composite blob, snapshot entire total_bytes,
//               planes not applicable (unpacked array/struct/union)
enum class SlotStorageKind : uint8_t {
  kPacked2 = 0,
  kPacked4 = 1,
  kHandle = 2,
  kAggregate = 3,
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

// Metadata for a single design slot's byte layout within DesignState.
struct SlotMeta {
  uint32_t base_off = 0;
  uint32_t total_bytes = 0;
  SlotStorageKind kind = SlotStorageKind::kPacked2;
  PackedPlanes planes;
};

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
  [[nodiscard]] auto Get(uint32_t slot_id) const -> const SlotMeta&;

  [[nodiscard]] auto Size() const -> uint32_t;
  [[nodiscard]] auto IsPopulated() const -> bool;

  // Machine-stable dump to WriteOutput. Includes version and count header.
  // Called right after registry construction, before simulation runs.
  void DumpSummary() const;

 private:
  std::vector<SlotMeta> slots_;
};

}  // namespace lyra::runtime
