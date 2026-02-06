#pragma once

#include <cstdint>
#include <span>
#include <vector>

namespace lyra::runtime {

// Byte range within a slot that was modified.
struct DirtyRange {
  uint32_t off;
  uint32_t size;
};

// Check if any dirty range overlaps the observation region [obs_off, obs_off +
// obs_size). Dirty ranges must be sorted by off (as maintained by UpdateSet).
auto RangesOverlap(
    std::span<const DirtyRange> dirty, uint32_t obs_off, uint32_t obs_size)
    -> bool;

// Sparse dirty slot tracking with O(1) dedup and per-slot byte-range tracking.
// Maintains dual dirty lists: dirty_list_ accumulates across the time slot
// (for trace), delta_dirty_ tracks per-delta changes (for scheduler).
//
// delta_slot_ranges_ is the canonical semantic dirty data. dirty_list_ /
// delta_dirty_ are iteration indices -- the set of slot IDs with non-empty
// ranges. Invariant: a slot appears in delta_dirty_ iff
// delta_slot_ranges_[slot_id] is non-empty.
class UpdateSet {
 public:
  UpdateSet() = default;

  // Initialize with the total number of slots and their byte sizes.
  // Must be called once before MarkDirty/MarkDirtyRange.
  // Called from Engine::InitSlotMeta after slot_count is known.
  void Init(uint32_t slot_count, std::span<const uint32_t> slot_sizes);

  // Mark a byte range within a slot as dirty.
  // Strict validation when initialized: size must be > 0, range must not
  // exceed slot size. Safe no-op if not initialized or slot_id OOB.
  void MarkDirtyRange(uint32_t slot_id, uint32_t off, uint32_t size);

  // Mark an entire slot as dirty. Wrapper for MarkDirtyRange with full slot.
  void MarkDirty(uint32_t slot_id) {
    if (slot_id < slot_sizes_.size()) {
      MarkDirtyRange(slot_id, 0, slot_sizes_[slot_id]);
    }
  }

  // All dirty slots this time slot (deduped across time slot). For trace.
  [[nodiscard]] auto DirtySlots() const -> std::span<const uint32_t> {
    return dirty_list_;
  }

  // Dirty slots since last ClearDelta() (deduped per delta). For scheduler.
  [[nodiscard]] auto DeltaDirtySlots() const -> std::span<const uint32_t> {
    return delta_dirty_;
  }

  // Merged dirty ranges for a slot in the current delta. Empty if OOB.
  [[nodiscard]] auto DeltaRangesFor(uint32_t slot_id) const
      -> std::span<const DirtyRange> {
    if (slot_id >= delta_slot_ranges_.size()) return {};
    return delta_slot_ranges_[slot_id];
  }

  // Called at each delta boundary (by FlushSignalUpdates).
  // O(delta_dirty_count), NOT O(slot_count).
  void ClearDelta();

  // Clear all dirty slots. O(dirty_count), NOT O(slot_count).
  // Called at end of time slot (by FlushDirtySlots). Resets everything.
  void Clear();

  // Check if any slots are dirty (time-slot level).
  [[nodiscard]] auto IsEmpty() const -> bool {
    return dirty_list_.empty();
  }

 private:
  // Sorted insert + in-place merge of overlapping/adjacent ranges.
  // Postcondition: ranges sorted by off, non-overlapping, non-adjacent.
  static void InsertAndMerge(
      std::vector<DirtyRange>& ranges, uint32_t off, uint32_t size);

  std::vector<uint32_t> dirty_list_;
  std::vector<uint8_t> seen_;
  std::vector<uint32_t> delta_dirty_;
  std::vector<uint8_t> delta_seen_;

  // Per-slot total_bytes (cached from SlotMetaRegistry at Init).
  std::vector<uint32_t> slot_sizes_;

  // Per-slot delta dirty ranges. Indexed by slot_id.
  // Each inner vector is sorted by off, non-overlapping (merged on insert).
  // Cleared by ClearDelta(). Canonical semantic dirty data for scheduler.
  std::vector<std::vector<DirtyRange>> delta_slot_ranges_;
};

}  // namespace lyra::runtime
