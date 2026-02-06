#pragma once

#include <algorithm>
#include <cstdint>
#include <span>
#include <vector>

namespace lyra::runtime {

// Sparse dirty slot tracking with O(1) dedup.
// Maintains dual dirty lists: dirty_list_ accumulates across the time slot
// (for trace), delta_dirty_ tracks per-delta changes (for scheduler).
class UpdateSet {
 public:
  UpdateSet() = default;

  // Initialize with the total number of slots. Must be called once before
  // MarkDirty. Called from Engine::InitSlotMeta after slot_count is known.
  void Init(uint32_t slot_count) {
    seen_.assign(slot_count, 0);
    delta_seen_.assign(slot_count, 0);
    dirty_list_.reserve(std::min(slot_count, uint32_t{256}));
    delta_dirty_.reserve(std::min(slot_count, uint32_t{256}));
  }

  // Mark a slot as dirty. O(1) with dedup via seen bitmaps.
  // Silently ignores out-of-bounds slot_id (bounds check).
  void MarkDirty(uint32_t slot_id) {
    if (slot_id < seen_.size()) {
      if (seen_[slot_id] == 0) {
        seen_[slot_id] = 1;
        dirty_list_.push_back(slot_id);
      }
      if (delta_seen_[slot_id] == 0) {
        delta_seen_[slot_id] = 1;
        delta_dirty_.push_back(slot_id);
      }
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

  // Called at each delta boundary (by FlushSignalUpdates).
  // O(delta_dirty_count), NOT O(slot_count).
  void ClearDelta() {
    for (uint32_t id : delta_dirty_) {
      delta_seen_[id] = 0;
    }
    delta_dirty_.clear();
  }

  // Clear all dirty slots. O(dirty_count), NOT O(slot_count).
  // Called at end of time slot (by FlushDirtySlots). Resets everything.
  void Clear() {
    for (uint32_t id : dirty_list_) {
      seen_[id] = 0;
    }
    dirty_list_.clear();
    ClearDelta();
  }

  // Check if any slots are dirty (time-slot level).
  [[nodiscard]] auto IsEmpty() const -> bool {
    return dirty_list_.empty();
  }

 private:
  std::vector<uint32_t> dirty_list_;
  std::vector<uint8_t> seen_;
  std::vector<uint32_t> delta_dirty_;
  std::vector<uint8_t> delta_seen_;
};

}  // namespace lyra::runtime
