#pragma once

#include <algorithm>
#include <cstdint>
#include <span>
#include <vector>

namespace lyra::runtime {

// Sparse dirty slot tracking with O(1) dedup for flush-based trace snapshots.
// Maintains a seen bitmap + dirty list to efficiently track which slots have
// been modified within a time slot.
class UpdateSet {
 public:
  UpdateSet() = default;

  // Initialize with the total number of slots. Must be called once before
  // MarkDirty. Called from Engine::InitSlotMeta after slot_count is known.
  void Init(uint32_t slot_count) {
    seen_.assign(slot_count, 0);
    dirty_list_.reserve(std::min(slot_count, uint32_t{256}));
  }

  // Mark a slot as dirty. O(1) with dedup via seen bitmap.
  // Silently ignores out-of-bounds slot_id (bounds check).
  void MarkDirty(uint32_t slot_id) {
    if (slot_id < seen_.size() && seen_[slot_id] == 0) {
      seen_[slot_id] = 1;
      dirty_list_.push_back(slot_id);
    }
  }

  // Clear all dirty slots. O(dirty_count), NOT O(slot_count).
  // Only touched slots are reset in the seen bitmap.
  void Clear() {
    for (uint32_t id : dirty_list_) {
      seen_[id] = 0;
    }
    dirty_list_.clear();
  }

  // Get the list of dirty slot IDs.
  [[nodiscard]] auto DirtySlots() const -> std::span<const uint32_t> {
    return dirty_list_;
  }

  // Check if any slots are dirty.
  [[nodiscard]] auto IsEmpty() const -> bool {
    return dirty_list_.empty();
  }

 private:
  std::vector<uint32_t> dirty_list_;
  std::vector<uint8_t> seen_;
};

}  // namespace lyra::runtime
