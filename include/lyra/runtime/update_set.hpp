#pragma once

#include <cstdint>
#include <span>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/mutation_event.hpp"
#include "lyra/common/range_set.hpp"

namespace lyra::runtime {

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
  void MarkDirtyRange(
      uint32_t slot_id, uint32_t off, uint32_t size,
      common::MutationKind kind = common::MutationKind::kValueWrite,
      common::EpochEffect epoch = common::EpochEffect::kNone);

  // Mark an entire slot as dirty. Wrapper for MarkDirtyRange with full slot.
  // INVARIANT: Always produces a full-slot entry [0, slot_size) in
  // delta_slot_ranges_. This guarantees Overlaps() returns true for any
  // sub-range observation, so no subscription is incorrectly skipped.
  void MarkSlotDirty(
      uint32_t slot_id,
      common::MutationKind kind = common::MutationKind::kValueWrite,
      common::EpochEffect epoch = common::EpochEffect::kNone) {
    if (slot_id < slot_sizes_.size()) {
      MarkDirtyRange(slot_id, 0, slot_sizes_[slot_id], kind, epoch);
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

  // Merged dirty ranges for a slot in the current delta.
  [[nodiscard]] auto DeltaRangesFor(uint32_t slot_id) const
      -> const common::RangeSet& {
    if (slot_id >= delta_slot_ranges_.size()) {
      static const common::RangeSet empty;
      return empty;
    }
    return delta_slot_ranges_[slot_id];
  }

  // Per-slot max mutation kind in current delta.
  [[nodiscard]] auto DeltaKindFor(uint32_t slot_id) const
      -> common::MutationKind {
    if (slot_id >= delta_slot_kinds_.size()) {
      return common::MutationKind::kValueWrite;
    }
    return delta_slot_kinds_[slot_id];
  }

  // Per-slot max epoch effect in current delta.
  [[nodiscard]] auto DeltaEpochFor(uint32_t slot_id) const
      -> common::EpochEffect {
    if (slot_id >= delta_slot_epochs_.size()) {
      return common::EpochEffect::kNone;
    }
    return delta_slot_epochs_[slot_id];
  }

  // Mark a heap-relative byte range as dirty for a container slot.
  // size==0 means "full backing buffer dirty" (overlap with everything).
  void MarkExternalDirtyRange(uint32_t slot_id, uint32_t off, uint32_t size);

  // Get external dirty ranges for scheduler. Returns empty RangeSet if none.
  [[nodiscard]] auto DeltaExternalRangesFor(uint32_t slot_id) const
      -> const common::RangeSet&;

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
  std::vector<uint32_t> dirty_list_;
  std::vector<uint8_t> seen_;
  std::vector<uint32_t> delta_dirty_;
  std::vector<uint8_t> delta_seen_;

  // Per-slot total_bytes (cached from SlotMetaRegistry at Init).
  std::vector<uint32_t> slot_sizes_;

  // Per-slot delta dirty ranges. Indexed by slot_id.
  // Cleared by ClearDelta(). Canonical semantic dirty data for scheduler.
  std::vector<common::RangeSet> delta_slot_ranges_;

  // Per-slot max mutation kind in current delta. Indexed by slot_id.
  std::vector<common::MutationKind> delta_slot_kinds_;

  // Per-slot max epoch effect in current delta. Indexed by slot_id.
  std::vector<common::EpochEffect> delta_slot_epochs_;

  // External dirty ranges (heap-relative) for container element subscriptions.
  // Sparse: only slots with external dirty facts have entries.
  absl::flat_hash_map<uint32_t, common::RangeSet> delta_external_ranges_;
};

}  // namespace lyra::runtime
