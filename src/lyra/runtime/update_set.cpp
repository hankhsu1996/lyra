#include "lyra/runtime/update_set.hpp"

#include <algorithm>
#include <cstdint>
#include <span>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/mutation_event.hpp"
#include "lyra/common/range_set.hpp"

namespace lyra::runtime {

void UpdateSet::Init(
    uint32_t slot_count, std::span<const uint32_t> slot_sizes) {
  if (!seen_.empty()) {
    throw common::InternalError("UpdateSet::Init", "already initialized");
  }
  if (slot_sizes.size() != slot_count) {
    throw common::InternalError(
        "UpdateSet::Init", "slot_sizes length mismatch");
  }
  seen_.assign(slot_count, 0);
  delta_seen_.assign(slot_count, 0);
  dirty_list_.reserve(std::min(slot_count, uint32_t{256}));
  delta_dirty_.reserve(std::min(slot_count, uint32_t{256}));
  slot_sizes_.assign(slot_sizes.begin(), slot_sizes.end());
  delta_slot_ranges_.resize(slot_count);
  delta_slot_kinds_.assign(slot_count, common::MutationKind::kValueWrite);
  delta_slot_epochs_.assign(slot_count, common::EpochEffect::kNone);
}

void UpdateSet::MarkDirtyRange(
    uint32_t slot_id, uint32_t off, uint32_t size, common::MutationKind kind,
    common::EpochEffect epoch) {
  if (slot_id >= seen_.size()) return;

  if (size == 0) {
    throw common::InternalError(
        "UpdateSet::MarkDirtyRange", "size must be > 0");
  }
  if (static_cast<uint64_t>(off) + static_cast<uint64_t>(size) >
      slot_sizes_[slot_id]) {
    throw common::InternalError(
        "UpdateSet::MarkDirtyRange", "range exceeds slot size");
  }

  TouchSlot(slot_id, kind, epoch);

  // Full-slot write canonicalizes to full-extent immediately.
  if (off == 0 && size == slot_sizes_[slot_id]) {
    delta_slot_ranges_[slot_id].MarkFullExtent();
  } else {
    delta_slot_ranges_[slot_id].Insert(off, size);
  }
}

void UpdateSet::MarkExternalDirtyRange(
    uint32_t slot_id, uint32_t off, uint32_t size) {
  if (slot_id >= seen_.size()) return;

  TouchSlot(
      slot_id, common::MutationKind::kValueWrite, common::EpochEffect::kNone);

  auto& range_set = delta_external_ranges_[slot_id];
  if (size == 0) {
    // Full-dirty sentinel.
    range_set.MarkFullExtent();
    return;
  }

  // Skip if already full-dirty.
  if (range_set.IsFullExtent()) return;

  range_set.Insert(off, size);
}

auto UpdateSet::DeltaExternalRangesFor(uint32_t slot_id) const
    -> const common::RangeSet& {
  auto it = delta_external_ranges_.find(slot_id);
  if (it == delta_external_ranges_.end()) {
    static const common::RangeSet empty;
    return empty;
  }
  return it->second;
}

void UpdateSet::ClearDelta() {
  for (uint32_t id : delta_dirty_) {
    delta_slot_ranges_[id].Clear();
    delta_slot_kinds_[id] = common::MutationKind::kValueWrite;
    delta_slot_epochs_[id] = common::EpochEffect::kNone;
    delta_seen_[id] = 0;
  }
  delta_dirty_.clear();
  if (!delta_external_ranges_.empty()) {
    delta_external_ranges_.clear();
  }
}

void UpdateSet::Clear() {
  for (uint32_t id : dirty_list_) {
    seen_[id] = 0;
  }
  dirty_list_.clear();
  ClearDelta();
}

}  // namespace lyra::runtime
