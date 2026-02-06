#include "lyra/runtime/update_set.hpp"

#include <algorithm>
#include <cstdint>
#include <span>
#include <vector>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

auto RangesOverlap(
    std::span<const DirtyRange> dirty, uint32_t obs_off, uint32_t obs_size)
    -> bool {
  uint64_t obs_end =
      static_cast<uint64_t>(obs_off) + static_cast<uint64_t>(obs_size);
  for (const auto& r : dirty) {
    uint64_t r_end =
        static_cast<uint64_t>(r.off) + static_cast<uint64_t>(r.size);
    if (r.off < obs_end && obs_off < r_end) return true;
    if (r.off >= obs_end) break;
  }
  return false;
}

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
}

void UpdateSet::MarkDirtyRange(uint32_t slot_id, uint32_t off, uint32_t size) {
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

  // Slot-level dedup (iteration indices).
  if (seen_[slot_id] == 0) {
    seen_[slot_id] = 1;
    dirty_list_.push_back(slot_id);
  }
  if (delta_seen_[slot_id] == 0) {
    delta_seen_[slot_id] = 1;
    delta_dirty_.push_back(slot_id);
  }

  // Range storage + merge (canonical data).
  InsertAndMerge(delta_slot_ranges_[slot_id], off, size);
}

void UpdateSet::ClearDelta() {
  for (uint32_t id : delta_dirty_) {
    delta_slot_ranges_[id].clear();
    delta_seen_[id] = 0;
  }
  delta_dirty_.clear();
}

void UpdateSet::Clear() {
  for (uint32_t id : dirty_list_) {
    seen_[id] = 0;
  }
  dirty_list_.clear();
  ClearDelta();
}

void UpdateSet::InsertAndMerge(
    std::vector<DirtyRange>& ranges, uint32_t off, uint32_t size) {
  uint64_t new_start = off;
  uint64_t new_end = static_cast<uint64_t>(off) + static_cast<uint64_t>(size);

  // Find first range whose end >= new_start (could overlap or be adjacent).
  auto it = ranges.begin();
  while (it != ranges.end()) {
    uint64_t r_end =
        static_cast<uint64_t>(it->off) + static_cast<uint64_t>(it->size);
    if (r_end >= new_start) break;
    ++it;
  }

  // If no existing range overlaps/adjoins, insert before it.
  if (it == ranges.end() || static_cast<uint64_t>(it->off) > new_end) {
    ranges.insert(it, DirtyRange{.off = off, .size = size});
    return;
  }

  // Merge: extend the found range to cover the union.
  new_start = std::min(new_start, static_cast<uint64_t>(it->off));
  uint64_t r_end =
      static_cast<uint64_t>(it->off) + static_cast<uint64_t>(it->size);
  new_end = std::max(new_end, r_end);

  // Absorb subsequent overlapping/adjacent ranges.
  auto merge_end = it + 1;
  while (merge_end != ranges.end() &&
         static_cast<uint64_t>(merge_end->off) <= new_end) {
    uint64_t me = static_cast<uint64_t>(merge_end->off) +
                  static_cast<uint64_t>(merge_end->size);
    new_end = std::max(new_end, me);
    ++merge_end;
  }

  // Write merged range and erase absorbed entries.
  it->off = static_cast<uint32_t>(new_start);
  it->size = static_cast<uint32_t>(new_end - new_start);
  ranges.erase(it + 1, merge_end);
}

}  // namespace lyra::runtime
