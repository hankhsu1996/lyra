#pragma once

#include <algorithm>
#include <cstdint>
#include <span>
#include <vector>

#include "lyra/common/mutation_event.hpp"
#include "lyra/common/range_set.hpp"
#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {

// Per-instance dirty tracking for instance-owned signals.
// Indexed by LocalSignalId, not by flat design-global slot_id.
//
// Maintains dual dirty lists: dirty_list_ accumulates across the time slot
// (for trace), delta_dirty_ tracks per-delta changes (for scheduler).
// Same lifecycle semantics as the global UpdateSet, but with local-shaped
// identity throughout.
class LocalUpdateSet {
 public:
  LocalUpdateSet() = default;

  // Initialize with the number of local signals and their byte sizes.
  // Must be called once before any MarkDirty calls.
  void Init(uint32_t local_signal_count, std::span<const uint32_t> sizes);

  // Mark a local signal as fully dirty.
  auto MarkSlotDirty(
      LocalSignalId signal,
      common::MutationKind kind = common::MutationKind::kValueWrite,
      common::EpochEffect epoch = common::EpochEffect::kNone) -> bool;

  // Fast path for full-extent dirty marking with default kind/epoch.
  // Skips bounds validation and range-size comparison. Caller must
  // guarantee signal is within range (i.e., Contains(signal) is true).
  void MarkSlotDirtyFull(LocalSignalId signal) {
    TouchSignal(
        signal, common::MutationKind::kValueWrite, common::EpochEffect::kNone);
    delta_ranges_[signal.value].MarkFullExtent();
  }

  // Mark a byte range within a local signal as dirty.
  void MarkDirtyRange(
      LocalSignalId signal, uint32_t byte_off, uint32_t byte_size,
      common::MutationKind kind = common::MutationKind::kValueWrite,
      common::EpochEffect epoch = common::EpochEffect::kNone);

  // All dirty local signals this time slot (for trace).
  [[nodiscard]] auto DirtySignals() const -> std::span<const LocalSignalId> {
    return dirty_list_;
  }

  // Dirty local signals since last ClearDelta (for scheduler).
  [[nodiscard]] auto DeltaDirtySignals() const
      -> std::span<const LocalSignalId> {
    return delta_dirty_;
  }

  // Per-signal delta dirty ranges.
  [[nodiscard]] auto DeltaRangesFor(LocalSignalId signal) const
      -> const common::RangeSet&;

  // Per-signal max mutation kind in current delta.
  [[nodiscard]] auto DeltaKindFor(LocalSignalId signal) const
      -> common::MutationKind;

  // Per-signal max epoch effect in current delta.
  [[nodiscard]] auto DeltaEpochFor(LocalSignalId signal) const
      -> common::EpochEffect;

  // Check whether a local signal id is within the initialized range.
  [[nodiscard]] auto Contains(LocalSignalId signal) const -> bool {
    return signal.value < seen_.size();
  }

  // Check whether a signal was dirtied in the current delta.
  [[nodiscard]] auto IsDeltaDirty(LocalSignalId signal) const -> bool {
    return Contains(signal) && delta_seen_[signal.value] != 0;
  }

  // Clear per-delta state. O(delta_dirty_count).
  void ClearDelta();

  // Clear all dirty state. O(dirty_count).
  void Clear();

  // Check if any signals are dirty (time-slot level).
  [[nodiscard]] auto IsEmpty() const -> bool {
    return dirty_list_.empty();
  }

 private:
  void TouchSignal(
      LocalSignalId signal, common::MutationKind kind,
      common::EpochEffect epoch) {
    auto id = signal.value;
    if (seen_[id] == 0) {
      seen_[id] = 1;
      dirty_list_.push_back(signal);
    }
    if (delta_seen_[id] == 0) {
      delta_seen_[id] = 1;
      delta_dirty_.push_back(signal);
    }
    delta_kinds_[id] = std::max(delta_kinds_[id], kind);
    delta_epochs_[id] = std::max(delta_epochs_[id], epoch);
  }

  std::vector<LocalSignalId> dirty_list_;
  std::vector<uint8_t> seen_;
  std::vector<LocalSignalId> delta_dirty_;
  std::vector<uint8_t> delta_seen_;

  std::vector<uint32_t> signal_sizes_;
  std::vector<common::RangeSet> delta_ranges_;
  std::vector<common::MutationKind> delta_kinds_;
  std::vector<common::EpochEffect> delta_epochs_;
};

}  // namespace lyra::runtime
