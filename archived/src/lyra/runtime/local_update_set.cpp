#include "lyra/runtime/local_update_set.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <span>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

void LocalUpdateSet::Init(
    uint32_t local_signal_count, std::span<const uint32_t> sizes) {
  if (!seen_.empty()) {
    throw common::InternalError("LocalUpdateSet::Init", "already initialized");
  }
  if (sizes.size() != local_signal_count) {
    throw common::InternalError(
        "LocalUpdateSet::Init", std::format(
                                    "sizes length {} != local_signal_count {}",
                                    sizes.size(), local_signal_count));
  }
  seen_.assign(local_signal_count, 0);
  delta_seen_.assign(local_signal_count, 0);
  dirty_list_.reserve(std::min(local_signal_count, uint32_t{64}));
  delta_dirty_.reserve(std::min(local_signal_count, uint32_t{64}));
  signal_sizes_.assign(sizes.begin(), sizes.end());
  delta_ranges_.resize(local_signal_count);
  delta_kinds_.assign(local_signal_count, common::MutationKind::kValueWrite);
  delta_epochs_.assign(local_signal_count, common::EpochEffect::kNone);
}

auto LocalUpdateSet::MarkSlotDirty(
    LocalSignalId signal, common::MutationKind kind, common::EpochEffect epoch)
    -> bool {
  if (!Contains(signal)) return false;
  bool first_delta = (delta_seen_[signal.value] == 0);
  TouchSignal(signal, kind, epoch);
  delta_ranges_[signal.value].MarkFullExtent();
  return first_delta;
}

void LocalUpdateSet::MarkDirtyRange(
    LocalSignalId signal, uint32_t byte_off, uint32_t byte_size,
    common::MutationKind kind, common::EpochEffect epoch) {
  if (!Contains(signal)) return;

  if (byte_size == 0) {
    throw common::InternalError(
        "LocalUpdateSet::MarkDirtyRange", "size must be > 0");
  }
  if (static_cast<uint64_t>(byte_off) + static_cast<uint64_t>(byte_size) >
      signal_sizes_[signal.value]) {
    throw common::InternalError(
        "LocalUpdateSet::MarkDirtyRange",
        std::format(
            "range [{}, +{}) exceeds signal size {}", byte_off, byte_size,
            signal_sizes_[signal.value]));
  }

  TouchSignal(signal, kind, epoch);

  auto total = signal_sizes_[signal.value];
  if (byte_off == 0 && byte_size == total) {
    delta_ranges_[signal.value].MarkFullExtent();
  } else {
    delta_ranges_[signal.value].Insert(byte_off, byte_size);
  }
}

auto LocalUpdateSet::DeltaRangesFor(LocalSignalId signal) const
    -> const common::RangeSet& {
  if (!Contains(signal)) {
    static const common::RangeSet kEmpty;
    return kEmpty;
  }
  return delta_ranges_[signal.value];
}

auto LocalUpdateSet::DeltaKindFor(LocalSignalId signal) const
    -> common::MutationKind {
  if (!Contains(signal)) return common::MutationKind::kValueWrite;
  return delta_kinds_[signal.value];
}

auto LocalUpdateSet::DeltaEpochFor(LocalSignalId signal) const
    -> common::EpochEffect {
  if (!Contains(signal)) return common::EpochEffect::kNone;
  return delta_epochs_[signal.value];
}

void LocalUpdateSet::ClearDelta() {
  for (auto lid : delta_dirty_) {
    delta_ranges_[lid.value].Clear();
    delta_kinds_[lid.value] = common::MutationKind::kValueWrite;
    delta_epochs_[lid.value] = common::EpochEffect::kNone;
    delta_seen_[lid.value] = 0;
  }
  delta_dirty_.clear();
}

void LocalUpdateSet::Clear() {
  for (auto lid : dirty_list_) {
    seen_[lid.value] = 0;
  }
  dirty_list_.clear();
  ClearDelta();
}

}  // namespace lyra::runtime
