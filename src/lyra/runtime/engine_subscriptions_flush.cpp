#include <cstdint>
#include <cstring>
#include <format>
#include <span>
#include <vector>

#include "lyra/common/bit_target_mapping.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/index_plan.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/dyn_array_data.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/index_plan.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/update_set.hpp"

namespace lyra::runtime {

auto Engine::EvaluateEdge(common::EdgeKind edge, bool old_lsb, bool new_lsb)
    -> bool {
  switch (edge) {
    case common::EdgeKind::kPosedge:
      return !old_lsb && new_lsb;
    case common::EdgeKind::kNegedge:
      return old_lsb && !new_lsb;
    case common::EdgeKind::kAnyChange:
      return true;
  }
  return false;
}

void Engine::RebindSubscription(uint32_t edge_target_id) {
  if (edge_target_id >= edge_target_table_.size()) {
    throw common::InternalError(
        "Engine::RebindSubscription",
        std::format(
            "edge_target_id {} >= table size {}", edge_target_id,
            edge_target_table_.size()));
  }
  auto& target_handle = edge_target_table_[edge_target_id];
  if (target_handle.slot_id == UINT32_MAX) return;  // freed

  // Resolve plan/mapping and epoch from the appropriate cold pool.
  IndexPlanRef plan_ref;
  BitTargetMapping mapping;
  uint32_t* epoch_ptr = nullptr;
  uint32_t target_process_id = 0;

  if (target_handle.kind == SubKind::kEdge) {
    auto& esub =
        signal_subs_[target_handle.slot_id].edge_subs[target_handle.index];
    if (esub.cold_idx == UINT32_MAX) return;
    auto& ecold = edge_cold_pool_[esub.cold_idx];
    plan_ref = ecold.plan_ref;
    mapping = ecold.rebind_mapping;
    epoch_ptr = &ecold.last_rebind_epoch;
    target_process_id = esub.process_id;
  } else {
    auto& csub =
        signal_subs_[target_handle.slot_id].container_subs[target_handle.index];
    auto& ccold = container_cold_pool_[csub.cold_idx];
    plan_ref = ccold.plan_ref;
    mapping = ccold.rebind_mapping;
    epoch_ptr = &ccold.last_rebind_epoch;
    target_process_id = csub.process_id;
  }

  // Epoch guard: skip if already rebound this flush cycle.
  if (*epoch_ptr == flush_epoch_) return;
  *epoch_ptr = flush_epoch_;

  // Resolve plan from the process's pool.
  if (target_process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::RebindSubscription",
        std::format(
            "process_id {} exceeds num_processes {}", target_process_id,
            num_processes_));
  }
  auto& proc_state = process_states_[target_process_id];
  auto all_ops = std::span<const IndexPlanOp>(proc_state.plan_pool.ops);
  auto plan_span = all_ops.subspan(plan_ref.start, plan_ref.count);

  bool should_deactivate = false;
  int64_t index_val = EvaluateIndexPlan(
      design_state_base_, slot_meta_registry_, plan_span, &should_deactivate);

  // Helper lambdas for active/inactive flag setting.
  auto set_inactive = [&]() {
    if (target_handle.kind == SubKind::kEdge) {
      signal_subs_[target_handle.slot_id]
          .edge_subs[target_handle.index]
          .flags &= ~kSubActive;
    } else {
      signal_subs_[target_handle.slot_id]
          .container_subs[target_handle.index]
          .flags &= ~kSubActive;
    }
  };

  if (should_deactivate) {
    set_inactive();
    return;
  }

  // Container rebind path: recompute which element is observed.
  // After index plan evaluation, we update container_sv_index to the new
  // element index, chase the heap handle to validate bounds, and re-seed
  // last_bit from the new element's LSB. This is sufficient because container
  // edge triggers observe only bit 0 of byte 0 of the element (see
  // ContainerSub observation model). The next FlushContainerSub will compare
  // the reseeded last_bit against the current element value.
  if (target_handle.kind == SubKind::kContainer) {
    auto& csub =
        signal_subs_[target_handle.slot_id].container_subs[target_handle.index];
    auto& ccold = container_cold_pool_[csub.cold_idx];

    auto ds = std::span(
        static_cast<const uint8_t*>(design_state_base_),
        ccold.container_base_off + sizeof(void*));
    void* handle_ptr = nullptr;
    std::memcpy(&handle_ptr, &ds[ccold.container_base_off], sizeof(void*));

    if (handle_ptr == nullptr) {
      set_inactive();
      return;
    }
    const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
    if (arr->magic != DynArrayData::kMagic) {
      throw common::InternalError(
          "Engine::RebindSubscription", "invalid container magic");
    }
    ccold.container_sv_index = index_val;
    ccold.container_epoch = arr->epoch;
    if (arr->data == nullptr || index_val < 0 || index_val >= arr->size) {
      set_inactive();
      return;
    }
    csub.flags |= kSubActive;
    // Capture initial bit for edge detection.
    auto byte_off =
        static_cast<uint32_t>(index_val) * ccold.container_elem_stride;
    auto heap_data =
        std::span(static_cast<const uint8_t*>(arr->data), byte_off + 1);
    csub.last_bit = heap_data[byte_off] & 1;
    return;
  }

  // Edge path: packed/unpacked DesignState bit position update.
  int64_t logical_offset =
      static_cast<int64_t>(index_val - mapping.index_base) * mapping.index_step;

  if (logical_offset < 0 ||
      static_cast<uint64_t>(logical_offset) >= mapping.total_bits) {
    set_inactive();
    return;
  }

  auto& esub =
      signal_subs_[target_handle.slot_id].edge_subs[target_handle.index];
  esub.flags |= kSubActive;
  auto bit = static_cast<uint64_t>(logical_offset);
  auto new_byte_offset = static_cast<uint32_t>(bit / 8);
  auto new_bit_index = static_cast<uint8_t>(bit % 8);

  // Update the target's observation position.
  auto& ecold = edge_cold_pool_[esub.cold_idx];
  const auto& meta = slot_meta_registry_.Get(target_handle.slot_id);
  auto ds = std::span(
      static_cast<const uint8_t*>(design_state_base_),
      meta.base_off + new_byte_offset + 1);

  // Same-byte rebinding: extract old bit from the saved byte snapshot
  // so the edge pass detects the 0->1 / 1->0 transition correctly.
  // Different-byte: read from current design state (no prior observation).
  if (new_byte_offset == esub.byte_offset && ecold.has_edge_last_byte) {
    esub.last_bit = (ecold.edge_last_byte >> new_bit_index) & 1;
  } else {
    esub.last_bit = (ds[meta.base_off + new_byte_offset] >> new_bit_index) & 1;
  }
  // Save full byte snapshot for potential future same-byte rebinding.
  ecold.edge_last_byte = ds[meta.base_off + new_byte_offset];
  ecold.has_edge_last_byte = true;
  esub.byte_offset = new_byte_offset;
  esub.bit_index = new_bit_index;
}

void Engine::FlushContainerSub(
    uint32_t slot_id, ContainerSub& sub,
    std::span<const uint8_t> design_state) {
  auto& cold = container_cold_pool_[sub.cold_idx];

  // Chase handle from DesignState.
  void* handle_ptr = nullptr;
  std::memcpy(
      &handle_ptr, &design_state[cold.container_base_off], sizeof(void*));

  if (handle_ptr == nullptr) {
    sub.flags &= ~kSubActive;
    return;
  }

  const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
  if (arr->magic != DynArrayData::kMagic) {
    throw common::InternalError(
        "Engine::FlushContainerSub", "invalid container magic");
  }

  int64_t idx = cold.container_sv_index;

  if (arr->data == nullptr || idx < 0 || idx >= arr->size) {
    sub.flags &= ~kSubActive;
    cold.container_epoch = arr->epoch;
    return;
  }

  // Element is in range.
  if (!(sub.flags & kSubActive)) {
    // Was inactive, now in-range: reactivate, capture snapshot, no edge.
    sub.flags |= kSubActive;
    cold.container_epoch = arr->epoch;
    auto byte_off = static_cast<uint32_t>(idx) * cold.container_elem_stride;
    auto heap_data =
        std::span(static_cast<const uint8_t*>(arr->data), byte_off + 1);
    sub.last_bit = heap_data[byte_off] & 1;
    return;
  }

  cold.container_epoch = arr->epoch;

  auto byte_off = static_cast<uint32_t>(idx) * cold.container_elem_stride;
  auto heap_data = std::span(
      static_cast<const uint8_t*>(arr->data),
      byte_off + cold.container_elem_stride);
  uint8_t current_bit = heap_data[byte_off] & 1;

  bool should_wake =
      EvaluateEdge(sub.edge, sub.last_bit != 0, current_bit != 0);
  sub.last_bit = current_bit;

  if (should_wake) {
    EnqueueProcessWakeup(
        sub.process_id, sub.instance_id, sub.resume_block, slot_id,
        WakeCause::kContainer);
  }
}

void Engine::EnqueueProcessWakeupCold(
    uint32_t process_id, uint32_t instance_id, uint32_t resume_block,
    uint32_t trigger_slot, WakeCause cause) {
  if (detailed_stats_enabled_) ++stats_.detailed.wakeup_attempts;
  ScheduledEvent event{
      .handle = ProcessHandle{process_id, instance_id},
      .resume = ResumePoint{resume_block, 0},
      .cause = cause,
      .trigger_slot = trigger_slot,
  };
  next_delta_queue_.push_back(event);
  process_states_[process_id].is_enqueued = true;
}

void Engine::FlushSlotRebindSubs(
    std::vector<RebindWatcherSub>& subs, const SlotMeta& meta,
    std::span<const uint8_t> design_state) {
  if (subs.empty()) return;

  // Invariant: rebind watchers are always active once installed (never
  // deactivated) and always have valid cold storage (cold_idx != UINT32_MAX).
  // No kSubActive check or cold_idx guard needed.
  for (auto& sub : subs) {
    auto& wcold = watcher_cold_pool_[sub.cold_idx];
    const auto* current = &design_state[meta.base_off + sub.byte_offset];
    auto* snapshot = wcold.snapshot.data();
    if (std::memcmp(current, snapshot, sub.byte_size) != 0) {
      std::memcpy(snapshot, current, sub.byte_size);
      RebindSubscription(wcold.edge_target_id);
    }
  }
}

void Engine::FlushSlotEdgeSubs(
    uint32_t slot_id, std::vector<EdgeSub>& subs, const SlotMeta& meta,
    const common::RangeSet& dirty_ranges, RangeFilterMode mode,
    std::span<const uint8_t> design_state) {
  if (subs.empty()) return;
  const bool detailed = detailed_stats_enabled_;

  for (auto& sub : subs) {
    if (!(sub.flags & kSubActive)) continue;
    if (mode == RangeFilterMode::kPartial &&
        !dirty_ranges.Overlaps(sub.byte_offset, sub.byte_size))
      continue;

    if (detailed) ++stats_.detailed.edge_sub_checks;
    uint8_t current_byte = design_state[meta.base_off + sub.byte_offset];
    uint8_t current_bit = (current_byte >> sub.bit_index) & 1;

    bool should_wake = false;
    if (sub.last_bit != current_bit) {
      should_wake = EvaluateEdge(sub.edge, sub.last_bit != 0, current_bit != 0);
      sub.last_bit = current_bit;
    }
    // Update byte snapshot for potential same-byte rebinding.
    if (sub.cold_idx != UINT32_MAX) {
      auto& ecold = edge_cold_pool_[sub.cold_idx];
      ecold.edge_last_byte = current_byte;
      ecold.has_edge_last_byte = true;
    }

    if (should_wake) {
      if (detailed) ++stats_.detailed.edge_sub_wakeups;
      EnqueueProcessWakeup(
          sub.process_id, sub.instance_id, sub.resume_block, slot_id,
          WakeCause::kEdge);
    }
  }
}

void Engine::FlushSlotChangeSubs(
    uint32_t slot_id, std::vector<ChangeSub>& subs, const SlotMeta& meta,
    const common::RangeSet& dirty_ranges, RangeFilterMode mode,
    std::span<const uint8_t> design_state) {
  if (subs.empty()) return;
  const bool detailed = detailed_stats_enabled_;

  for (auto& sub : subs) {
    if (!(sub.flags & kSubActive)) continue;
    if (mode == RangeFilterMode::kPartial &&
        !dirty_ranges.Overlaps(sub.byte_offset, sub.byte_size))
      continue;

    if (detailed) ++stats_.detailed.change_sub_checks;
    const auto* current = &design_state[meta.base_off + sub.byte_offset];
    uint8_t* snapshot = nullptr;
    if (sub.byte_size <= ChangeSub::kInlineSnapshotCap) {
      snapshot = sub.snapshot_inline.data();
    } else if (sub.cold_idx != UINT32_MAX) {
      snapshot = change_cold_pool_[sub.cold_idx].snapshot.data();
    }
    if (snapshot == nullptr) continue;

    if (std::memcmp(current, snapshot, sub.byte_size) != 0) {
      if (detailed) ++stats_.detailed.change_sub_wakeups;
      std::memcpy(snapshot, current, sub.byte_size);
      EnqueueProcessWakeup(
          sub.process_id, sub.instance_id, sub.resume_block, slot_id,
          WakeCause::kChange);
    }
  }
}

void Engine::FlushSlotContainerSubs(
    uint32_t slot_id, std::vector<ContainerSub>& subs,
    std::span<const uint8_t> design_state) {
  if (subs.empty()) return;

  for (auto& sub : subs) {
    FlushContainerSub(slot_id, sub, design_state);
  }
}

void Engine::FlushDirtySlot(
    uint32_t slot_id, SlotSubscriptions& slot, const SlotMeta& meta,
    std::span<const uint8_t> design_state) {
  // Phase ordering contract:
  //   1. Rebind  -- must precede edge (rebind updates byte_offset/bit_index)
  //   2. Edge    -- range-filtered, wakes on bit transitions
  //   3. Change  -- range-filtered, wakes on byte-level value change
  //   4. Container -- independent, unfiltered (heap-chasing comparison)
  FlushSlotRebindSubs(slot.rebind_subs, meta, design_state);

  if (!slot.edge_subs.empty() || !slot.change_subs.empty()) {
    const auto& dirty_ranges = update_set_.DeltaRangesFor(slot_id);
    if (dirty_ranges.IsFullExtent()) {
      // Fast path: full-slot dirty (most common on clocked designs).
      // Skip range filtering entirely -- all subs are in range.
      FlushSlotEdgeSubs(
          slot_id, slot.edge_subs, meta, dirty_ranges, RangeFilterMode::kFull,
          design_state);
      FlushSlotChangeSubs(
          slot_id, slot.change_subs, meta, dirty_ranges, RangeFilterMode::kFull,
          design_state);
    } else if (!dirty_ranges.IsEmpty()) {
      // Slow path: partial dirty ranges -- per-sub range filtering.
      FlushSlotEdgeSubs(
          slot_id, slot.edge_subs, meta, dirty_ranges,
          RangeFilterMode::kPartial, design_state);
      FlushSlotChangeSubs(
          slot_id, slot.change_subs, meta, dirty_ranges,
          RangeFilterMode::kPartial, design_state);
    }
  }

  FlushSlotContainerSubs(slot_id, slot.container_subs, design_state);
}

void Engine::FlushSignalUpdates() {
  if (finished_) {
    return;
  }

  // Guard: MIR path has no slot meta / design state.
  if (!slot_meta_registry_.IsPopulated() || design_state_base_ == nullptr) {
    return;
  }

  auto newly_dirty = update_set_.DeltaDirtySlots();
  if (newly_dirty.empty()) {
    return;
  }

  // Increment flush epoch for rebind deduplication.
  ++flush_epoch_;

  const bool detailed = detailed_stats_enabled_;
  std::span design_state(
      static_cast<const uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());

  for (uint32_t slot_id : newly_dirty) {
    if (detailed) ++stats_.detailed.flush_dirty_slots;
    auto& slot = signal_subs_[slot_id];
    const auto& meta = slot_meta_registry_.Get(slot_id);
    FlushDirtySlot(slot_id, slot, meta, design_state);
  }
}

}  // namespace lyra::runtime
