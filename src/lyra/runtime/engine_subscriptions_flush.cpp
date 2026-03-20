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
    auto& esub = ResolveEdgeSub(
        SubRef{
            .slot_id = target_handle.slot_id,
            .index = target_handle.index,
            .kind = SubKind::kEdge,
            .edge_bucket = target_handle.edge_bucket,
            .edge_group = target_handle.edge_group});
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
      ResolveEdgeSub(
          SubRef{
              .slot_id = target_handle.slot_id,
              .index = target_handle.index,
              .kind = SubKind::kEdge,
              .edge_bucket = target_handle.edge_bucket,
              .edge_group = target_handle.edge_group})
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
    auto byte_off =
        static_cast<uint32_t>(index_val) * ccold.container_elem_stride;
    auto heap_data =
        std::span(static_cast<const uint8_t*>(arr->data), byte_off + 1);
    csub.last_bit = heap_data[byte_off] & 1;
    return;
  }

  // Edge path: packed/unpacked DesignState bit position update.
  // Rebinding is group migration: the sub moves from its current observation-
  // point group to the group for the new (byte_offset, bit_index).
  int64_t logical_offset =
      static_cast<int64_t>(index_val - mapping.index_base) * mapping.index_step;

  if (logical_offset < 0 ||
      static_cast<uint64_t>(logical_offset) >= mapping.total_bits) {
    set_inactive();
    return;
  }

  auto bit = static_cast<uint64_t>(logical_offset);
  auto new_byte_offset = static_cast<uint32_t>(bit / 8);
  auto new_bit_index = static_cast<uint8_t>(bit % 8);

  // Read current byte for cold snapshot and last_bit initialization.
  const auto& meta = slot_meta_registry_.Get(target_handle.slot_id);
  auto ds = std::span(
      static_cast<const uint8_t*>(design_state_base_),
      meta.base_off + new_byte_offset + 1);

  // Resolve old group to get the sub and its cold state.
  auto& old_group =
      GetEdgeGroup(target_handle.slot_id, target_handle.edge_group);
  auto& old_vec = (target_handle.edge_bucket == EdgeBucket::kPosedge)
                      ? old_group.posedge_subs
                      : old_group.negedge_subs;
  auto& esub = old_vec[target_handle.index];

  // Update cold snapshot for same-byte rebind detection.
  auto& ecold = edge_cold_pool_[esub.cold_idx];
  uint8_t current_byte = ds[meta.base_off + new_byte_offset];

  // Check if the new observation point is in the same group.
  bool same_group =
      (new_byte_offset == old_group.byte_offset &&
       new_bit_index == old_group.bit_index);

  if (same_group) {
    // No migration needed. Just update cold state and activate.
    esub.flags |= kSubActive;
    ecold.edge_last_byte = current_byte;
    ecold.has_edge_last_byte = true;
    return;
  }

  // Group migration: remove from old group, insert into new group.
  // Save sub data before removal (swap-and-pop may invalidate the reference).
  EdgeSub sub_copy = esub;
  sub_copy.flags |= kSubActive;

  // Remove from old bucket via centralized helper.
  RemoveEdgeSubFromBucket(
      target_handle.slot_id, target_handle.edge_group,
      target_handle.edge_bucket, target_handle.index);

  // Find or create the target group.
  // For the initial_last_bit of a new group, use the pre-change byte snapshot
  // (from the cold pool) when available. This preserves transition detection:
  // if the bit was 0 before and is now 1, the group starts with last_bit=0
  // so the flush detects the 0->1 posedge.
  uint8_t pre_change_bit = 0;
  if (new_byte_offset == old_group.byte_offset && ecold.has_edge_last_byte) {
    pre_change_bit = (ecold.edge_last_byte >> new_bit_index) & 1;
  } else {
    pre_change_bit = (current_byte >> new_bit_index) & 1;
  }
  uint8_t new_group_idx = FindOrCreateEdgeGroup(
      target_handle.slot_id, new_byte_offset, new_bit_index, pre_change_bit);

  // Insert into target group's matching polarity bucket.
  auto& new_vec = EdgeSubVec(
      target_handle.slot_id, new_group_idx, target_handle.edge_bucket);
  auto new_index = static_cast<uint32_t>(new_vec.size());
  new_vec.push_back(sub_copy);

  // Update SubRef.
  auto& proc = process_states_[sub_copy.process_id];
  auto& ref = proc.sub_refs[sub_copy.process_sub_idx];
  ref.edge_group = new_group_idx;
  ref.index = new_index;

  // Update EdgeTargetHandle.
  target_handle.edge_group = new_group_idx;
  target_handle.index = new_index;

  // Update cold snapshot.
  ecold.edge_last_byte = current_byte;
  ecold.has_edge_last_byte = true;
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
  if ((sub.flags & kSubActive) == 0) {
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

void Engine::FlushSlotRebindSubs(
    std::vector<RebindWatcherSub>& subs, const SlotMeta& meta,
    std::span<const uint8_t> design_state) {
  if (subs.empty()) return;

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

void Engine::UpdateEdgeColdSnapshots(
    EdgeWatchGroup& group, uint8_t current_byte) {
  // Update edge_last_byte for rebind-capable subs only.
  // This is pay-for-play: normal edge dispatch does not carry rebind cost.
  auto update = [&](std::vector<EdgeSub>& subs) {
    for (auto& sub : subs) {
      if (sub.cold_idx != UINT32_MAX) {
        auto& ecold = edge_cold_pool_[sub.cold_idx];
        ecold.edge_last_byte = current_byte;
        ecold.has_edge_last_byte = true;
      }
    }
  };
  update(group.posedge_subs);
  update(group.negedge_subs);
}

void Engine::FlushSlotEdgeGroups(
    uint32_t slot_id, std::vector<EdgeWatchGroup>& groups, const SlotMeta& meta,
    const common::RangeSet& dirty_ranges, RangeFilterMode mode,
    std::span<const uint8_t> design_state) {
  const bool detailed = detailed_stats_enabled_;

  for (auto& group : groups) {
    if (mode == RangeFilterMode::kPartial &&
        !dirty_ranges.Overlaps(group.byte_offset, 1)) {
      continue;
    }

    uint8_t current_byte = design_state[meta.base_off + group.byte_offset];
    uint8_t current_bit = (current_byte >> group.bit_index) & 1;

    // Cold byte snapshot must be refreshed whenever the observed byte is
    // dirty, even if the observed bit did not transition. Another bit in the
    // same byte may have changed, and a later same-byte rebind needs the
    // current byte to seed the new bit position correctly.
    UpdateEdgeColdSnapshots(group, current_byte);

    if (group.last_bit == current_bit) continue;

    // Direction known: dispatch only the matching polarity bucket.
    auto& wake_subs =
        (current_bit != 0) ? group.posedge_subs : group.negedge_subs;

    for (auto& sub : wake_subs) {
      if ((sub.flags & kSubActive) == 0) continue;
      if (detailed) ++stats_.detailed.edge_sub_checks;
      if (detailed) ++stats_.detailed.edge_sub_wakeups;
      EnqueueProcessWakeup(
          sub.process_id, sub.instance_id, sub.resume_block, slot_id,
          WakeCause::kEdge);
    }

    group.last_bit = current_bit;
  }
}

void Engine::FlushSlotChangeSubs(
    uint32_t slot_id, std::vector<ChangeSub>& subs, const SlotMeta& meta,
    const common::RangeSet& dirty_ranges, RangeFilterMode mode,
    std::span<const uint8_t> design_state) {
  if (subs.empty()) return;
  const bool detailed = detailed_stats_enabled_;

  for (auto& sub : subs) {
    if ((sub.flags & kSubActive) == 0) continue;
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

// Flush edge, change, and container subscriptions for one slot.
// Rebind subscriptions are handled in the global rebind phase and
// must not be repeated here.
void Engine::FlushDirtySlotPostRebind(
    uint32_t slot_id, SlotSubscriptions& slot, const SlotMeta& meta,
    std::span<const uint8_t> design_state) {
  if (!slot.edge_groups.empty() || !slot.change_subs.empty()) {
    const auto& dirty_ranges = update_set_.DeltaRangesFor(slot_id);
    if (dirty_ranges.IsFullExtent()) {
      FlushSlotEdgeGroups(
          slot_id, slot.edge_groups, meta, dirty_ranges, RangeFilterMode::kFull,
          design_state);
      FlushSlotChangeSubs(
          slot_id, slot.change_subs, meta, dirty_ranges, RangeFilterMode::kFull,
          design_state);
    } else if (!dirty_ranges.IsEmpty()) {
      FlushSlotEdgeGroups(
          slot_id, slot.edge_groups, meta, dirty_ranges,
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

  // Two-phase flush: rebinds globally first, then edge/change/container.
  //
  // Phase 1: flush all rebind subscriptions across all dirty slots.
  // Rebind watchers update observation points (e.g., which bit of a
  // target slot an edge trigger watches). This must complete globally
  // before any edge evaluation, because a dependency slot (e.g., a
  // dynamic index) and its target slot (e.g., a bus) may both be dirty
  // in the same pass. If the target were flushed before the dependency,
  // the edge trigger would evaluate at the stale observation point.
  //
  // Load-bearing invariant: FlushSlotRebindSubs must only update
  // observation points (cold state, bit targets, snapshot bytes).
  // It must not trigger edge/change/container dispatch or add new
  // dirty slots. If rebind handling ever gains side effects that
  // require dispatch, the phase split must be revisited.
  for (uint32_t slot_id : newly_dirty) {
    auto& slot = signal_subs_[slot_id];
    if (slot.rebind_subs.empty()) continue;
    const auto& meta = slot_meta_registry_.Get(slot_id);
    FlushSlotRebindSubs(slot.rebind_subs, meta, design_state);
  }

  // Phase 2: flush edge, change, and container subscriptions.
  // All rebind observation points are now up-to-date.
  for (uint32_t slot_id : newly_dirty) {
    auto& slot = signal_subs_[slot_id];
    if (slot.edge_groups.empty() && slot.change_subs.empty() &&
        slot.container_subs.empty())
      continue;
    if (detailed) ++stats_.detailed.flush_dirty_slots;
    const auto& meta = slot_meta_registry_.Get(slot_id);
    FlushDirtySlotPostRebind(slot_id, slot, meta, design_state);
  }
}

}  // namespace lyra::runtime
