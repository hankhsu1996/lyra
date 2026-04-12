#include <cstdint>
#include <cstring>
#include <format>
#include <span>
#include <utility>
#include <vector>

#include "lyra/common/bit_target_mapping.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/dyn_array_data.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/index_plan.hpp"
#include "lyra/runtime/instance_observability.hpp"
#include "lyra/runtime/runtime_instance.hpp"
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
  if (IsGlobalEdgeTargetId(edge_target_id)) {
    RebindGlobalSubscription(EdgeTargetIndex(edge_target_id));
  } else {
    RebindLocalSubscription(EdgeTargetIndex(edge_target_id));
  }
}

// Update positional fields of a moved edge target handle after swap-and-pop.
inline void UpdateMovedEdgeTargetPosition(
    std::vector<LocalEdgeTargetHandle>& local_table,
    std::vector<GlobalEdgeTargetHandle>& global_table, uint32_t edge_target_id,
    uint32_t index, uint32_t edge_group, EdgeBucket bucket) {
  if (IsGlobalEdgeTargetId(edge_target_id)) {
    auto& h = global_table[EdgeTargetIndex(edge_target_id)];
    h.index = index;
    h.edge_group = edge_group;
    h.edge_bucket = bucket;
  } else {
    auto& h = local_table[EdgeTargetIndex(edge_target_id)];
    h.index = index;
    h.edge_group = edge_group;
    h.edge_bucket = bucket;
  }
}

// Shared edge group migration logic. Extracts the sub from its old group,
// finds or creates the new group, inserts the sub, and updates all
// bookkeeping (process sub refs, cold snapshots, target handle position).
// BackpatchFn: (uint32_t process_id, uint32_t process_sub_idx,
//               uint32_t new_idx, uint32_t new_group) -> void
template <typename HandleT, typename BackpatchFn>
void RebindEdgeGroupMigration(
    HandleT& target, SlotSubscriptions& target_subs,
    std::vector<EdgeTargetCold>& edge_cold_pool,
    std::vector<LocalEdgeTargetHandle>& local_target_table,
    std::vector<GlobalEdgeTargetHandle>& global_target_table,
    std::span<const uint8_t> slot_storage, uint32_t new_byte_offset,
    uint8_t new_bit_index, BackpatchFn backpatch) {
  auto& old_group = target_subs.edge_groups[target.edge_group];
  auto& old_vec = (target.edge_bucket == EdgeBucket::kPosedge)
                      ? old_group.posedge_subs
                      : old_group.negedge_subs;
  auto& esub = old_vec[target.index];

  auto& ecold = edge_cold_pool[esub.cold_idx];
  uint8_t current_byte = slot_storage[new_byte_offset];

  bool same_group =
      (new_byte_offset == old_group.byte_offset &&
       new_bit_index == old_group.bit_index);

  if (same_group) {
    esub.flags |= kSubActive;
    ecold.edge_last_byte = current_byte;
    ecold.has_edge_last_byte = true;
    return;
  }

  EdgeSub sub_copy = esub;
  sub_copy.flags |= kSubActive;

  // Remove from old bucket.
  {
    auto& rm_vec = (target.edge_bucket == EdgeBucket::kPosedge)
                       ? old_group.posedge_subs
                       : old_group.negedge_subs;
    uint32_t rm_last = static_cast<uint32_t>(rm_vec.size()) - 1;
    if (target.index != rm_last) {
      rm_vec[target.index] = rm_vec[rm_last];
      auto& moved = rm_vec[target.index];
      backpatch(
          moved.process_id, moved.process_sub_idx, target.index,
          target.edge_group, target.edge_bucket);
      if (moved.cold_idx != UINT32_MAX) {
        auto& moved_cold = edge_cold_pool[moved.cold_idx];
        if (moved_cold.edge_target_id != UINT32_MAX) {
          UpdateMovedEdgeTargetPosition(
              local_target_table, global_target_table,
              moved_cold.edge_target_id, target.index, target.edge_group,
              target.edge_bucket);
        }
      }
    }
    rm_vec.pop_back();
  }

  // Find or create the target group.
  uint8_t pre_change_bit = 0;
  if (new_byte_offset == old_group.byte_offset && ecold.has_edge_last_byte) {
    pre_change_bit = (ecold.edge_last_byte >> new_bit_index) & 1;
  } else {
    pre_change_bit = (current_byte >> new_bit_index) & 1;
  }
  uint8_t new_group_idx = 0;
  {
    auto& groups = target_subs.edge_groups;
    bool found = false;
    for (uint32_t gi = 0; gi < groups.size(); ++gi) {
      if (groups[gi].byte_offset == new_byte_offset &&
          groups[gi].bit_index == new_bit_index) {
        if (groups[gi].posedge_subs.empty() &&
            groups[gi].negedge_subs.empty()) {
          groups[gi].last_bit = pre_change_bit;
        }
        new_group_idx = static_cast<uint8_t>(gi);
        found = true;
        break;
      }
    }
    if (!found) {
      for (uint32_t gi = 0; gi < groups.size(); ++gi) {
        if (groups[gi].posedge_subs.empty() &&
            groups[gi].negedge_subs.empty()) {
          groups[gi].byte_offset = new_byte_offset;
          groups[gi].bit_index = new_bit_index;
          groups[gi].last_bit = pre_change_bit;
          new_group_idx = static_cast<uint8_t>(gi);
          found = true;
          break;
        }
      }
      if (!found) {
        new_group_idx = static_cast<uint8_t>(groups.size());
        groups.push_back(
            EdgeWatchGroup{
                .byte_offset = new_byte_offset,
                .bit_index = new_bit_index,
                .last_bit = pre_change_bit,
                .posedge_subs = {},
                .negedge_subs = {}});
      }
    }
  }

  // Insert into target group's matching polarity bucket.
  auto& new_group = target_subs.edge_groups[new_group_idx];
  auto& new_vec = (target.edge_bucket == EdgeBucket::kPosedge)
                      ? new_group.posedge_subs
                      : new_group.negedge_subs;
  auto new_index = static_cast<uint32_t>(new_vec.size());
  new_vec.push_back(sub_copy);

  // Update sub ref.
  backpatch(
      sub_copy.process_id, sub_copy.process_sub_idx, new_index, new_group_idx,
      target.edge_bucket);

  // Update target handle position.
  target.edge_group = new_group_idx;
  target.index = new_index;

  // Update cold snapshot.
  ecold.edge_last_byte = current_byte;
  ecold.has_edge_last_byte = true;
}

void Engine::RebindLocalSubscription(uint32_t idx) {
  if (idx >= local_edge_target_table_.size()) {
    throw common::InternalError(
        "Engine::RebindLocalSubscription",
        std::format(
            "local edge target idx {} >= table size {}", idx,
            local_edge_target_table_.size()));
  }
  auto& target = local_edge_target_table_[idx];
  if (target.instance == nullptr) return;

  auto& target_subs =
      target.instance->observability.local_signal_subs[target.signal.value];

  IndexPlanRef plan_ref;
  BitTargetMapping mapping;
  uint32_t* epoch_ptr = nullptr;
  uint32_t target_process_id = 0;

  if (target.kind == SubKind::kEdge) {
    auto& g = target_subs.edge_groups[target.edge_group];
    auto& vec = (target.edge_bucket == EdgeBucket::kPosedge) ? g.posedge_subs
                                                             : g.negedge_subs;
    auto& esub = vec[target.index];
    if (esub.cold_idx == UINT32_MAX) return;
    auto& ecold = edge_cold_pool_[esub.cold_idx];
    plan_ref = ecold.plan_ref;
    mapping = ecold.rebind_mapping;
    epoch_ptr = &ecold.last_rebind_epoch;
    target_process_id = esub.process_id;
  } else {
    auto& csub = target_subs.container_subs[target.index];
    auto& ccold = container_cold_pool_[csub.cold_idx];
    plan_ref = ccold.plan_ref;
    mapping = ccold.rebind_mapping;
    epoch_ptr = &ccold.last_rebind_epoch;
    target_process_id = csub.process_id;
  }

  if (*epoch_ptr == flush_epoch_) return;
  *epoch_ptr = flush_epoch_;

  if (target_process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::RebindLocalSubscription",
        std::format(
            "process_id {} exceeds num_processes {}", target_process_id,
            num_processes_));
  }
  auto& proc_state = process_states_[target_process_id];
  auto all_ops = std::span<const IndexPlanOp>(proc_state.plan_pool.ops);
  auto plan_span = all_ops.subspan(plan_ref.start, plan_ref.count);

  bool should_deactivate = false;
  int64_t index_val = EvaluateIndexPlan(
      design_state_base_, const_instances_, slot_meta_registry_,
      target.instance, plan_span, &should_deactivate);

  auto set_inactive = [&]() {
    if (target.kind == SubKind::kEdge) {
      auto& g = target_subs.edge_groups[target.edge_group];
      auto& vec = (target.edge_bucket == EdgeBucket::kPosedge) ? g.posedge_subs
                                                               : g.negedge_subs;
      vec[target.index].flags &= ~kSubActive;
    } else {
      target_subs.container_subs[target.index].flags &= ~kSubActive;
    }
  };

  if (should_deactivate) {
    set_inactive();
    return;
  }

  // Container rebind path.
  if (target.kind == SubKind::kContainer) {
    auto& csub = target_subs.container_subs[target.index];
    auto& ccold = container_cold_pool_[csub.cold_idx];

    const uint8_t* cbase = ResolveInstanceSlotBase(
        *target.instance, LocalSignalId{ccold.container_signal_id});
    void* handle_ptr = nullptr;
    std::memcpy(&handle_ptr, cbase, sizeof(void*));

    if (handle_ptr == nullptr) {
      set_inactive();
      return;
    }
    const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
    if (arr->magic != DynArrayData::kMagic) {
      throw common::InternalError(
          "Engine::RebindLocalSubscription", "invalid container magic");
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

  // Edge rebind path.
  int64_t logical_offset =
      static_cast<int64_t>(index_val - mapping.index_base) * mapping.index_step;

  if (logical_offset < 0 ||
      std::cmp_greater_equal(logical_offset, mapping.total_bits)) {
    set_inactive();
    return;
  }

  auto bit = static_cast<uint64_t>(logical_offset);
  auto new_byte_offset = static_cast<uint32_t>(bit / 8);
  auto new_bit_index = static_cast<uint8_t>(bit % 8);

  // Resolve storage directly from instance.
  auto& inst = *target.instance;
  const auto& imeta = inst.observability.layout->slot_meta[target.signal.value];
  auto slot_storage = std::span(
      ResolveInstanceSlotBase(inst, target.signal), imeta.total_bytes);

  RebindEdgeGroupMigration(
      target, target_subs, edge_cold_pool_, local_edge_target_table_,
      global_edge_target_table_, slot_storage, new_byte_offset, new_bit_index,
      [this](
          uint32_t pid, uint32_t psi, uint32_t new_idx, uint32_t grp,
          EdgeBucket bkt) {
        auto& mr = process_states_[pid].local_sub_refs[psi];
        mr.index = new_idx;
        mr.edge_group = grp;
        mr.edge_bucket = bkt;
      });
}

void Engine::RebindGlobalSubscription(uint32_t idx) {
  if (idx >= global_edge_target_table_.size()) {
    throw common::InternalError(
        "Engine::RebindGlobalSubscription",
        std::format(
            "global edge target idx {} >= table size {}", idx,
            global_edge_target_table_.size()));
  }
  auto& target = global_edge_target_table_[idx];
  if (target.signal.value == UINT32_MAX) return;

  auto& target_subs = signal_subs_[target.signal.value];

  IndexPlanRef plan_ref;
  BitTargetMapping mapping;
  uint32_t* epoch_ptr = nullptr;
  uint32_t target_process_id = 0;

  if (target.kind == SubKind::kEdge) {
    auto& g = target_subs.edge_groups[target.edge_group];
    auto& vec = (target.edge_bucket == EdgeBucket::kPosedge) ? g.posedge_subs
                                                             : g.negedge_subs;
    auto& esub = vec[target.index];
    if (esub.cold_idx == UINT32_MAX) return;
    auto& ecold = edge_cold_pool_[esub.cold_idx];
    plan_ref = ecold.plan_ref;
    mapping = ecold.rebind_mapping;
    epoch_ptr = &ecold.last_rebind_epoch;
    target_process_id = esub.process_id;
  } else {
    auto& csub = target_subs.container_subs[target.index];
    auto& ccold = container_cold_pool_[csub.cold_idx];
    plan_ref = ccold.plan_ref;
    mapping = ccold.rebind_mapping;
    epoch_ptr = &ccold.last_rebind_epoch;
    target_process_id = csub.process_id;
  }

  if (*epoch_ptr == flush_epoch_) return;
  *epoch_ptr = flush_epoch_;

  if (target_process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::RebindGlobalSubscription",
        std::format(
            "process_id {} exceeds num_processes {}", target_process_id,
            num_processes_));
  }
  auto& proc_state = process_states_[target_process_id];
  auto all_ops = std::span<const IndexPlanOp>(proc_state.plan_pool.ops);
  auto plan_span = all_ops.subspan(plan_ref.start, plan_ref.count);

  bool should_deactivate = false;
  int64_t index_val = EvaluateIndexPlan(
      design_state_base_, const_instances_, slot_meta_registry_, nullptr,
      plan_span, &should_deactivate);

  auto set_inactive = [&]() {
    if (target.kind == SubKind::kEdge) {
      auto& g = target_subs.edge_groups[target.edge_group];
      auto& vec = (target.edge_bucket == EdgeBucket::kPosedge) ? g.posedge_subs
                                                               : g.negedge_subs;
      vec[target.index].flags &= ~kSubActive;
    } else {
      target_subs.container_subs[target.index].flags &= ~kSubActive;
    }
  };

  if (should_deactivate) {
    set_inactive();
    return;
  }

  // Container rebind path.
  if (target.kind == SubKind::kContainer) {
    auto& csub = target_subs.container_subs[target.index];
    auto& ccold = container_cold_pool_[csub.cold_idx];

    const auto& cmeta = slot_meta_registry_.Get(ccold.container_signal_id);
    const uint8_t* cbase =
        ResolveSlotBase(cmeta, design_state_base_, instances_);
    void* handle_ptr = nullptr;
    std::memcpy(&handle_ptr, cbase, sizeof(void*));

    if (handle_ptr == nullptr) {
      set_inactive();
      return;
    }
    const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
    if (arr->magic != DynArrayData::kMagic) {
      throw common::InternalError(
          "Engine::RebindGlobalSubscription", "invalid container magic");
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

  // Edge rebind path.
  int64_t logical_offset =
      static_cast<int64_t>(index_val - mapping.index_base) * mapping.index_step;

  if (logical_offset < 0 ||
      std::cmp_greater_equal(logical_offset, mapping.total_bits)) {
    set_inactive();
    return;
  }

  auto bit = static_cast<uint64_t>(logical_offset);
  auto new_byte_offset = static_cast<uint32_t>(bit / 8);
  auto new_bit_index = static_cast<uint8_t>(bit % 8);

  // Resolve storage via global slot metadata.
  const auto& meta = slot_meta_registry_.Get(target.signal.value);
  auto slot_storage = std::span(
      ResolveSlotBase(meta, design_state_base_, const_instances_),
      meta.total_bytes);

  RebindEdgeGroupMigration(
      target, target_subs, edge_cold_pool_, local_edge_target_table_,
      global_edge_target_table_, slot_storage, new_byte_offset, new_bit_index,
      [this](
          uint32_t pid, uint32_t psi, uint32_t new_idx, uint32_t grp,
          EdgeBucket bkt) {
        auto& mr = process_states_[pid].global_sub_refs[psi];
        mr.index = new_idx;
        mr.edge_group = grp;
        mr.edge_bucket = bkt;
      });
}

void Engine::FlushContainerSub(
    uint32_t slot_id, ContainerSub& sub,
    std::span<const uint8_t> slot_storage) {
  auto& cold = container_cold_pool_[sub.cold_idx];

  // Chase handle from slot storage.
  void* handle_ptr = nullptr;
  std::memcpy(&handle_ptr, slot_storage.data(), sizeof(void*));

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
        sub.process_id, sub.instance_id.value, sub.resume_block, slot_id,
        WakeCause::kContainer);
  }
}

void Engine::FlushSlotRebindSubs(
    std::vector<RebindWatcherSub>& subs,
    std::span<const uint8_t> slot_storage) {
  if (subs.empty()) return;

  for (auto& sub : subs) {
    auto& wcold = watcher_cold_pool_[sub.cold_idx];
    const auto* current = &slot_storage[sub.byte_offset];
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
    uint32_t slot_id, std::vector<EdgeWatchGroup>& groups,
    std::span<const uint8_t> slot_storage, const common::RangeSet& dirty_ranges,
    RangeFilterMode mode) {
  const bool detailed = detailed_stats_enabled_;
  for (auto& group : groups) {
    if (mode == RangeFilterMode::kPartial &&
        !dirty_ranges.Overlaps(group.byte_offset, 1)) {
      continue;
    }

    uint8_t current_byte = slot_storage[group.byte_offset];
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
          sub.process_id, sub.instance_id.value, sub.resume_block, slot_id,
          WakeCause::kEdge);
    }

    group.last_bit = current_bit;
  }
}

void Engine::FlushSlotChangeSubs(
    uint32_t slot_id, std::vector<ChangeSub>& subs,
    std::span<const uint8_t> slot_storage, const common::RangeSet& dirty_ranges,
    RangeFilterMode mode) {
  if (subs.empty()) return;
  const bool detailed = detailed_stats_enabled_;
  for (auto& sub : subs) {
    if ((sub.flags & kSubActive) == 0) continue;
    if (mode == RangeFilterMode::kPartial &&
        !dirty_ranges.Overlaps(sub.byte_offset, sub.byte_size))
      continue;

    if (detailed) ++stats_.detailed.change_sub_checks;
    const auto* current = &slot_storage[sub.byte_offset];
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
          sub.process_id, sub.instance_id.value, sub.resume_block, slot_id,
          WakeCause::kChange);
    }
  }
}

void Engine::FlushSlotContainerSubs(
    uint32_t slot_id, std::vector<ContainerSub>& subs,
    std::span<const uint8_t> slot_storage) {
  if (subs.empty()) return;

  for (auto& sub : subs) {
    FlushContainerSub(slot_id, sub, slot_storage);
  }
}

// Flush edge, change, and container subscriptions for one slot.
// Rebind subscriptions are handled in the rebind phase and
// must not be repeated here.
void Engine::FlushDirtySlotPostRebind(
    uint32_t slot_id, SlotSubscriptions& slot,
    std::span<const uint8_t> slot_storage) {
  if (!slot.edge_groups.empty() || !slot.change_subs.empty()) {
    const auto& dirty_ranges = update_set_.DeltaRangesFor(slot_id);
    if (dirty_ranges.IsFullExtent()) {
      FlushSlotEdgeGroups(
          slot_id, slot.edge_groups, slot_storage, dirty_ranges,
          RangeFilterMode::kFull);
      FlushSlotChangeSubs(
          slot_id, slot.change_subs, slot_storage, dirty_ranges,
          RangeFilterMode::kFull);
    } else if (!dirty_ranges.IsEmpty()) {
      FlushSlotEdgeGroups(
          slot_id, slot.edge_groups, slot_storage, dirty_ranges,
          RangeFilterMode::kPartial);
      FlushSlotChangeSubs(
          slot_id, slot.change_subs, slot_storage, dirty_ranges,
          RangeFilterMode::kPartial);
    }
  }

  FlushSlotContainerSubs(slot_id, slot.container_subs, slot_storage);
}

void Engine::FlushSignalUpdates() {
  if (finished_) return;

  // Advance flush epoch for rebind epoch guard. Must cover both global
  // and local flush cycles.
  ++flush_epoch_;

  // Dispatch global subscriptions from update_set_, then local
  // subscriptions from per-instance local_updates.
  FlushGlobalSignalUpdates();
  for (uint32_t idx : delta_dirty_instances_) {
    auto* inst = instances_[idx];
    auto& obs = inst->observability;
    if (obs.local_signal_count > 0 &&
        !obs.local_updates.DeltaDirtySignals().empty()) {
      ++obs.local_flush_epoch;
    }
    FlushLocalSignalUpdates(*inst);
  }
}

// R5: Domain-split flush entrypoints.

void Engine::FlushGlobalSignalUpdates() {
  if (!slot_meta_registry_.IsPopulated() || design_state_base_ == nullptr) {
    return;
  }

  auto newly_dirty = update_set_.DeltaDirtySlots();
  if (newly_dirty.empty()) return;

  const bool detailed = detailed_stats_enabled_;

  // Two-phase flush: rebinds globally first, then edge/change/container.
  for (uint32_t slot_id : newly_dirty) {
    auto& slot = signal_subs_[slot_id];
    if (slot.rebind_subs.empty()) continue;
    const auto& meta = slot_meta_registry_.Get(slot_id);
    auto slot_storage = std::span(
        ResolveSlotBase(meta, design_state_base_, const_instances_),
        meta.total_bytes);
    FlushSlotRebindSubs(slot.rebind_subs, slot_storage);
  }

  for (uint32_t slot_id : newly_dirty) {
    auto& slot = signal_subs_[slot_id];
    if (slot.edge_groups.empty() && slot.change_subs.empty() &&
        slot.container_subs.empty())
      continue;
    if (detailed) ++stats_.detailed.flush_dirty_slots;
    const auto& meta = slot_meta_registry_.Get(slot_id);
    auto slot_storage = std::span(
        ResolveSlotBase(meta, design_state_base_, const_instances_),
        meta.total_bytes);
    FlushDirtySlotPostRebind(slot_id, slot, slot_storage);
  }
}

void Engine::FlushLocalSignalUpdates(RuntimeInstance& inst) {
  auto& obs = inst.observability;
  if (obs.local_signal_count == 0) return;
  if (obs.local_updates.DeltaDirtySignals().empty()) return;

  // Dispatch subscriptions for locally dirty instance-owned signals.
  // Two-phase: rebinds first, then edge/change/container.

  // Phase 1: rebind watchers.
  for (LocalSignalId lid : obs.local_updates.DeltaDirtySignals()) {
    auto& slot = obs.local_signal_subs[lid.value];
    if (slot.rebind_subs.empty()) continue;
    const auto& imeta = obs.layout->slot_meta[lid.value];
    auto slot_storage =
        std::span(ResolveInstanceSlotBase(inst, lid), imeta.total_bytes);
    FlushSlotRebindSubs(slot.rebind_subs, slot_storage);
  }

  // Phase 2: edge/change/container via native local flush.
  for (LocalSignalId lid : obs.local_updates.DeltaDirtySignals()) {
    auto& slot = obs.local_signal_subs[lid.value];
    if (slot.edge_groups.empty() && slot.change_subs.empty() &&
        slot.container_subs.empty())
      continue;
    if (detailed_stats_enabled_) ++stats_.detailed.flush_dirty_slots;
    FlushLocalDirtySlotPostRebind(inst, lid, slot);
  }
}

void Engine::FlushLocalDirtySlotPostRebind(
    RuntimeInstance& inst, LocalSignalId signal, SlotSubscriptions& slot) {
  const auto& imeta = inst.observability.layout->slot_meta[signal.value];
  auto slot_storage =
      std::span(ResolveInstanceSlotBase(inst, signal), imeta.total_bytes);
  // Trigger slot for EnqueueProcessWakeup is cosmetic (activation trace
  // only). Use local signal id directly.
  uint32_t trigger_slot = signal.value;

  if (!slot.edge_groups.empty() || !slot.change_subs.empty()) {
    const auto& dirty_ranges =
        inst.observability.local_updates.DeltaRangesFor(signal);
    if (dirty_ranges.IsFullExtent()) {
      FlushSlotEdgeGroups(
          trigger_slot, slot.edge_groups, slot_storage, dirty_ranges,
          RangeFilterMode::kFull);
      FlushSlotChangeSubs(
          trigger_slot, slot.change_subs, slot_storage, dirty_ranges,
          RangeFilterMode::kFull);
    } else if (!dirty_ranges.IsEmpty()) {
      FlushSlotEdgeGroups(
          trigger_slot, slot.edge_groups, slot_storage, dirty_ranges,
          RangeFilterMode::kPartial);
      FlushSlotChangeSubs(
          trigger_slot, slot.change_subs, slot_storage, dirty_ranges,
          RangeFilterMode::kPartial);
    }
  }

  FlushSlotContainerSubs(trigger_slot, slot.container_subs, slot_storage);
}

}  // namespace lyra::runtime
