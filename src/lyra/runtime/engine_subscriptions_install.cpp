#include <algorithm>
#include <cstdint>
#include <cstring>
#include <format>
#include <span>
#include <type_traits>
#include <variant>
#include <vector>

#include "lyra/common/bit_target_mapping.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/dyn_array_data.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/index_plan.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/update_set.hpp"
#include "lyra/runtime/wait_site.hpp"

// Subscription lifecycle: install, removal, refresh, and post-activation
// reconciliation.
//
// Responsibilities:
//   1. Subscription install: Subscribe, SubscribeContainerElement,
//      FindOrCreateEdgeGroup, InstallTriggers, InstallWaitSite,
//      SubscribeRebind
//   2. Subscription removal: Remove{Local,Global}{Edge,Change,RebindWatcher,
//      Container}Sub, ClearInstalledSubscriptions (swap-and-pop with
//      domain-split backpatch)
//   3. Post-activation reconciliation: ReconcilePostActivation,
//      RefreshInstalledSnapshots (routes by SuspendTag, refreshes cold
//      snapshots for persistent waits)
//
// Ownership boundary: group.last_bit is flush-owned (FlushSlotEdgeGroups).
// This file must NOT write group.last_bit except during group creation
// (FindOrCreateEdgeGroup, empty-group init only).

#include "lyra/runtime/runtime_instance.hpp"

namespace lyra::runtime {

namespace {

// Validate cross-instance local identity and return a typed LocalSignalRef.
// Throws InternalError if the target instance doesn't exist or the local
// signal id is out of range.
auto ValidateCrossInstanceLocal(
    const Engine& eng, InstanceId iid, LocalSignalId lid, const char* where)
    -> LocalSignalRef {
  const auto* inst = eng.FindInstance(iid);
  if (inst == nullptr) {
    throw common::InternalError(
        where, std::format(
                   "cross-instance local references missing instance {}", iid));
  }
  if (lid.value >= inst->observability.local_signal_count) {
    throw common::InternalError(
        where, std::format(
                   "cross-instance local_id {} >= local_signal_count {} "
                   "for instance {}",
                   lid.value, inst->observability.local_signal_count, iid));
  }
  return LocalSignalRef{.instance_id = iid, .signal = lid};
}

}  // namespace

auto Engine::ResolveLocalSubSlot(RuntimeInstance& inst, LocalSignalId signal)
    -> SlotSubscriptions& {
  return inst.observability.local_signal_subs[signal.value];
}

auto Engine::ResolveLocalSubSlot(
    const RuntimeInstance& inst, LocalSignalId signal) const
    -> const SlotSubscriptions& {
  return inst.observability.local_signal_subs[signal.value];
}

auto Engine::ResolveGlobalSubSlot(GlobalSignalId signal) -> SlotSubscriptions& {
  return signal_subs_[signal.value];
}

auto Engine::ResolveGlobalSubSlot(GlobalSignalId signal) const
    -> const SlotSubscriptions& {
  return signal_subs_[signal.value];
}

namespace {

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

// Update only the dense index of a moved edge target handle (container subs).
inline void UpdateMovedEdgeTargetIndex(
    std::vector<LocalEdgeTargetHandle>& local_table,
    std::vector<GlobalEdgeTargetHandle>& global_table, uint32_t edge_target_id,
    uint32_t index) {
  if (IsGlobalEdgeTargetId(edge_target_id)) {
    global_table[EdgeTargetIndex(edge_target_id)].index = index;
  } else {
    local_table[EdgeTargetIndex(edge_target_id)].index = index;
  }
}

auto ComputeObserverFlag(const SlotSubscriptions& slot) -> uint8_t {
  uint8_t has = 0;
  if (!slot.edge_groups.empty()) {
    for (const auto& g : slot.edge_groups) {
      if (!g.posedge_subs.empty() || !g.negedge_subs.empty()) {
        has = 1;
        break;
      }
    }
  }
  if (has == 0 && !slot.change_subs.empty()) has = 1;
  if (has == 0 && !slot.container_subs.empty()) has = 1;
  if (has == 0 && !slot.rebind_subs.empty()) has = 1;
  return has;
}

}  // namespace

void Engine::UpdateLocalObserverFlag(
    RuntimeInstance& inst, LocalSignalId signal) {
  auto& slot = ResolveLocalSubSlot(inst, signal);
  inst.observability.local_has_observers[signal.value] =
      ComputeObserverFlag(slot);
}

void Engine::UpdateGlobalObserverFlag(GlobalSignalId signal) {
  auto& slot = ResolveGlobalSubSlot(signal);
  global_has_observers_[signal.value] = ComputeObserverFlag(slot);
}

namespace {

// Backpatch a moved subscription's LocalSubRef after swap-and-pop removal.
void BackpatchMovedLocalSubRef(
    std::vector<ProcessState>& process_states, uint32_t process_id,
    uint32_t process_sub_idx, uint32_t new_index) {
  process_states[process_id].local_sub_refs[process_sub_idx].index = new_index;
}

// Backpatch a moved subscription's GlobalSubRef after swap-and-pop removal.
void BackpatchMovedGlobalSubRef(
    std::vector<ProcessState>& process_states, uint32_t process_id,
    uint32_t process_sub_idx, uint32_t new_index) {
  process_states[process_id].global_sub_refs[process_sub_idx].index = new_index;
}

}  // namespace

auto Engine::GetEdgeGroup(uint32_t slot_id, uint32_t group) -> EdgeWatchGroup& {
  return signal_subs_[slot_id].edge_groups[group];
}

auto Engine::EdgeSubVec(uint32_t slot_id, uint32_t group, EdgeBucket bucket)
    -> std::vector<EdgeSub>& {
  auto& g = signal_subs_[slot_id].edge_groups[group];
  return (bucket == EdgeBucket::kPosedge) ? g.posedge_subs : g.negedge_subs;
}

auto Engine::FindOrCreateEdgeGroup(
    uint32_t slot_id, uint32_t byte_offset, uint8_t bit_index,
    uint8_t initial_last_bit) -> uint32_t {
  // For callers that don't have a slot reference, use flat signal_subs_.
  // This is used by flush paths only (for global slots). Subscribe paths
  // should call FindOrCreateEdgeGroupInSlot instead.
  auto& groups = signal_subs_[slot_id].edge_groups;

  // Search for existing group with matching observation point.
  for (uint32_t i = 0; i < groups.size(); ++i) {
    if (groups[i].byte_offset == byte_offset &&
        groups[i].bit_index == bit_index) {
      // Refresh last_bit when group is empty (all subs removed by prior
      // ClearInstalledSubscriptions). Without this, a stale last_bit from
      // the original install would cause false edge detection on reinstall.
      if (groups[i].posedge_subs.empty() && groups[i].negedge_subs.empty()) {
        groups[i].last_bit = initial_last_bit;
      }
      return i;
    }
  }

  // Reuse an empty group slot if available.
  for (uint32_t i = 0; i < groups.size(); ++i) {
    if (groups[i].posedge_subs.empty() && groups[i].negedge_subs.empty()) {
      groups[i].byte_offset = byte_offset;
      groups[i].bit_index = bit_index;
      groups[i].last_bit = initial_last_bit;
      return i;
    }
  }

  // Append new group.
  auto idx = static_cast<uint32_t>(groups.size());
  groups.push_back(
      EdgeWatchGroup{
          .byte_offset = byte_offset,
          .bit_index = bit_index,
          .last_bit = initial_last_bit,
          .posedge_subs = {},
          .negedge_subs = {}});
  return idx;
}

namespace {

// Shared edge sub removal core: free cold, swap-and-pop, backpatch.
// Caller provides the pre-resolved slot and domain-specific backpatch function.
template <typename BackpatchFn>
void RemoveEdgeSubCore(
    SlotSubscriptions& subs, uint32_t edge_group, EdgeBucket edge_bucket,
    uint32_t index, std::vector<EdgeTargetCold>& edge_cold_pool,
    std::vector<LocalEdgeTargetHandle>& local_target_table,
    std::vector<GlobalEdgeTargetHandle>& global_target_table,
    auto& free_edge_target, auto& free_edge_cold, BackpatchFn backpatch) {
  auto& g = subs.edge_groups[edge_group];
  auto& vec =
      (edge_bucket == EdgeBucket::kPosedge) ? g.posedge_subs : g.negedge_subs;
  uint32_t cold_idx = vec[index].cold_idx;

  if (cold_idx != UINT32_MAX) {
    auto& cold = edge_cold_pool[cold_idx];
    if (cold.edge_target_id != UINT32_MAX) {
      free_edge_target(cold.edge_target_id);
    }
    free_edge_cold(cold_idx);
  }

  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    auto& moved = vec[index];
    backpatch(
        moved.process_id, moved.process_sub_idx, index, edge_group,
        edge_bucket);
    if (moved.cold_idx != UINT32_MAX) {
      auto& moved_cold = edge_cold_pool[moved.cold_idx];
      if (moved_cold.edge_target_id != UINT32_MAX) {
        UpdateMovedEdgeTargetPosition(
            local_target_table, global_target_table, moved_cold.edge_target_id,
            index, edge_group, edge_bucket);
      }
    }
  }
  vec.pop_back();
}

}  // namespace

void Engine::RemoveLocalEdgeSub(const LocalSubRef& ref) {
  auto& subs = ResolveLocalSubSlot(*ref.instance, ref.signal);
  auto free_target = [this](uint32_t id) { FreeEdgeTarget(id); };
  auto free_cold = [this](uint32_t id) { FreeEdgeCold(id); };
  RemoveEdgeSubCore(
      subs, ref.edge_group, ref.edge_bucket, ref.index, edge_cold_pool_,
      local_edge_target_table_, global_edge_target_table_, free_target,
      free_cold,
      [this](
          uint32_t pid, uint32_t psi, uint32_t new_idx, uint32_t grp,
          EdgeBucket bkt) {
        auto& mr = process_states_[pid].local_sub_refs[psi];
        mr.index = new_idx;
        mr.edge_group = grp;
        mr.edge_bucket = bkt;
      });
  UpdateLocalObserverFlag(*ref.instance, ref.signal);
}

void Engine::RemoveGlobalEdgeSub(const GlobalSubRef& ref) {
  auto& subs = ResolveGlobalSubSlot(ref.signal);
  auto free_target = [this](uint32_t id) { FreeEdgeTarget(id); };
  auto free_cold = [this](uint32_t id) { FreeEdgeCold(id); };
  RemoveEdgeSubCore(
      subs, ref.edge_group, ref.edge_bucket, ref.index, edge_cold_pool_,
      local_edge_target_table_, global_edge_target_table_, free_target,
      free_cold,
      [this](
          uint32_t pid, uint32_t psi, uint32_t new_idx, uint32_t grp,
          EdgeBucket bkt) {
        auto& mr = process_states_[pid].global_sub_refs[psi];
        mr.index = new_idx;
        mr.edge_group = grp;
        mr.edge_bucket = bkt;
      });
  UpdateGlobalObserverFlag(ref.signal);
}

void Engine::RemoveLocalChangeSub(const LocalSubRef& ref) {
  auto& vec = ResolveLocalSubSlot(*ref.instance, ref.signal).change_subs;
  auto index = ref.index;
  if (vec[index].cold_idx != UINT32_MAX) {
    FreeChangeCold(vec[index].cold_idx);
  }
  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    BackpatchMovedLocalSubRef(
        process_states_, vec[index].process_id, vec[index].process_sub_idx,
        index);
  }
  vec.pop_back();
  UpdateLocalObserverFlag(*ref.instance, ref.signal);
}

void Engine::RemoveGlobalChangeSub(const GlobalSubRef& ref) {
  auto& vec = ResolveGlobalSubSlot(ref.signal).change_subs;
  auto index = ref.index;
  if (vec[index].cold_idx != UINT32_MAX) {
    FreeChangeCold(vec[index].cold_idx);
  }
  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    BackpatchMovedGlobalSubRef(
        process_states_, vec[index].process_id, vec[index].process_sub_idx,
        index);
  }
  vec.pop_back();
  UpdateGlobalObserverFlag(ref.signal);
}

void Engine::RemoveLocalRebindWatcherSub(const LocalSubRef& ref) {
  auto& vec = ResolveLocalSubSlot(*ref.instance, ref.signal).rebind_subs;
  auto index = ref.index;
  if (vec[index].cold_idx != UINT32_MAX) {
    FreeWatcherCold(vec[index].cold_idx);
  }
  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    BackpatchMovedLocalSubRef(
        process_states_, vec[index].process_id, vec[index].process_sub_idx,
        index);
  }
  vec.pop_back();
  UpdateLocalObserverFlag(*ref.instance, ref.signal);
}

void Engine::RemoveGlobalRebindWatcherSub(const GlobalSubRef& ref) {
  auto& vec = ResolveGlobalSubSlot(ref.signal).rebind_subs;
  auto index = ref.index;
  if (vec[index].cold_idx != UINT32_MAX) {
    FreeWatcherCold(vec[index].cold_idx);
  }
  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    BackpatchMovedGlobalSubRef(
        process_states_, vec[index].process_id, vec[index].process_sub_idx,
        index);
  }
  vec.pop_back();
  UpdateGlobalObserverFlag(ref.signal);
}

void Engine::RemoveLocalContainerSub(const LocalSubRef& ref) {
  auto& vec = ResolveLocalSubSlot(*ref.instance, ref.signal).container_subs;
  auto index = ref.index;
  if (vec[index].cold_idx != UINT32_MAX) {
    auto& cold = container_cold_pool_[vec[index].cold_idx];
    if (cold.edge_target_id != UINT32_MAX) {
      FreeEdgeTarget(cold.edge_target_id);
    }
    FreeContainerCold(vec[index].cold_idx);
  }
  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    BackpatchMovedLocalSubRef(
        process_states_, vec[index].process_id, vec[index].process_sub_idx,
        index);
    if (vec[index].cold_idx != UINT32_MAX) {
      auto& moved_cold = container_cold_pool_[vec[index].cold_idx];
      if (moved_cold.edge_target_id != UINT32_MAX) {
        UpdateMovedEdgeTargetIndex(
            local_edge_target_table_, global_edge_target_table_,
            moved_cold.edge_target_id, index);
      }
    }
  }
  vec.pop_back();
  UpdateLocalObserverFlag(*ref.instance, ref.signal);
}

void Engine::RemoveGlobalContainerSub(const GlobalSubRef& ref) {
  auto& vec = ResolveGlobalSubSlot(ref.signal).container_subs;
  auto index = ref.index;
  if (vec[index].cold_idx != UINT32_MAX) {
    auto& cold = container_cold_pool_[vec[index].cold_idx];
    if (cold.edge_target_id != UINT32_MAX) {
      FreeEdgeTarget(cold.edge_target_id);
    }
    FreeContainerCold(vec[index].cold_idx);
  }
  uint32_t last = static_cast<uint32_t>(vec.size()) - 1;
  if (index != last) {
    vec[index] = vec[last];
    BackpatchMovedGlobalSubRef(
        process_states_, vec[index].process_id, vec[index].process_sub_idx,
        index);
    if (vec[index].cold_idx != UINT32_MAX) {
      auto& moved_cold = container_cold_pool_[vec[index].cold_idx];
      if (moved_cold.edge_target_id != UINT32_MAX) {
        UpdateMovedEdgeTargetIndex(
            local_edge_target_table_, global_edge_target_table_,
            moved_cold.edge_target_id, index);
      }
    }
  }
  vec.pop_back();
  UpdateGlobalObserverFlag(ref.signal);
}

void Engine::ClearInstalledSubscriptions(ProcessHandle handle) {
  if (handle.process_id >= num_processes_) {
    return;
  }
  auto& proc_state = process_states_[handle.process_id];

  // Iterate in reverse so swap-and-pop doesn't invalidate earlier indices
  // belonging to this same process (they'll be removed too).
  for (const auto& ref : std::views::reverse(proc_state.local_sub_refs)) {
    switch (ref.kind) {
      case SubKind::kEdge:
        RemoveLocalEdgeSub(ref);
        break;
      case SubKind::kChange:
        RemoveLocalChangeSub(ref);
        break;
      case SubKind::kRebindWatcher:
        RemoveLocalRebindWatcherSub(ref);
        break;
      case SubKind::kContainer:
        RemoveLocalContainerSub(ref);
        break;
    }
    if (live_subscription_count_ == 0) {
      throw common::InternalError(
          "Engine::ClearInstalledSubscriptions",
          "live_subscription_count_ underflow");
    }
    --live_subscription_count_;
  }
  for (const auto& ref : std::views::reverse(proc_state.global_sub_refs)) {
    switch (ref.kind) {
      case SubKind::kEdge:
        RemoveGlobalEdgeSub(ref);
        break;
      case SubKind::kChange:
        RemoveGlobalChangeSub(ref);
        break;
      case SubKind::kRebindWatcher:
        RemoveGlobalRebindWatcherSub(ref);
        break;
      case SubKind::kContainer:
        RemoveGlobalContainerSub(ref);
        break;
    }
    if (live_subscription_count_ == 0) {
      throw common::InternalError(
          "Engine::ClearInstalledSubscriptions",
          "live_subscription_count_ underflow");
    }
    --live_subscription_count_;
  }

  proc_state.local_sub_refs.clear();
  proc_state.global_sub_refs.clear();
  proc_state.subscription_count = 0;
  proc_state.plan_pool.ops.clear();
}

void Engine::InvalidateInstalledWait(ProcessHandle handle) {
  if (handle.process_id >= num_processes_) return;
  auto& proc_state = process_states_[handle.process_id];
  proc_state.installed_wait = InstalledWaitState{};
}

void Engine::ResetInstalledWait(ProcessHandle handle) {
  ClearInstalledSubscriptions(handle);
  InvalidateInstalledWait(handle);
}

void Engine::ClearProcessSubscriptions(ProcessHandle handle) {
  ResetInstalledWait(handle);
}

// Refresh installed edge/change baselines for subscriptions whose slots
// were dirtied after the most recent flush.
//
// Precondition: the caller (ReconcilePostActivation) has already verified
// that DeltaDirtySlots() is non-empty. When no post-flush dirties exist,
// installed baselines are already correct from FlushSlotEdgeGroups
// (group.last_bit) and FlushSlotChangeSubs (snapshot memcpy), and this
// function must not be called.
//
// Only called on the can_refresh path (WaitShapeKind::kStatic), so all
// sub_refs are snapshot-bearing (kEdge or kChange). Rebind watchers and
// container subs are structurally impossible here.
auto Engine::RefreshInstalledSnapshots(ProcessHandle handle) -> bool {
  if (handle.process_id >= num_processes_) return false;

  auto& proc_state = process_states_[handle.process_id];

  // R5: Domain-split watermark skip. Check both global and local
  // freshness to determine if any new dirty marks appeared since the
  // last refresh. Skip if both domains are unchanged.
  auto current_global_epoch = update_set_.DeltaEpoch();
  auto current_global_dirty =
      static_cast<uint32_t>(update_set_.DeltaDirtySlots().size());
  bool global_unchanged =
      (current_global_epoch ==
           proc_state.installed_wait.last_global_refresh_epoch &&
       current_global_dirty ==
           proc_state.installed_wait.last_global_refresh_dirty_count);
  bool local_unchanged = true;
  for (const auto& stamp : proc_state.installed_wait.local_refresh_epochs) {
    if (stamp.instance == nullptr) {
      throw common::InternalError(
          "Engine::RefreshInstalledSnapshots",
          "installed wait has null instance in local_refresh_epochs");
    }
    if (stamp.instance->observability.local_flush_epoch != stamp.epoch) {
      local_unchanged = false;
      break;
    }
  }
  if (global_unchanged && local_unchanged) {
    return false;
  }

  bool needs_reinstall = false;

  // Local sub refresh -- direct instance pointer, no InstanceId lookup.
  for (const auto& ref : proc_state.local_sub_refs) {
    if (ref.instance == nullptr) {
      throw common::InternalError(
          "Engine::RefreshInstalledSnapshots",
          "local sub ref has null instance");
    }
    auto& obs = ref.instance->observability;
    if (!obs.local_updates.IsDeltaDirty(ref.signal)) continue;
    const auto& imeta = obs.layout->slot_meta[ref.signal.value];
    auto storage = std::span(
        ResolveInstanceSlotBase(*ref.instance, ref.signal), imeta.total_bytes);

    switch (ref.kind) {
      case SubKind::kEdge: {
        auto& slot = ResolveLocalSubSlot(*ref.instance, ref.signal);
        auto& group = slot.edge_groups[ref.edge_group];
        auto& vec = (ref.edge_bucket == EdgeBucket::kPosedge)
                        ? group.posedge_subs
                        : group.negedge_subs;
        auto& sub = vec[ref.index];
        uint8_t current_byte = storage[group.byte_offset];
        uint8_t current_bit = (current_byte >> group.bit_index) & 1;
        if (current_bit != group.last_bit) {
          needs_reinstall = true;
        }
        if (sub.cold_idx != UINT32_MAX) {
          auto& cold = edge_cold_pool_[sub.cold_idx];
          cold.edge_last_byte = current_byte;
          cold.has_edge_last_byte = true;
        }
        break;
      }
      case SubKind::kChange: {
        auto& sub = ResolveLocalSubSlot(*ref.instance, ref.signal)
                        .change_subs[ref.index];
        const auto* current = &storage[sub.byte_offset];
        if (sub.byte_size <= ChangeSub::kInlineSnapshotCap) {
          std::memcpy(sub.snapshot_inline.data(), current, sub.byte_size);
        } else if (sub.cold_idx != UINT32_MAX) {
          auto& cold = change_cold_pool_[sub.cold_idx];
          if (cold.snapshot.size() < sub.byte_size) {
            cold.snapshot.resize(sub.byte_size);
          }
          std::memcpy(cold.snapshot.data(), current, sub.byte_size);
        }
        break;
      }
      case SubKind::kRebindWatcher:
        throw common::InternalError(
            "Engine::RefreshInstalledSnapshots",
            std::format(
                "process {} has rebind watcher on static refresh path",
                handle.process_id));
      case SubKind::kContainer:
        throw common::InternalError(
            "Engine::RefreshInstalledSnapshots",
            std::format(
                "process {} has container sub on static refresh path",
                handle.process_id));
    }
  }

  // Global sub refresh.
  for (const auto& ref : proc_state.global_sub_refs) {
    if (!update_set_.IsDeltaDirty(ref.signal.value)) continue;
    const auto& meta = slot_meta_registry_.Get(ref.signal.value);
    auto storage = std::span(
        ResolveSlotBase(meta, design_state_base_, const_instances_),
        meta.total_bytes);

    switch (ref.kind) {
      case SubKind::kEdge: {
        auto& slot = ResolveGlobalSubSlot(ref.signal);
        auto& group = slot.edge_groups[ref.edge_group];
        auto& vec = (ref.edge_bucket == EdgeBucket::kPosedge)
                        ? group.posedge_subs
                        : group.negedge_subs;
        auto& sub = vec[ref.index];
        uint8_t current_byte = storage[group.byte_offset];
        uint8_t current_bit = (current_byte >> group.bit_index) & 1;
        if (current_bit != group.last_bit) {
          needs_reinstall = true;
        }
        if (sub.cold_idx != UINT32_MAX) {
          auto& cold = edge_cold_pool_[sub.cold_idx];
          cold.edge_last_byte = current_byte;
          cold.has_edge_last_byte = true;
        }
        break;
      }
      case SubKind::kChange: {
        auto& sub = ResolveGlobalSubSlot(ref.signal).change_subs[ref.index];
        const auto* current = &storage[sub.byte_offset];
        if (sub.byte_size <= ChangeSub::kInlineSnapshotCap) {
          std::memcpy(sub.snapshot_inline.data(), current, sub.byte_size);
        } else if (sub.cold_idx != UINT32_MAX) {
          auto& cold = change_cold_pool_[sub.cold_idx];
          if (cold.snapshot.size() < sub.byte_size) {
            cold.snapshot.resize(sub.byte_size);
          }
          std::memcpy(cold.snapshot.data(), current, sub.byte_size);
        }
        break;
      }
      case SubKind::kRebindWatcher:
        throw common::InternalError(
            "Engine::RefreshInstalledSnapshots",
            std::format(
                "process {} has rebind watcher on static refresh path",
                handle.process_id));
      case SubKind::kContainer:
        throw common::InternalError(
            "Engine::RefreshInstalledSnapshots",
            std::format(
                "process {} has container sub on static refresh path",
                handle.process_id));
    }
  }

  // Update watermark after refresh.
  proc_state.installed_wait.last_global_refresh_epoch = current_global_epoch;
  proc_state.installed_wait.last_global_refresh_dirty_count =
      current_global_dirty;
  for (auto& stamp : proc_state.installed_wait.local_refresh_epochs) {
    if (stamp.instance == nullptr) {
      throw common::InternalError(
          "Engine::RefreshInstalledSnapshots",
          "installed wait has null instance in local_refresh_epochs");
    }
    stamp.epoch = stamp.instance->observability.local_flush_epoch;
  }
  return needs_reinstall;
}

void Engine::InstallTriggers(ProcessHandle handle, const WaitRequest& req) {
  auto resume = req.resume;
  auto triggers = req.triggers;
  const auto& late_bound = req.late_bound;
  // Track created subscription info for late-bound rebinding.
  // Invariant: created_subs[i] records the dense vector index assigned
  // to trigger i at creation time. These indices remain stable within
  // this function because no removals occur between subscription
  // creation and rebind hookup below.
  struct CreatedSub {
    SignalId signal_id = {};
    SubKind kind = {};
    uint32_t index = 0;
    uint8_t edge_group = 0;
    EdgeBucket edge_bucket = EdgeBucket::kPosedge;
    bool is_local = false;
  };
  bool has_late_bound = !late_bound.headers.empty();
  std::vector<CreatedSub> created_subs;
  if (has_late_bound) {
    created_subs.resize(
        triggers.size(),
        CreatedSub{
            .signal_id = 0, .kind = SubKind::kEdge, .index = UINT32_MAX});
  }

  for (uint32_t i = 0; i < triggers.size(); ++i) {
    const auto& trigger = triggers[i];
    auto edge = static_cast<common::EdgeKind>(trigger.edge);
    bool initially_active = (trigger.flags & kTriggerInitiallyActive) != 0;
    bool is_local = (trigger.flags & kTriggerLocalSignal) != 0;
    bool is_cross_instance = (trigger.flags & kTriggerCrossInstanceLocal) != 0;

    // Validate flag consistency.
    if (is_cross_instance && !is_local) {
      throw common::InternalError(
          "Engine::InstallTriggers",
          std::format(
              "trigger {}: kTriggerCrossInstanceLocal without "
              "kTriggerLocalSignal",
              i));
    }

    // Build typed signal reference from producer metadata directly.
    SignalRef sig_ref;
    if (is_cross_instance) {
      sig_ref = ValidateCrossInstanceLocal(
          *this, InstanceId{trigger.target_instance_id},
          LocalSignalId{trigger.target_local_signal_id},
          "Engine::InstallTriggers");
    } else if (is_local) {
      InstanceId inst_id = (handle.process_id < process_instance_map_.size())
                               ? process_instance_map_[handle.process_id]
                               : handle.instance_id;
      sig_ref = LocalSignalRef{
          .instance_id = inst_id, .signal = LocalSignalId{trigger.signal_id}};
    } else {
      sig_ref = GlobalSignalId{trigger.signal_id};
    }
    uint32_t sub_idx = UINT32_MAX;
    SubKind sub_kind = SubKind::kEdge;

    if (trigger.kind > static_cast<uint8_t>(TriggerInstallKind::kContainer)) {
      throw common::InternalError(
          "Engine::InstallTriggers",
          std::format("trigger {}: invalid kind {}", i, trigger.kind));
    }
    auto install_kind = static_cast<TriggerInstallKind>(trigger.kind);

    switch (install_kind) {
      case TriggerInstallKind::kContainer: {
        if (trigger.container_elem_stride == 0) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kContainer requires container_elem_stride > 0",
                  i));
        }
        if (trigger.byte_size != 0) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kContainer requires byte_size == 0, got {}", i,
                  trigger.byte_size));
        }
        if (trigger.bit_index != 0) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kContainer requires bit_index == 0, got {}", i,
                  trigger.bit_index));
        }
        if (edge != common::EdgeKind::kPosedge &&
            edge != common::EdgeKind::kNegedge &&
            edge != common::EdgeKind::kAnyChange) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kContainer has invalid edge {}", i,
                  static_cast<uint8_t>(edge)));
        }
        // For kContainer, byte_offset carries the element index directly.
        int64_t sv_index =
            initially_active ? static_cast<int64_t>(trigger.byte_offset) : -1;
        sub_idx = SubscribeContainerElement(
            handle, resume, sig_ref, edge, sv_index,
            trigger.container_elem_stride, initially_active);
        sub_kind = SubKind::kContainer;
        break;
      }
      case TriggerInstallKind::kChange: {
        if (edge != common::EdgeKind::kAnyChange) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kChange requires kAnyChange edge, got {}", i,
                  static_cast<uint8_t>(edge)));
        }
        if (trigger.byte_size > 0) {
          sub_idx = Subscribe(
              handle, resume, sig_ref, edge, trigger.byte_offset,
              trigger.byte_size, trigger.bit_index, initially_active);
        } else {
          sub_idx = Subscribe(handle, resume, sig_ref, edge, initially_active);
        }
        sub_kind = SubKind::kChange;
        break;
      }
      case TriggerInstallKind::kEdge: {
        if (edge == common::EdgeKind::kAnyChange) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              std::format(
                  "trigger {}: kEdge requires posedge/negedge, got kAnyChange",
                  i));
        }
        if (trigger.byte_size > 0) {
          sub_idx = Subscribe(
              handle, resume, sig_ref, edge, trigger.byte_offset,
              trigger.byte_size, trigger.bit_index, initially_active);
        } else {
          sub_idx = Subscribe(handle, resume, sig_ref, edge, initially_active);
        }
        sub_kind = SubKind::kEdge;
        break;
      }
    }

    if (has_late_bound) {
      CreatedSub cs{
          .signal_id = trigger.signal_id,
          .kind = sub_kind,
          .index = sub_idx,
          .is_local = is_local};
      if (sub_kind == SubKind::kEdge && sub_idx != UINT32_MAX) {
        auto& ps = process_states_[handle.process_id];
        if (is_local) {
          auto& last_ref = ps.local_sub_refs.back();
          cs.edge_group = last_ref.edge_group;
          cs.edge_bucket = last_ref.edge_bucket;
        } else {
          auto& last_ref = ps.global_sub_refs.back();
          cs.edge_group = last_ref.edge_group;
          cs.edge_bucket = last_ref.edge_bucket;
        }
      }
      created_subs[i] = cs;
    }
  }

  // Install rebind watchers from late-bound headers.
  for (uint32_t h = 0; h < late_bound.headers.size(); ++h) {
    const auto& hdr = late_bound.headers[h];

    if (hdr.trigger_index >= triggers.size()) {
      throw common::InternalError(
          "Engine::InstallTriggers",
          std::format(
              "late_bound[{}]: trigger_index {} >= num_triggers {}", h,
              hdr.trigger_index, triggers.size()));
    }
    if (static_cast<uint64_t>(hdr.plan_ops_start) + hdr.plan_ops_count >
        late_bound.plan_ops.size()) {
      throw common::InternalError(
          "Engine::InstallTriggers",
          std::format(
              "late_bound[{}]: plan_ops span [{}, +{}) exceeds pool size {}", h,
              hdr.plan_ops_start, hdr.plan_ops_count,
              late_bound.plan_ops.size()));
    }
    if (static_cast<uint64_t>(hdr.dep_slots_start) + hdr.dep_slots_count >
        late_bound.dep_slots.size()) {
      throw common::InternalError(
          "Engine::InstallTriggers",
          std::format(
              "late_bound[{}]: dep_slots span [{}, +{}) exceeds pool size {}",
              h, hdr.dep_slots_start, hdr.dep_slots_count,
              late_bound.dep_slots.size()));
    }

    const auto& target = created_subs[hdr.trigger_index];
    if (target.index == UINT32_MAX) continue;
    if (hdr.dep_slots_count == 0) continue;

    BitTargetMapping mapping{
        .index_base = hdr.index_base,
        .index_step = hdr.index_step,
        .total_bits = hdr.total_bits};
    auto hdr_plan =
        late_bound.plan_ops.subspan(hdr.plan_ops_start, hdr.plan_ops_count);

    // Resolve instance_id for the process (used for local deps).
    InstanceId rebind_inst_id =
        (handle.process_id < process_instance_map_.size())
            ? process_instance_map_[handle.process_id]
            : handle.instance_id;

    // Decode each dep record into a typed SignalRef.
    auto hdr_dep_records =
        late_bound.dep_slots.subspan(hdr.dep_slots_start, hdr.dep_slots_count);
    std::vector<SignalRef> dep_signals;
    dep_signals.reserve(hdr_dep_records.size());
    for (const auto& rec : hdr_dep_records) {
      bool dep_cross = (rec.flags & kDepCrossInstanceLocal) != 0;
      if (dep_cross) {
        if ((rec.flags & kDepLocalSignal) == 0) {
          throw common::InternalError(
              "Engine::InstallTriggers",
              "dep record: kDepCrossInstanceLocal without kDepLocalSignal");
        }
        dep_signals.emplace_back(ValidateCrossInstanceLocal(
            *this, InstanceId{rec.target_instance_id},
            LocalSignalId{rec.target_local_signal_id},
            "Engine::InstallTriggers(dep)"));
      } else if ((rec.flags & kDepLocalSignal) != 0) {
        dep_signals.emplace_back(
            LocalSignalRef{
                .instance_id = rebind_inst_id,
                .signal = LocalSignalId{rec.signal_id}});
      } else {
        dep_signals.emplace_back(GlobalSignalId{rec.signal_id});
      }
    }

    SignalRef rebind_target =
        target.is_local ? SignalRef{LocalSignalRef{
                              .instance_id = rebind_inst_id,
                              .signal = LocalSignalId{target.signal_id}}}
                        : SignalRef{GlobalSignalId{target.signal_id}};
    SubscribeRebind(
        handle, UINT32_MAX, rebind_target, target.kind, target.index,
        target.edge_group, target.edge_bucket, hdr_plan, mapping, dep_signals);
  }
}

void Engine::InstallWaitSite(ProcessHandle handle, const WaitRequest& req) {
  const auto& descriptor = wait_site_meta_.Get(req.wait_site_id);
  // Validate compiled-vs-runtime agreement before any installation work.
  if (descriptor.resume_block != req.resume.block_index) {
    throw common::InternalError(
        "Engine::InstallWaitSite",
        std::format(
            "process {} wait_site {} resume_block mismatch: "
            "descriptor={} vs request={}",
            handle.process_id, descriptor.id, descriptor.resume_block,
            req.resume.block_index));
  }
  if (descriptor.num_triggers != static_cast<uint32_t>(req.triggers.size())) {
    throw common::InternalError(
        "Engine::InstallWaitSite",
        std::format(
            "process {} wait_site {} num_triggers mismatch: "
            "descriptor={} vs request={}",
            handle.process_id, descriptor.id, descriptor.num_triggers,
            req.triggers.size()));
  }

  bool has_late_bound = !req.late_bound.headers.empty();

  // Validate late-bound presence matches compiled descriptor.
  if (has_late_bound != descriptor.has_late_bound) {
    throw common::InternalError(
        "Engine::InstallWaitSite",
        std::format(
            "process {} wait_site {}: has_late_bound mismatch: "
            "descriptor={} vs request={}",
            handle.process_id, descriptor.id, descriptor.has_late_bound,
            has_late_bound));
  }

  InstallTriggers(handle, req);

  // Install-time realized-state invariant: when the compiled shape is
  // kStatic, the installed subscription set must contain only
  // snapshot-bearing triggers (kEdge/kChange). This is expected because
  // static waits have no late-bound indices, so InstallTriggers should not
  // produce rebind watcher or container subs for this shape.
  auto& proc_state = process_states_[handle.process_id];
  if (descriptor.shape == WaitShapeKind::kStatic) {
    auto is_snapshot_bearing = [](const auto& ref) {
      return ref.kind == SubKind::kEdge || ref.kind == SubKind::kChange;
    };
    if (!std::ranges::all_of(proc_state.local_sub_refs, is_snapshot_bearing) ||
        !std::ranges::all_of(proc_state.global_sub_refs, is_snapshot_bearing)) {
      throw common::InternalError(
          "Engine::InstallWaitSite",
          std::format(
              "process {} wait_site {}: kStatic shape but installed "
              "non-snapshot-bearing sub_refs",
              handle.process_id, descriptor.id));
    }
  }

  // Publish install-time state with derived policy.
  proc_state.installed_wait = InstalledWaitState{
      .wait_site_id = descriptor.id,
      .valid = true,
      .can_refresh_snapshot = (descriptor.shape == WaitShapeKind::kStatic),
      .local_refresh_epochs = {}};

  // Snapshots are fresh from install -- set domain-split watermark.
  // Global watermark from update_set_.
  proc_state.installed_wait.last_global_refresh_epoch =
      update_set_.DeltaEpoch();
  proc_state.installed_wait.last_global_refresh_dirty_count =
      static_cast<uint32_t>(update_set_.DeltaDirtySlots().size());
  // Local watermark: track only instances this wait site depends on.
  // Iterate local_sub_refs only -- global subs have no instance dependency.
  proc_state.installed_wait.local_refresh_epochs.clear();
  for (const auto& ref : proc_state.local_sub_refs) {
    if (ref.instance == nullptr) {
      throw common::InternalError(
          "Engine::InstallWaitSite", "local sub ref has null instance");
    }
    // Deduplicate by instance pointer (small sets, linear scan fine).
    bool already_tracked = false;
    for (const auto& stamp : proc_state.installed_wait.local_refresh_epochs) {
      if (stamp.instance == ref.instance) {
        already_tracked = true;
        break;
      }
    }
    if (!already_tracked) {
      proc_state.installed_wait.local_refresh_epochs.push_back(
          InstalledWaitState::LocalRefreshStamp{
              .instance = ref.instance,
              .epoch = ref.instance->observability.local_flush_epoch});
    }
  }
}

auto Engine::CanRefreshInstalledWait(
    ProcessHandle handle, WaitSiteId wait_site_id) const -> bool {
  if (handle.process_id >= process_states_.size()) return false;
  const auto& installed = process_states_[handle.process_id].installed_wait;
  return installed.valid && installed.wait_site_id == wait_site_id &&
         installed.can_refresh_snapshot;
}

auto Engine::HasPendingDirtyState() const -> bool {
  return !update_set_.DeltaDirtySlots().empty() ||
         !delta_dirty_instances_.empty();
}

void Engine::ReconcilePostActivation(ProcessHandle handle) {
  if (!HasPostActivationReconciliation()) {
    throw common::InternalError(
        "Engine::ReconcilePostActivation",
        "called without post-activation reconciliation capability");
  }
  if (handle.process_id >= suspend_records_.size()) {
    throw common::InternalError(
        "Engine::ReconcilePostActivation",
        std::format(
            "process_id {} >= suspend_records size {}", handle.process_id,
            suspend_records_.size()));
  }
  auto* suspend = suspend_records_[handle.process_id];

  auto resume =
      ResumePoint{.block_index = suspend->resume_block, .instruction_index = 0};

  switch (suspend->tag) {
    case SuspendTag::kFinished:
      ResetInstalledWait(handle);
      break;

    case SuspendTag::kDelay:
      ResetInstalledWait(handle);
      Delay(handle, resume, suspend->delay_ticks);
      break;

    case SuspendTag::kWait: {
      if (suspend->wait_site_id == kInvalidWaitSiteId) {
        throw common::InternalError(
            "Engine::ReconcilePostActivation",
            std::format(
                "process {} suspended with kWait but wait_site_id is invalid",
                handle.process_id));
      }

      auto& proc_state = process_states_[handle.process_id];

      if (!CanRefreshInPlace(
              proc_state.installed_wait, suspend->wait_site_id)) {
        // Reinstall required: different wait site or non-static shape.
        const auto& descriptor = wait_site_meta_.Get(suspend->wait_site_id);
        ResetInstalledWait(handle);
        InstallWaitSite(handle, suspend, descriptor);
        break;
      }

      // Flush owns baseline advancement for installed edge/change waits.
      // FlushSlotEdgeGroups sets group.last_bit; FlushSlotChangeSubs
      // memcpy's snapshots. ClearDelta() resets post-flush dirtiness
      // tracking but does NOT invalidate those installed baselines.
      // Post-activation refresh is only needed when post-flush slot
      // dirties are recorded in delta_dirty_ (any active-region mutation
      // path that calls MarkSlotDirty or MarkDirtyRange).
      //
      // Note: MarkExternalDirtyRange (container heap mutations) also
      // pushes to delta_dirty_ via TouchSlot, which is intentional
      // over-invalidation. Edge/change baselines observe design-state
      // bytes, not heap data, so the per-sub IsDeltaDirty checks inside
      // RefreshInstalledSnapshots filter these out harmlessly.
      // R5: check both global and local dirty state. Instance-owned
      // signals go to local_updates, not update_set_.
      bool has_any_dirty = !update_set_.DeltaDirtySlots().empty() ||
                           !delta_dirty_instances_.empty();
      if (!has_any_dirty) {
        break;
      }

      if (RefreshInstalledSnapshots(handle)) {
        // A same-delta blocking write changed an observed edge bit.
        // group.last_bit is shared state and cannot be updated here.
        // Fall back to full reinstall to get a fresh group baseline.
        const auto& descriptor = wait_site_meta_.Get(suspend->wait_site_id);
        ResetInstalledWait(handle);
        InstallWaitSite(handle, suspend, descriptor);
      }
      break;
    }

    case SuspendTag::kRepeat:
      ResetInstalledWait(handle);
      ScheduleNextDelta(handle, ResumePoint{.block_index = 0});
      break;

    case SuspendTag::kWaitEvent: {
      ResetInstalledWait(handle);
      auto& inst = GetInstanceMut(handle.instance_id);
      AddInstanceEventWaiter(
          inst, suspend->event_id,
          EventWaiter{
              .process_id = handle.process_id,
              .instance = &inst,
              .resume_block = suspend->resume_block,
          });
      break;
    }
  }
}

// R5: Domain-split edge/change subscribe helpers.
// Shared core that operates on pre-resolved slot_base and SlotSubscriptions.

namespace {

auto FindOrCreateEdgeGroupInSlot(
    SlotSubscriptions& slot, uint32_t byte_offset, uint8_t bit_index,
    uint8_t initial_last_bit) -> uint8_t {
  auto& groups = slot.edge_groups;
  for (uint32_t i = 0; i < groups.size(); ++i) {
    if (groups[i].byte_offset == byte_offset &&
        groups[i].bit_index == bit_index) {
      if (groups[i].posedge_subs.empty() && groups[i].negedge_subs.empty()) {
        groups[i].last_bit = initial_last_bit;
      }
      return static_cast<uint8_t>(i);
    }
  }
  for (uint32_t i = 0; i < groups.size(); ++i) {
    if (groups[i].posedge_subs.empty() && groups[i].negedge_subs.empty()) {
      groups[i].byte_offset = byte_offset;
      groups[i].bit_index = bit_index;
      groups[i].last_bit = initial_last_bit;
      return static_cast<uint8_t>(i);
    }
  }
  auto idx = static_cast<uint8_t>(groups.size());
  groups.push_back(
      EdgeWatchGroup{
          .byte_offset = byte_offset,
          .bit_index = bit_index,
          .last_bit = initial_last_bit,
          .posedge_subs = {},
          .negedge_subs = {}});
  return idx;
}

}  // namespace

auto Engine::SubscribeGlobalChange(
    ProcessHandle handle, ResumePoint resume, GlobalSignalId signal,
    uint32_t byte_offset, uint32_t byte_size, bool initially_active)
    -> uint32_t {
  if (finished_) return UINT32_MAX;
  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::SubscribeGlobalChange", "Subscribe before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::SubscribeGlobalChange", "Subscribe before InitSlotMeta");
  }
  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeGlobalChange",
        std::format(
            "process_id {} exceeds num_processes {}", handle.process_id,
            num_processes_));
  }
  auto& proc_state = process_states_[handle.process_id];
  if (!CheckSubscriptionLimits(proc_state)) return UINT32_MAX;

  const auto& meta = slot_meta_registry_.Get(signal.value);
  if (byte_size == 0 || byte_offset + byte_size > meta.total_bytes) {
    throw common::InternalError(
        "Engine::SubscribeGlobalChange", "invalid observation range");
  }

  auto& slot = signal_subs_[signal.value];
  auto sub_idx = static_cast<uint32_t>(slot.change_subs.size());
  auto proc_sub_idx = static_cast<uint32_t>(proc_state.global_sub_refs.size());

  ChangeSub sub{};
  sub.process_id = handle.process_id;
  sub.instance_id = handle.instance_id;
  sub.resume_block = resume.block_index;
  sub.byte_offset = byte_offset;
  sub.byte_size = byte_size;
  sub.process_sub_idx = proc_sub_idx;
  sub.cold_idx = UINT32_MAX;
  sub.flags = initially_active ? kSubActive : 0;

  auto storage = std::span(
      ResolveSlotBase(meta, design_state_base_, const_instances_),
      meta.total_bytes);
  const auto* src = &storage[byte_offset];
  if (byte_size <= ChangeSub::kInlineSnapshotCap) {
    std::memcpy(sub.snapshot_inline.data(), src, byte_size);
  } else {
    sub.cold_idx = AllocChangeCold();
    auto& cold = change_cold_pool_[sub.cold_idx];
    cold.snapshot.resize(byte_size);
    std::memcpy(cold.snapshot.data(), src, byte_size);
  }

  slot.change_subs.push_back(sub);
  proc_state.global_sub_refs.push_back(
      GlobalSubRef{
          .signal = signal, .index = sub_idx, .kind = SubKind::kChange});
  ++proc_state.subscription_count;
  ++live_subscription_count_;
  global_has_observers_[signal.value] = 1;
  return sub_idx;
}

auto Engine::SubscribeLocalChange(
    ProcessHandle handle, ResumePoint resume, LocalSignalRef signal,
    uint32_t byte_offset, uint32_t byte_size, bool initially_active)
    -> uint32_t {
  if (finished_) return UINT32_MAX;
  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeLocalChange",
        std::format(
            "process_id {} exceeds num_processes {}", handle.process_id,
            num_processes_));
  }
  auto& proc_state = process_states_[handle.process_id];
  if (!CheckSubscriptionLimits(proc_state)) return UINT32_MAX;

  auto& inst = GetInstanceMut(signal.instance_id);
  const auto& inst_meta =
      inst.observability.layout->slot_meta[signal.signal.value];
  if (byte_size == 0 || byte_offset + byte_size > inst_meta.total_bytes) {
    throw common::InternalError(
        "Engine::SubscribeLocalChange", "invalid observation range");
  }

  auto& slot = inst.observability.local_signal_subs[signal.signal.value];
  auto sub_idx = static_cast<uint32_t>(slot.change_subs.size());
  auto proc_sub_idx = static_cast<uint32_t>(proc_state.local_sub_refs.size());

  ChangeSub sub{};
  sub.process_id = handle.process_id;
  sub.instance_id = signal.instance_id;
  sub.resume_block = resume.block_index;
  sub.byte_offset = byte_offset;
  sub.byte_size = byte_size;
  sub.process_sub_idx = proc_sub_idx;
  sub.cold_idx = UINT32_MAX;
  sub.flags = initially_active ? kSubActive : 0;

  auto storage = std::span(
      ResolveInstanceSlotBase(inst, signal.signal), inst_meta.total_bytes);
  const auto* src = &storage[byte_offset];
  if (byte_size <= ChangeSub::kInlineSnapshotCap) {
    std::memcpy(sub.snapshot_inline.data(), src, byte_size);
  } else {
    sub.cold_idx = AllocChangeCold();
    auto& cold = change_cold_pool_[sub.cold_idx];
    cold.snapshot.resize(byte_size);
    std::memcpy(cold.snapshot.data(), src, byte_size);
  }

  slot.change_subs.push_back(sub);
  proc_state.local_sub_refs.push_back(
      LocalSubRef{
          .instance = &inst,
          .signal = signal.signal,
          .index = sub_idx,
          .kind = SubKind::kChange});
  ++proc_state.subscription_count;
  ++live_subscription_count_;
  inst.observability.local_has_observers[signal.signal.value] = 1;
  return sub_idx;
}

auto Engine::SubscribeGlobalEdge(
    ProcessHandle handle, ResumePoint resume, GlobalSignalId signal,
    common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size,
    uint8_t bit_index, bool initially_active) -> uint32_t {
  if (finished_) return UINT32_MAX;
  if (byte_size != 1) {
    throw common::InternalError(
        "Engine::SubscribeGlobalEdge",
        "edge subscriptions require byte_size=1");
  }
  if (bit_index > 7) {
    throw common::InternalError(
        "Engine::SubscribeGlobalEdge", "bit_index must be in [0,7]");
  }
  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::SubscribeGlobalEdge", "Subscribe before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::SubscribeGlobalEdge", "Subscribe before InitSlotMeta");
  }
  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeGlobalEdge",
        std::format(
            "process_id {} exceeds num_processes {}", handle.process_id,
            num_processes_));
  }
  auto& proc_state = process_states_[handle.process_id];
  if (!CheckSubscriptionLimits(proc_state)) return UINT32_MAX;

  const auto& meta = slot_meta_registry_.Get(signal.value);
  if (byte_offset + byte_size > meta.total_bytes) {
    throw common::InternalError(
        "Engine::SubscribeGlobalEdge", "observation range exceeds slot size");
  }

  auto& slot = signal_subs_[signal.value];
  auto proc_sub_idx = static_cast<uint32_t>(proc_state.global_sub_refs.size());

  auto storage = std::span(
      ResolveSlotBase(meta, design_state_base_, const_instances_),
      meta.total_bytes);
  uint8_t initial_last_bit = (storage[byte_offset] >> bit_index) & 1;
  uint8_t group_idx = FindOrCreateEdgeGroupInSlot(
      slot, byte_offset, bit_index, initial_last_bit);

  EdgeBucket bucket = (edge == common::EdgeKind::kPosedge)
                          ? EdgeBucket::kPosedge
                          : EdgeBucket::kNegedge;
  auto& group = slot.edge_groups[group_idx];
  auto& target_vec = (bucket == EdgeBucket::kPosedge) ? group.posedge_subs
                                                      : group.negedge_subs;
  auto sub_idx = static_cast<uint32_t>(target_vec.size());

  EdgeSub sub{};
  sub.process_id = handle.process_id;
  sub.instance_id = handle.instance_id;
  sub.resume_block = resume.block_index;
  sub.flags = initially_active ? kSubActive : 0;
  sub.process_sub_idx = proc_sub_idx;
  sub.cold_idx = UINT32_MAX;

  target_vec.push_back(sub);
  proc_state.global_sub_refs.push_back(
      GlobalSubRef{
          .signal = signal,
          .index = sub_idx,
          .kind = SubKind::kEdge,
          .edge_bucket = bucket,
          .edge_group = group_idx});
  ++proc_state.subscription_count;
  ++live_subscription_count_;
  global_has_observers_[signal.value] = 1;
  return sub_idx;
}

auto Engine::SubscribeLocalEdge(
    ProcessHandle handle, ResumePoint resume, LocalSignalRef signal,
    common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size,
    uint8_t bit_index, bool initially_active) -> uint32_t {
  if (finished_) return UINT32_MAX;
  if (byte_size != 1) {
    throw common::InternalError(
        "Engine::SubscribeLocalEdge", "edge subscriptions require byte_size=1");
  }
  if (bit_index > 7) {
    throw common::InternalError(
        "Engine::SubscribeLocalEdge", "bit_index must be in [0,7]");
  }
  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeLocalEdge",
        std::format(
            "process_id {} exceeds num_processes {}", handle.process_id,
            num_processes_));
  }
  auto& proc_state = process_states_[handle.process_id];
  if (!CheckSubscriptionLimits(proc_state)) return UINT32_MAX;

  auto& inst = GetInstanceMut(signal.instance_id);
  const auto& inst_meta =
      inst.observability.layout->slot_meta[signal.signal.value];
  if (byte_offset + byte_size > inst_meta.total_bytes) {
    throw common::InternalError(
        "Engine::SubscribeLocalEdge", "observation range exceeds slot size");
  }

  auto& slot = inst.observability.local_signal_subs[signal.signal.value];
  auto proc_sub_idx = static_cast<uint32_t>(proc_state.local_sub_refs.size());

  auto storage = std::span(
      ResolveInstanceSlotBase(inst, signal.signal), inst_meta.total_bytes);
  uint8_t initial_last_bit = (storage[byte_offset] >> bit_index) & 1;
  uint8_t group_idx = FindOrCreateEdgeGroupInSlot(
      slot, byte_offset, bit_index, initial_last_bit);

  EdgeBucket bucket = (edge == common::EdgeKind::kPosedge)
                          ? EdgeBucket::kPosedge
                          : EdgeBucket::kNegedge;
  auto& group = slot.edge_groups[group_idx];
  auto& target_vec = (bucket == EdgeBucket::kPosedge) ? group.posedge_subs
                                                      : group.negedge_subs;
  auto sub_idx = static_cast<uint32_t>(target_vec.size());

  EdgeSub sub{};
  sub.process_id = handle.process_id;
  sub.instance_id = signal.instance_id;
  sub.resume_block = resume.block_index;
  sub.flags = initially_active ? kSubActive : 0;
  sub.process_sub_idx = proc_sub_idx;
  sub.cold_idx = UINT32_MAX;

  target_vec.push_back(sub);
  proc_state.local_sub_refs.push_back(
      LocalSubRef{
          .instance = &inst,
          .signal = signal.signal,
          .index = sub_idx,
          .kind = SubKind::kEdge,
          .edge_bucket = bucket,
          .edge_group = group_idx});
  ++proc_state.subscription_count;
  ++live_subscription_count_;
  inst.observability.local_has_observers[signal.signal.value] = 1;
  return sub_idx;
}

// R5: SignalRef top boundary -- dispatch once, then domain-specific internals.
auto Engine::Subscribe(
    ProcessHandle handle, ResumePoint resume, SignalRef signal_ref,
    common::EdgeKind edge, bool initially_active) -> uint32_t {
  return std::visit(
      [&](auto sig) -> uint32_t {
        using T = std::decay_t<decltype(sig)>;
        if constexpr (std::is_same_v<T, GlobalSignalId>) {
          if (!slot_meta_registry_.IsPopulated()) {
            throw common::InternalError(
                "Engine::Subscribe", "Subscribe before InitSlotMeta");
          }
          const auto& meta = slot_meta_registry_.Get(sig.value);
          uint32_t obs_size =
              (edge == common::EdgeKind::kAnyChange) ? meta.total_bytes : 1;
          if (edge == common::EdgeKind::kAnyChange) {
            return SubscribeGlobalChange(
                handle, resume, sig, 0, obs_size, initially_active);
          }
          return SubscribeGlobalEdge(
              handle, resume, sig, edge, 0, obs_size, 0, initially_active);
        } else {
          const auto& inst_meta =
              GetInstance(sig.instance_id)
                  .observability.layout->slot_meta[sig.signal.value];
          uint32_t obs_size = (edge == common::EdgeKind::kAnyChange)
                                  ? inst_meta.total_bytes
                                  : 1;
          if (edge == common::EdgeKind::kAnyChange) {
            return SubscribeLocalChange(
                handle, resume, sig, 0, obs_size, initially_active);
          }
          return SubscribeLocalEdge(
              handle, resume, sig, edge, 0, obs_size, 0, initially_active);
        }
      },
      signal_ref);
}

auto Engine::Subscribe(
    ProcessHandle handle, ResumePoint resume, SignalRef signal_ref,
    common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size,
    uint8_t bit_index, bool initially_active) -> uint32_t {
  return std::visit(
      [&](auto sig) -> uint32_t {
        using T = std::decay_t<decltype(sig)>;
        if constexpr (std::is_same_v<T, GlobalSignalId>) {
          if (edge == common::EdgeKind::kAnyChange) {
            return SubscribeGlobalChange(
                handle, resume, sig, byte_offset, byte_size, initially_active);
          }
          return SubscribeGlobalEdge(
              handle, resume, sig, edge, byte_offset, byte_size, bit_index,
              initially_active);
        } else {
          if (edge == common::EdgeKind::kAnyChange) {
            return SubscribeLocalChange(
                handle, resume, sig, byte_offset, byte_size, initially_active);
          }
          return SubscribeLocalEdge(
              handle, resume, sig, edge, byte_offset, byte_size, bit_index,
              initially_active);
        }
      },
      signal_ref);
}

auto Engine::SubscribeContainerElement(
    ProcessHandle handle, ResumePoint resume, SignalRef signal_ref,
    common::EdgeKind edge, int64_t sv_index, uint32_t elem_stride,
    bool initially_active) -> uint32_t {
  return std::visit(
      [&](auto sig) -> uint32_t {
        using T = std::decay_t<decltype(sig)>;
        if constexpr (std::is_same_v<T, GlobalSignalId>) {
          return SubscribeGlobalContainerElement(
              handle, resume, sig, edge, sv_index, elem_stride,
              initially_active);
        } else {
          return SubscribeLocalContainerElement(
              handle, resume, sig, edge, sv_index, elem_stride,
              initially_active);
        }
      },
      signal_ref);
}

namespace {

// Create and initialize a container sub from pre-resolved slot_base.
// Returns the container sub index within the slot.
void InitContainerSubState(
    ContainerSub& sub, ContainerCold& cold, uint32_t signal_id,
    int64_t sv_index, uint32_t elem_stride, bool initially_active,
    const uint8_t* slot_base) {
  cold.container_signal_id = signal_id;
  cold.container_elem_stride = elem_stride;
  cold.container_sv_index = sv_index;

  void* handle_ptr = nullptr;
  std::memcpy(&handle_ptr, slot_base, sizeof(void*));

  if (!initially_active) {
    cold.container_epoch = 0;
  } else if (handle_ptr == nullptr) {
    cold.container_epoch = 0;
  } else {
    const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
    if (arr->magic != DynArrayData::kMagic) {
      throw common::InternalError(
          "SubscribeContainerElement", "invalid container magic");
    }
    cold.container_epoch = arr->epoch;

    if (arr->data != nullptr && sv_index >= 0 && sv_index < arr->size) {
      auto byte_off = static_cast<uint32_t>(sv_index) * elem_stride;
      auto heap_data = std::span(
          static_cast<const uint8_t*>(arr->data), byte_off + elem_stride);
      sub.last_bit = heap_data[byte_off] & 1;
      sub.flags = kSubActive;
    }
  }
}

}  // namespace

auto Engine::SubscribeGlobalContainerElement(
    ProcessHandle handle, ResumePoint resume, GlobalSignalId signal,
    common::EdgeKind edge, int64_t sv_index, uint32_t elem_stride,
    bool initially_active) -> uint32_t {
  if (finished_) return UINT32_MAX;
  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::SubscribeGlobalContainerElement",
        "Subscribe before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::SubscribeGlobalContainerElement",
        "Subscribe before InitSlotMeta");
  }
  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeGlobalContainerElement",
        std::format(
            "process_id {} exceeds num_processes {}", handle.process_id,
            num_processes_));
  }
  if (elem_stride == 0) {
    throw common::InternalError(
        "Engine::SubscribeGlobalContainerElement", "elem_stride must be > 0");
  }
  auto& proc_state = process_states_[handle.process_id];
  if (!CheckSubscriptionLimits(proc_state)) return UINT32_MAX;

  const auto& meta = slot_meta_registry_.Get(signal.value);
  auto& slot = signal_subs_[signal.value];
  const auto* slot_base =
      ResolveSlotBase(meta, design_state_base_, const_instances_);

  auto sub_idx = static_cast<uint32_t>(slot.container_subs.size());
  auto proc_sub_idx = static_cast<uint32_t>(proc_state.global_sub_refs.size());
  uint32_t cold_idx = AllocContainerCold();

  ContainerSub sub{};
  sub.process_id = handle.process_id;
  sub.instance_id = handle.instance_id;
  sub.resume_block = resume.block_index;
  sub.process_sub_idx = proc_sub_idx;
  sub.cold_idx = cold_idx;
  sub.edge = edge;
  sub.last_bit = 0;
  sub.flags = 0;

  InitContainerSubState(
      sub, container_cold_pool_[cold_idx], signal.value, sv_index, elem_stride,
      initially_active, slot_base);

  slot.container_subs.push_back(sub);
  proc_state.global_sub_refs.push_back(
      GlobalSubRef{
          .signal = signal, .index = sub_idx, .kind = SubKind::kContainer});
  ++proc_state.subscription_count;
  ++live_subscription_count_;
  global_has_observers_[signal.value] = 1;
  return sub_idx;
}

auto Engine::SubscribeLocalContainerElement(
    ProcessHandle handle, ResumePoint resume, LocalSignalRef signal,
    common::EdgeKind edge, int64_t sv_index, uint32_t elem_stride,
    bool initially_active) -> uint32_t {
  if (finished_) return UINT32_MAX;
  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeLocalContainerElement",
        std::format(
            "process_id {} exceeds num_processes {}", handle.process_id,
            num_processes_));
  }
  if (elem_stride == 0) {
    throw common::InternalError(
        "Engine::SubscribeLocalContainerElement", "elem_stride must be > 0");
  }
  auto& proc_state = process_states_[handle.process_id];
  if (!CheckSubscriptionLimits(proc_state)) return UINT32_MAX;

  auto& inst = GetInstanceMut(signal.instance_id);
  auto& slot = inst.observability.local_signal_subs[signal.signal.value];
  const auto* slot_base = ResolveInstanceSlotBase(inst, signal.signal);

  auto sub_idx = static_cast<uint32_t>(slot.container_subs.size());
  auto proc_sub_idx = static_cast<uint32_t>(proc_state.local_sub_refs.size());
  uint32_t cold_idx = AllocContainerCold();

  ContainerSub sub{};
  sub.process_id = handle.process_id;
  sub.instance_id = signal.instance_id;
  sub.resume_block = resume.block_index;
  sub.process_sub_idx = proc_sub_idx;
  sub.cold_idx = cold_idx;
  sub.edge = edge;
  sub.last_bit = 0;
  sub.flags = 0;

  InitContainerSubState(
      sub, container_cold_pool_[cold_idx], signal.signal.value, sv_index,
      elem_stride, initially_active, slot_base);

  slot.container_subs.push_back(sub);
  proc_state.local_sub_refs.push_back(
      LocalSubRef{
          .instance = &inst,
          .signal = signal.signal,
          .index = sub_idx,
          .kind = SubKind::kContainer});
  ++proc_state.subscription_count;
  ++live_subscription_count_;
  inst.observability.local_has_observers[signal.signal.value] = 1;
  return sub_idx;
}

void Engine::ValidateRebindDepSignals(
    std::span<const SignalRef> dep_signals) const {
  for (const auto& dep : dep_signals) {
    std::visit(
        [&](const auto& sig) {
          using T = std::decay_t<decltype(sig)>;
          if constexpr (std::is_same_v<T, GlobalSignalId>) {
            if (!slot_meta_registry_.IsPopulated() ||
                sig.value >= slot_meta_registry_.Size()) {
              throw common::InternalError(
                  "Engine::ValidateRebindDepSignals",
                  std::format(
                      "global dep signal {} out of range (slot_meta size {})",
                      sig.value, slot_meta_registry_.Size()));
            }
            if (sig.value >= signal_subs_.size()) {
              throw common::InternalError(
                  "Engine::ValidateRebindDepSignals",
                  std::format(
                      "global dep signal {} >= signal_subs size {}", sig.value,
                      signal_subs_.size()));
            }
          } else {
            const auto* dep_inst = FindInstance(sig.instance_id);
            if (dep_inst == nullptr) {
              throw common::InternalError(
                  "Engine::ValidateRebindDepSignals",
                  std::format(
                      "local dep instance_id {} not found", sig.instance_id));
            }
            const auto& obs = dep_inst->observability;
            if (obs.layout == nullptr ||
                sig.signal.value >= obs.layout->slot_meta.size()) {
              throw common::InternalError(
                  "Engine::ValidateRebindDepSignals",
                  std::format(
                      "local dep signal {} out of range (layout slot_meta "
                      "size {})",
                      sig.signal.value,
                      obs.layout != nullptr ? obs.layout->slot_meta.size()
                                            : 0));
            }
            if (sig.signal.value >= obs.local_signal_subs.size()) {
              throw common::InternalError(
                  "Engine::ValidateRebindDepSignals",
                  std::format(
                      "local dep signal {} >= local_signal_subs size {}",
                      sig.signal.value, obs.local_signal_subs.size()));
            }
          }
        },
        dep);
  }
}

// R5: Thin boundary wrapper -- dispatch to domain-specific rebind.
void Engine::SubscribeRebind(
    ProcessHandle handle, uint32_t edge_target_id, SignalRef target_signal,
    SubKind target_kind, uint32_t target_index, uint8_t target_edge_group,
    EdgeBucket target_edge_bucket, std::span<const IndexPlanOp> plan,
    BitTargetMapping mapping, std::span<const SignalRef> dep_signals) {
  std::visit(
      [&](const auto& target) {
        using T = std::decay_t<decltype(target)>;
        if constexpr (std::is_same_v<T, GlobalSignalId>) {
          SubscribeGlobalRebind(
              handle, edge_target_id, target, target_kind, target_index,
              target_edge_group, target_edge_bucket, plan, mapping,
              dep_signals);
        } else {
          SubscribeLocalRebind(
              handle, edge_target_id, target, target_kind, target_index,
              target_edge_group, target_edge_bucket, plan, mapping,
              dep_signals);
        }
      },
      target_signal);
}

// Shared dep-watcher installation. Called from both domain-specific rebind
// functions after the target cold entry and edge_target_id are established.
void Engine::InstallRebindDepWatchers(
    ProcessHandle handle, uint32_t edge_target_id,
    std::span<const SignalRef> dep_signals) {
  auto& proc_state = process_states_[handle.process_id];
  for (const auto& dep : dep_signals) {
    std::visit(
        [&](const auto& sig) {
          using T = std::decay_t<decltype(sig)>;
          SlotSubscriptions* dep_subs = nullptr;
          const uint8_t* dep_base = nullptr;
          uint32_t dep_total_bytes = 0;

          RuntimeInstance* dep_inst = nullptr;
          if constexpr (std::is_same_v<T, LocalSignalRef>) {
            dep_inst = &GetInstanceMut(sig.instance_id);
            auto& obs = dep_inst->observability;
            const auto& imeta = obs.layout->slot_meta[sig.signal.value];
            dep_subs = &obs.local_signal_subs[sig.signal.value];
            dep_total_bytes = imeta.total_bytes;
            dep_base = ResolveInstanceSlotBase(*dep_inst, sig.signal);
          } else {
            const auto& meta = slot_meta_registry_.Get(sig.value);
            dep_subs = &signal_subs_[sig.value];
            dep_total_bytes = meta.total_bytes;
            dep_base =
                ResolveSlotBase(meta, design_state_base_, const_instances_);
          }

          auto watcher_idx =
              static_cast<uint32_t>(dep_subs->rebind_subs.size());

          uint32_t watcher_cold = AllocWatcherCold();
          auto& wcold = watcher_cold_pool_[watcher_cold];
          wcold.edge_target_id = edge_target_id;
          wcold.snapshot.resize(dep_total_bytes);
          std::memcpy(wcold.snapshot.data(), dep_base, dep_total_bytes);

          RebindWatcherSub watcher{};
          watcher.process_id = handle.process_id;
          watcher.byte_offset = 0;
          watcher.byte_size = dep_total_bytes;
          watcher.cold_idx = watcher_cold;
          watcher.flags = kSubActive;

          if constexpr (std::is_same_v<T, LocalSignalRef>) {
            watcher.process_sub_idx =
                static_cast<uint32_t>(proc_state.local_sub_refs.size());
            dep_subs->rebind_subs.push_back(watcher);
            proc_state.local_sub_refs.push_back(
                LocalSubRef{
                    .instance = dep_inst,
                    .signal = sig.signal,
                    .index = watcher_idx,
                    .kind = SubKind::kRebindWatcher});
            UpdateLocalObserverFlag(*dep_inst, sig.signal);
          } else {
            watcher.process_sub_idx =
                static_cast<uint32_t>(proc_state.global_sub_refs.size());
            dep_subs->rebind_subs.push_back(watcher);
            proc_state.global_sub_refs.push_back(
                GlobalSubRef{
                    .signal = sig,
                    .index = watcher_idx,
                    .kind = SubKind::kRebindWatcher});
            UpdateGlobalObserverFlag(sig);
          }
          ++proc_state.subscription_count;
          ++live_subscription_count_;
        },
        dep);
  }
}

void Engine::SubscribeGlobalRebind(
    ProcessHandle handle, uint32_t edge_target_id, GlobalSignalId target_signal,
    SubKind target_kind, uint32_t target_index, uint8_t target_edge_group,
    EdgeBucket target_edge_bucket, std::span<const IndexPlanOp> plan,
    BitTargetMapping mapping, std::span<const SignalRef> dep_signals) {
  if (finished_) return;
  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::SubscribeGlobalRebind",
        "SubscribeRebind before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::SubscribeGlobalRebind", "SubscribeRebind before InitSlotMeta");
  }
  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeGlobalRebind",
        std::format(
            "process_id {} exceeds num_processes {}", handle.process_id,
            num_processes_));
  }
  if (target_kind != SubKind::kEdge && target_kind != SubKind::kContainer) {
    throw common::InternalError(
        "Engine::SubscribeGlobalRebind",
        std::format(
            "rebind target kind {} not supported",
            static_cast<int>(target_kind)));
  }

  auto& target_subs = signal_subs_[target_signal.value];
  if (target_kind == SubKind::kEdge) {
    if (target_edge_group >= target_subs.edge_groups.size()) {
      throw common::InternalError(
          "Engine::SubscribeGlobalRebind",
          std::format(
              "target_edge_group {} >= edge_groups size {}", target_edge_group,
              target_subs.edge_groups.size()));
    }
    auto& g = target_subs.edge_groups[target_edge_group];
    auto& tvec = (target_edge_bucket == EdgeBucket::kPosedge) ? g.posedge_subs
                                                              : g.negedge_subs;
    if (target_index >= tvec.size()) {
      throw common::InternalError(
          "Engine::SubscribeGlobalRebind",
          std::format(
              "target_index {} >= edge bucket size {}", target_index,
              tvec.size()));
    }
  } else if (target_index >= target_subs.container_subs.size()) {
    throw common::InternalError(
        "Engine::SubscribeGlobalRebind",
        std::format(
            "target_index {} >= container_subs size {}", target_index,
            target_subs.container_subs.size()));
  }

  ValidateRebindDepSignals(dep_signals);

  auto& proc_state = process_states_[handle.process_id];
  auto total_after = proc_state.subscription_count + dep_signals.size();
  auto global_after = live_subscription_count_ + dep_signals.size();
  if (max_total_subscriptions_ > 0 && global_after > max_total_subscriptions_) {
    TerminateWithResourceError(
        "global subscription limit exceeded (rebind batch)",
        live_subscription_count_, max_total_subscriptions_);
    return;
  }
  if (max_subscriptions_per_process_ > 0 &&
      total_after > max_subscriptions_per_process_) {
    TerminateWithResourceError(
        "per-process subscription limit exceeded (rebind batch)",
        proc_state.subscription_count, max_subscriptions_per_process_);
    return;
  }

  auto plan_start = static_cast<uint32_t>(proc_state.plan_pool.ops.size());
  proc_state.plan_pool.ops.insert(
      proc_state.plan_pool.ops.end(), plan.begin(), plan.end());
  IndexPlanRef plan_ref = {
      .start = plan_start, .count = static_cast<uint16_t>(plan.size())};

  const auto& tmeta = slot_meta_registry_.Get(target_signal.value);
  auto target_storage = std::span(
      ResolveSlotBase(tmeta, design_state_base_, const_instances_),
      tmeta.total_bytes);

  if (target_kind == SubKind::kEdge) {
    auto& group = target_subs.edge_groups[target_edge_group];
    auto& tvec = (target_edge_bucket == EdgeBucket::kPosedge)
                     ? group.posedge_subs
                     : group.negedge_subs;
    auto& esub = tvec[target_index];
    if (esub.cold_idx == UINT32_MAX) {
      esub.cold_idx = AllocEdgeCold();
      esub.flags |= kSubHasCold;
      auto& new_cold = edge_cold_pool_[esub.cold_idx];
      new_cold.edge_last_byte = target_storage[group.byte_offset];
      new_cold.has_edge_last_byte = true;
    }
    auto& ecold = edge_cold_pool_[esub.cold_idx];
    ecold.plan_ref = plan_ref;
    ecold.rebind_mapping = mapping;
    if (edge_target_id == UINT32_MAX) {
      edge_target_id = AllocGlobalEdgeTarget(
          GlobalEdgeTargetHandle{
              .signal = target_signal,
              .kind = SubKind::kEdge,
              .edge_bucket = target_edge_bucket,
              .edge_group = target_edge_group,
              .index = target_index});
      ecold.edge_target_id = edge_target_id;
    }
  } else {
    auto& csub = target_subs.container_subs[target_index];
    auto& ccold = container_cold_pool_[csub.cold_idx];
    ccold.plan_ref = plan_ref;
    ccold.rebind_mapping = mapping;
    if (edge_target_id == UINT32_MAX) {
      edge_target_id = AllocGlobalEdgeTarget(
          GlobalEdgeTargetHandle{
              .signal = target_signal,
              .kind = SubKind::kContainer,
              .index = target_index});
      ccold.edge_target_id = edge_target_id;
    }
  }

  InstallRebindDepWatchers(handle, edge_target_id, dep_signals);
  RebindSubscription(edge_target_id);
}

void Engine::SubscribeLocalRebind(
    ProcessHandle handle, uint32_t edge_target_id, LocalSignalRef target_signal,
    SubKind target_kind, uint32_t target_index, uint8_t target_edge_group,
    EdgeBucket target_edge_bucket, std::span<const IndexPlanOp> plan,
    BitTargetMapping mapping, std::span<const SignalRef> dep_signals) {
  if (finished_) return;
  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeLocalRebind",
        std::format(
            "process_id {} exceeds num_processes {}", handle.process_id,
            num_processes_));
  }
  if (target_kind != SubKind::kEdge && target_kind != SubKind::kContainer) {
    throw common::InternalError(
        "Engine::SubscribeLocalRebind",
        std::format(
            "rebind target kind {} not supported",
            static_cast<int>(target_kind)));
  }

  auto& inst = GetInstanceMut(target_signal.instance_id);
  auto& target_subs =
      inst.observability.local_signal_subs[target_signal.signal.value];
  if (target_kind == SubKind::kEdge) {
    if (target_edge_group >= target_subs.edge_groups.size()) {
      throw common::InternalError(
          "Engine::SubscribeLocalRebind",
          std::format(
              "target_edge_group {} >= edge_groups size {}", target_edge_group,
              target_subs.edge_groups.size()));
    }
    auto& g = target_subs.edge_groups[target_edge_group];
    auto& tvec = (target_edge_bucket == EdgeBucket::kPosedge) ? g.posedge_subs
                                                              : g.negedge_subs;
    if (target_index >= tvec.size()) {
      throw common::InternalError(
          "Engine::SubscribeLocalRebind",
          std::format(
              "target_index {} >= edge bucket size {}", target_index,
              tvec.size()));
    }
  } else if (target_index >= target_subs.container_subs.size()) {
    throw common::InternalError(
        "Engine::SubscribeLocalRebind",
        std::format(
            "target_index {} >= container_subs size {}", target_index,
            target_subs.container_subs.size()));
  }

  ValidateRebindDepSignals(dep_signals);

  auto& proc_state = process_states_[handle.process_id];
  auto total_after = proc_state.subscription_count + dep_signals.size();
  auto global_after = live_subscription_count_ + dep_signals.size();
  if (max_total_subscriptions_ > 0 && global_after > max_total_subscriptions_) {
    TerminateWithResourceError(
        "global subscription limit exceeded (rebind batch)",
        live_subscription_count_, max_total_subscriptions_);
    return;
  }
  if (max_subscriptions_per_process_ > 0 &&
      total_after > max_subscriptions_per_process_) {
    TerminateWithResourceError(
        "per-process subscription limit exceeded (rebind batch)",
        proc_state.subscription_count, max_subscriptions_per_process_);
    return;
  }

  auto plan_start = static_cast<uint32_t>(proc_state.plan_pool.ops.size());
  proc_state.plan_pool.ops.insert(
      proc_state.plan_pool.ops.end(), plan.begin(), plan.end());
  IndexPlanRef plan_ref = {
      .start = plan_start, .count = static_cast<uint16_t>(plan.size())};

  const auto& imeta =
      inst.observability.layout->slot_meta[target_signal.signal.value];
  auto target_storage = std::span(
      ResolveInstanceSlotBase(inst, target_signal.signal), imeta.total_bytes);

  if (target_kind == SubKind::kEdge) {
    auto& group = target_subs.edge_groups[target_edge_group];
    auto& tvec = (target_edge_bucket == EdgeBucket::kPosedge)
                     ? group.posedge_subs
                     : group.negedge_subs;
    auto& esub = tvec[target_index];
    if (esub.cold_idx == UINT32_MAX) {
      esub.cold_idx = AllocEdgeCold();
      esub.flags |= kSubHasCold;
      auto& new_cold = edge_cold_pool_[esub.cold_idx];
      new_cold.edge_last_byte = target_storage[group.byte_offset];
      new_cold.has_edge_last_byte = true;
    }
    auto& ecold = edge_cold_pool_[esub.cold_idx];
    ecold.plan_ref = plan_ref;
    ecold.rebind_mapping = mapping;
    if (edge_target_id == UINT32_MAX) {
      edge_target_id = AllocLocalEdgeTarget(
          LocalEdgeTargetHandle{
              .instance = &inst,
              .signal = target_signal.signal,
              .kind = SubKind::kEdge,
              .edge_bucket = target_edge_bucket,
              .edge_group = target_edge_group,
              .index = target_index});
      ecold.edge_target_id = edge_target_id;
    }
  } else {
    auto& csub = target_subs.container_subs[target_index];
    auto& ccold = container_cold_pool_[csub.cold_idx];
    ccold.plan_ref = plan_ref;
    ccold.rebind_mapping = mapping;
    if (edge_target_id == UINT32_MAX) {
      edge_target_id = AllocLocalEdgeTarget(
          LocalEdgeTargetHandle{
              .instance = &inst,
              .signal = target_signal.signal,
              .kind = SubKind::kContainer,
              .index = target_index});
      ccold.edge_target_id = edge_target_id;
    }
  }

  InstallRebindDepWatchers(handle, edge_target_id, dep_signals);
  RebindSubscription(edge_target_id);
}

}  // namespace lyra::runtime
