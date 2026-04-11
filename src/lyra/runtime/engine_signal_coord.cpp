#include <algorithm>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {

// --- Canonical local dirty-mark helpers ---
// All local dirty marking routes through these two methods. They
// update the per-instance LocalUpdateSet (authoritative truth) and
// the engine-level dirty-instance sparse indexes (derived acceleration).

void Engine::MarkLocalSignalDirty(RuntimeInstance& inst, LocalSignalId lid) {
  if (inst.observability.local_has_observers[lid.value] == 0 &&
      !trace_manager_.IsEnabled()) {
    return;
  }
  MarkLocalSignalDirty(inst, lid, GetInstanceIndex(inst));
}

void Engine::MarkLocalSignalDirty(
    RuntimeInstance& inst, LocalSignalId lid, uint32_t instance_idx) {
  auto& obs = inst.observability;
  if (obs.local_has_observers[lid.value] == 0 && !trace_manager_.IsEnabled()) {
    return;
  }
  obs.local_updates.MarkSlotDirty(lid);
  MarkInstanceDeltaDirty(instance_idx);
}

void Engine::MarkLocalSignalDirtyRange(
    RuntimeInstance& inst, LocalSignalId lid, uint32_t byte_off,
    uint32_t byte_size) {
  if (inst.observability.local_has_observers[lid.value] == 0 &&
      !trace_manager_.IsEnabled()) {
    return;
  }
  MarkLocalSignalDirtyRange(
      inst, lid, byte_off, byte_size, GetInstanceIndex(inst));
}

void Engine::MarkLocalSignalDirtyRange(
    RuntimeInstance& inst, LocalSignalId lid, uint32_t byte_off,
    uint32_t byte_size, uint32_t instance_idx) {
  auto& obs = inst.observability;
  if (obs.local_has_observers[lid.value] == 0 && !trace_manager_.IsEnabled()) {
    return;
  }
  obs.local_updates.MarkDirtyRange(lid, byte_off, byte_size);
  MarkInstanceDeltaDirty(instance_idx);
}

// --- Instance-owned local paths (source of truth: per-instance containers) ---

void Engine::MarkDirty(ObjectSignalRef signal) {
  if (detailed_stats_enabled_) ++stats_.detailed.dirty_mark_calls;
  MarkLocalSignalDirty(*signal.instance, signal.local);
}

void Engine::MarkDirtyRange(
    ObjectSignalRef signal, uint32_t byte_off, uint32_t byte_size) {
  if (detailed_stats_enabled_) ++stats_.detailed.dirty_mark_calls;
  MarkLocalSignalDirtyRange(
      *signal.instance, signal.local, byte_off, byte_size);
}

void Engine::ScheduleNba(
    ObjectSignalRef notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* mask_ptr, uint32_t byte_size) {
  MarkLocalNbaGeneric(*notify_signal.instance, notify_signal.local);
  NbaNotifySignal notify{NbaNotifyLocal{
      .instance_id = notify_signal.instance->instance_id,
      .signal = notify_signal.local}};
  ScheduleNba(
      write_ptr, notify_base_ptr, value_ptr, mask_ptr, byte_size, notify);
}

void Engine::ScheduleNbaCanonicalPacked(
    ObjectSignalRef notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* unk_ptr, uint32_t region_byte_size,
    uint32_t second_region_offset) {
  MarkLocalNbaGeneric(*notify_signal.instance, notify_signal.local);
  NbaNotifySignal notify{NbaNotifyLocal{
      .instance_id = notify_signal.instance->instance_id,
      .signal = notify_signal.local}};
  ScheduleNbaCanonicalPacked(
      write_ptr, notify_base_ptr, value_ptr, unk_ptr, region_byte_size,
      second_region_offset, notify);
}

auto Engine::IsTraceObserved(ObjectSignalRef signal) const -> bool {
  if (!trace_manager_.IsEnabled()) return false;
  auto& obs = signal.instance->observability;
  if (signal.local.value >= obs.trace_select.size()) return false;
  return obs.trace_select[signal.local.value] != 0;
}

// --- Global paths (truly global-only) ---

void Engine::MarkDirty(GlobalSignalId signal) {
  MarkSlotDirty(signal.value);
}

void Engine::MarkDirtyRange(
    GlobalSignalId signal, uint32_t byte_off, uint32_t byte_size) {
  MarkDirtyRange(signal.value, byte_off, byte_size);
}

void Engine::ScheduleNba(
    GlobalSignalId notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* mask_ptr, uint32_t byte_size) {
  NbaNotifySignal notify{NbaNotifyGlobal{notify_signal}};
  ScheduleNba(
      write_ptr, notify_base_ptr, value_ptr, mask_ptr, byte_size, notify);
}

void Engine::ScheduleNbaCanonicalPacked(
    GlobalSignalId notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* unk_ptr, uint32_t region_byte_size,
    uint32_t second_region_offset) {
  NbaNotifySignal notify{NbaNotifyGlobal{notify_signal}};
  ScheduleNbaCanonicalPacked(
      write_ptr, notify_base_ptr, value_ptr, unk_ptr, region_byte_size,
      second_region_offset, notify);
}

auto Engine::IsTraceObserved(GlobalSignalId signal) const -> bool {
  if (!trace_manager_.IsEnabled()) return false;
  return trace_selection_.IsSelected(signal.value);
}

// --- R5: Mutable instance span and local update lifecycle ---

void Engine::SetInstances(std::span<const RuntimeInstance* const> instances) {
  // Build canonical mutable list. The ABI passes const pointers, but the
  // underlying RuntimeInstance objects are non-const (owned by
  // ConstructionResult). The const_cast is safe because observability
  // fields are runtime-only state not part of the codegen binary contract.
  instance_ptrs_.resize(instances.size());
  for (size_t i = 0; i < instances.size(); ++i) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-const-cast)
    instance_ptrs_[i] = const_cast<RuntimeInstance*>(instances[i]);
  }
  instances_ = instance_ptrs_;

  // Const view for read-only APIs.
  const_instance_ptrs_.assign(instances.begin(), instances.end());
  const_instances_ = const_instance_ptrs_;

  // Wire trace resolver keyed by RuntimeInstance::instance_id.
  instance_trace_resolver_.Build(instances_);

  // Build reverse-lookup table: instance_id.value -> index in instances_[].
  // Dense table with sparsity bound to prevent pathological allocation.
  instance_to_idx_.clear();
  if (!instances_.empty()) {
    uint32_t max_id = 0;
    for (auto* inst : instances_) {
      max_id = std::max(max_id, inst->instance_id.value);
    }
    auto count = static_cast<uint32_t>(instances_.size());
    if (max_id > count * 4 + 1024) {
      throw common::InternalError(
          "Engine::SetInstances",
          std::format(
              "instance_id space too sparse for dense reverse lookup: "
              "max_id={} count={}",
              max_id, count));
    }
    instance_to_idx_.assign(static_cast<size_t>(max_id) + 1, UINT32_MAX);
    for (uint32_t i = 0; i < count; ++i) {
      auto raw = instances_[i]->instance_id.value;
      if (instance_to_idx_[raw] != UINT32_MAX) {
        throw common::InternalError(
            "Engine::SetInstances",
            std::format(
                "duplicate instance_id {}", instances_[i]->instance_id));
      }
      instance_to_idx_[raw] = i;
    }
  }

  // Initialize dirty-instance sparse indexes.
  auto n = instances_.size();
  in_delta_dirty_.assign(n, 0);
  in_timeslot_dirty_.assign(n, 0);
  delta_dirty_instances_.clear();
  delta_dirty_instances_.reserve(n);
  timeslot_dirty_instances_.clear();
  timeslot_dirty_instances_.reserve(n);

  // Initialize deferred-NBA pending sparse index.
  in_nba_pending_.assign(n, 0);
  nba_pending_instances_.clear();
  nba_pending_instances_.reserve(n);
}

void InstanceIdTraceResolver::Build(
    std::span<RuntimeInstance* const> instances) {
  lookup_.clear();
  lookup_mut_.clear();

  if (instances.empty()) return;

  uint32_t max_id = 0;
  for (auto* inst : instances) {
    if (inst == nullptr) {
      throw common::InternalError(
          "InstanceIdTraceResolver::Build", "null instance in instance list");
    }
    max_id = std::max(max_id, inst->instance_id.value);
  }

  auto table_size = static_cast<size_t>(max_id) + 1;
  lookup_.assign(table_size, nullptr);
  lookup_mut_.assign(table_size, nullptr);

  for (auto* inst : instances) {
    auto id = inst->instance_id.value;
    if (lookup_[id] != nullptr) {
      throw common::InternalError(
          "InstanceIdTraceResolver::Build",
          std::format("duplicate instance_id {}", inst->instance_id));
    }
    lookup_[id] = inst;
    lookup_mut_[id] = inst;
  }
}

auto Engine::GetInstanceIndex(InstanceId id) const -> uint32_t {
  if (id.value >= instance_to_idx_.size() ||
      instance_to_idx_[id.value] == UINT32_MAX) {
    throw common::InternalError(
        "Engine::GetInstanceIndex",
        std::format("instance_id {} not in reverse lookup", id));
  }
  return instance_to_idx_[id.value];
}

auto Engine::GetInstanceIndex(const RuntimeInstance& inst) const -> uint32_t {
  return GetInstanceIndex(inst.instance_id);
}

void Engine::ClearLocalUpdatesDelta() {
  for (uint32_t idx : delta_dirty_instances_) {
    instances_[idx]->observability.local_updates.ClearDelta();
    in_delta_dirty_[idx] = 0;
  }
  delta_dirty_instances_.clear();
}

void Engine::ClearLocalUpdates() {
  // Full timeslot clear dominates delta clear. Clear() calls ClearDelta()
  // internally on each instance, so we only need to iterate the timeslot
  // set. Both sparse indexes are reset as a consequence.
  for (uint32_t idx : timeslot_dirty_instances_) {
    instances_[idx]->observability.local_updates.Clear();
    in_timeslot_dirty_[idx] = 0;
    in_delta_dirty_[idx] = 0;
  }
  timeslot_dirty_instances_.clear();
  delta_dirty_instances_.clear();
}

}  // namespace lyra::runtime
