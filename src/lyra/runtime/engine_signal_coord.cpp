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
  auto& obs = inst.observability;
  if (obs.local_has_observers[lid.value] == 0 && !trace_manager_.IsEnabled()) {
    return;
  }
  obs.local_updates.MarkSlotDirty(lid);
  MarkInstanceDeltaDirty(inst);
}

void Engine::MarkLocalSignalDirtyRange(
    RuntimeInstance& inst, LocalSignalId lid, uint32_t byte_off,
    uint32_t byte_size) {
  auto& obs = inst.observability;
  if (obs.local_has_observers[lid.value] == 0 && !trace_manager_.IsEnabled()) {
    return;
  }
  obs.local_updates.MarkDirtyRange(lid, byte_off, byte_size);
  MarkInstanceDeltaDirty(inst);
}

void Engine::MarkLocalSignalDirtyFull(
    RuntimeInstance& inst, LocalSignalId lid) {
  auto& obs = inst.observability;
  if (obs.local_has_observers[lid.value] == 0 && !trace_manager_.IsEnabled()) {
    return;
  }
  obs.local_updates.MarkSlotDirtyFull(lid);
  MarkInstanceDeltaDirty(inst);
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

void Engine::ScheduleNbaCrossInstanceLocal(
    ObjectSignalRef notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* mask_ptr, uint32_t byte_size) {
  if (notify_signal.instance == nullptr ||
      !notify_signal.instance->nba_pending.IsInitialized()) {
    throw common::InternalError(
        "Engine::ScheduleNbaCrossInstanceLocal",
        "notify_signal.instance missing or has no dense index");
  }
  NbaNotifySignal notify{NbaNotifyLocal{
      .instance = notify_signal.instance, .signal = notify_signal.local}};
  ScheduleNba(
      write_ptr, notify_base_ptr, value_ptr, mask_ptr, byte_size, notify);
}

void Engine::ScheduleNbaCanonicalPackedCrossInstanceLocal(
    ObjectSignalRef notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* unk_ptr, uint32_t region_byte_size,
    uint32_t second_region_offset) {
  if (notify_signal.instance == nullptr ||
      !notify_signal.instance->nba_pending.IsInitialized()) {
    throw common::InternalError(
        "Engine::ScheduleNbaCanonicalPackedCrossInstanceLocal",
        "notify_signal.instance missing or has no dense index");
  }
  NbaNotifySignal notify{NbaNotifyLocal{
      .instance = notify_signal.instance, .signal = notify_signal.local}};
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

void Engine::ScheduleNbaGlobal(
    GlobalSignalId notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* mask_ptr, uint32_t byte_size) {
  NbaNotifySignal notify{NbaNotifyGlobal{notify_signal}};
  ScheduleNba(
      write_ptr, notify_base_ptr, value_ptr, mask_ptr, byte_size, notify);
}

void Engine::ScheduleNbaCanonicalPackedGlobal(
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

  // Initialize dirty-instance sparse lists and per-instance dedup flags.
  auto n = instances_.size();
  for (auto* inst : instances_) {
    inst->dedup_state.in_delta_dirty = false;
    inst->dedup_state.in_timeslot_dirty = false;
    inst->dedup_state.in_nba_pending = false;
  }
  delta_dirty_instances_.clear();
  delta_dirty_instances_.reserve(n);
  timeslot_dirty_instances_.clear();
  timeslot_dirty_instances_.reserve(n);
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

void Engine::ClearLocalUpdatesDelta() {
  for (auto* inst : delta_dirty_instances_) {
    inst->observability.local_updates.ClearDelta();
    inst->dedup_state.in_delta_dirty = false;
  }
  delta_dirty_instances_.clear();
}

void Engine::ClearLocalUpdates() {
  // Full timeslot clear dominates delta clear. Clear() calls ClearDelta()
  // internally on each instance, so we only need to iterate the timeslot
  // set. Both sparse lists are reset as a consequence.
  for (auto* inst : timeslot_dirty_instances_) {
    inst->observability.local_updates.Clear();
    inst->dedup_state.in_timeslot_dirty = false;
    inst->dedup_state.in_delta_dirty = false;
  }
  timeslot_dirty_instances_.clear();
  delta_dirty_instances_.clear();
}

}  // namespace lyra::runtime
