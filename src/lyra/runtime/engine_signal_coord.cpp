#include <algorithm>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {

// --- Instance-owned local paths (source of truth: per-instance containers) ---

void Engine::MarkDirty(ObjectSignalRef signal) {
  if (detailed_stats_enabled_) ++stats_.detailed.dirty_mark_calls;
  signal.instance->observability.local_updates.MarkSlotDirty(signal.local);
}

void Engine::MarkDirtyRange(
    ObjectSignalRef signal, uint32_t byte_off, uint32_t byte_size) {
  if (detailed_stats_enabled_) ++stats_.detailed.dirty_mark_calls;
  signal.instance->observability.local_updates.MarkDirtyRange(
      signal.local, byte_off, byte_size);
}

void Engine::ScheduleNba(
    ObjectSignalRef notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* mask_ptr, uint32_t byte_size) {
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
  NbaNotifySignal notify{NbaNotifyLocal{
      .instance_id = notify_signal.instance->instance_id,
      .signal = notify_signal.local}};
  ScheduleNbaCanonicalPacked(
      write_ptr, notify_base_ptr, value_ptr, unk_ptr, region_byte_size,
      second_region_offset, notify);
}

auto Engine::IsTraceObserved(ObjectSignalRef signal) -> bool {
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
  return trace_selection_.IsSelected(signal.value);
}

// --- Dense coordination (legacy, pre-R5) ---

void Engine::AssignDenseCoordinationBases(
    std::span<RuntimeInstance* const> mutable_instances) {
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::AssignDenseCoordinationBases",
        "slot meta registry must be initialized first");
  }

  uint32_t total_slots = slot_meta_registry_.Size();
  std::vector<uint32_t> instance_slot_counts(mutable_instances.size(), 0);
  uint32_t global_slot_count = 0;

  for (uint32_t i = 0; i < total_slots; ++i) {
    const auto& meta = slot_meta_registry_.Get(i);
    if (meta.domain == SlotStorageDomain::kDesignGlobal) {
      ++global_slot_count;
    } else {
      if (meta.owner_instance_id.value < instance_slot_counts.size()) {
        ++instance_slot_counts[meta.owner_instance_id.value];
      }
    }
  }

  uint32_t next_base = global_slot_count;
  for (uint32_t i = 0; i < mutable_instances.size(); ++i) {
    mutable_instances[i]->observability.flat_coord_base = next_base;
    next_base += instance_slot_counts[i];
  }

  if (next_base != total_slots) {
    throw common::InternalError(
        "Engine::AssignDenseCoordinationBases",
        std::format(
            "dense coordination base assignment mismatch: computed {} total "
            "but slot meta registry has {}",
            next_base, total_slots));
  }

  std::vector<uint32_t> next_expected(mutable_instances.size());
  for (uint32_t i = 0; i < mutable_instances.size(); ++i) {
    next_expected[i] = mutable_instances[i]->observability.flat_coord_base;
  }

  for (uint32_t slot_id = 0; slot_id < total_slots; ++slot_id) {
    const auto& meta = slot_meta_registry_.Get(slot_id);
    if (meta.domain != SlotStorageDomain::kInstanceOwned) continue;

    auto inst = meta.owner_instance_id.value;
    if (inst >= mutable_instances.size()) {
      throw common::InternalError(
          "Engine::AssignDenseCoordinationBases",
          std::format(
              "slot {} owner_instance_id {} out of range (have {} instances)",
              slot_id, inst, mutable_instances.size()));
    }

    if (slot_id != next_expected[inst]) {
      throw common::InternalError(
          "Engine::AssignDenseCoordinationBases",
          std::format(
              "instance {} slot ordering is not contiguous/local-id-ordered: "
              "expected internal slot {}, got {} (base={})",
              inst, next_expected[inst], slot_id,
              mutable_instances[inst]->observability.flat_coord_base));
    }
    ++next_expected[inst];
  }
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
  for (auto* inst : instances_) {
    if (inst->observability.local_signal_count > 0) {
      inst->observability.local_updates.ClearDelta();
    }
  }
}

void Engine::ClearLocalUpdates() {
  for (auto* inst : instances_) {
    if (inst->observability.local_signal_count > 0) {
      inst->observability.local_updates.Clear();
    }
  }
}

}  // namespace lyra::runtime
