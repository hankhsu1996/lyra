#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {

namespace {

// Temporary flat slot-id conversion for current coordination storage.
// All typed engine API methods lower through this until private dense
// coordination indexing replaces flat slot-id vectors.
auto ToFlatSlotId(ObjectSignalRef signal) -> uint32_t {
  return signal.instance->local_signal_coord_base + signal.local.value;
}

auto ToFlatSlotId(GlobalSignalId signal) -> uint32_t {
  return signal.value;
}

}  // namespace

void Engine::MarkDirty(ObjectSignalRef signal) {
  MarkSlotDirty(ToFlatSlotId(signal));
}

void Engine::MarkDirtyRange(
    ObjectSignalRef signal, uint32_t byte_off, uint32_t byte_size) {
  MarkDirtyRange(ToFlatSlotId(signal), byte_off, byte_size);
}

void Engine::MarkDirty(GlobalSignalId signal) {
  MarkSlotDirty(ToFlatSlotId(signal));
}

void Engine::MarkDirtyRange(
    GlobalSignalId signal, uint32_t byte_off, uint32_t byte_size) {
  MarkDirtyRange(ToFlatSlotId(signal), byte_off, byte_size);
}

void Engine::ScheduleNba(
    ObjectSignalRef notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* mask_ptr, uint32_t byte_size) {
  ScheduleNba(
      write_ptr, notify_base_ptr, value_ptr, mask_ptr, byte_size,
      ToFlatSlotId(notify_signal));
}

void Engine::ScheduleNba(
    GlobalSignalId notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* mask_ptr, uint32_t byte_size) {
  ScheduleNba(
      write_ptr, notify_base_ptr, value_ptr, mask_ptr, byte_size,
      ToFlatSlotId(notify_signal));
}

void Engine::ScheduleNbaCanonicalPacked(
    ObjectSignalRef notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* unk_ptr, uint32_t region_byte_size,
    uint32_t second_region_offset) {
  ScheduleNbaCanonicalPacked(
      write_ptr, notify_base_ptr, value_ptr, unk_ptr, region_byte_size,
      second_region_offset, ToFlatSlotId(notify_signal));
}

void Engine::ScheduleNbaCanonicalPacked(
    GlobalSignalId notify_signal, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* unk_ptr, uint32_t region_byte_size,
    uint32_t second_region_offset) {
  ScheduleNbaCanonicalPacked(
      write_ptr, notify_base_ptr, value_ptr, unk_ptr, region_byte_size,
      second_region_offset, ToFlatSlotId(notify_signal));
}

auto Engine::IsTraceObserved(ObjectSignalRef signal) const -> bool {
  return trace_selection_.IsSelected(ToFlatSlotId(signal));
}

auto Engine::IsTraceObserved(GlobalSignalId signal) const -> bool {
  return trace_selection_.IsSelected(ToFlatSlotId(signal));
}

void Engine::AssignDenseCoordinationBases(
    std::span<RuntimeInstance* const> mutable_instances) {
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::AssignDenseCoordinationBases",
        "slot meta registry must be initialized first");
  }

  // Count slots per instance from slot meta registry.
  // Also count package/global slots (not owned by any instance).
  uint32_t total_slots = slot_meta_registry_.Size();
  std::vector<uint32_t> instance_slot_counts(mutable_instances.size(), 0);
  uint32_t global_slot_count = 0;

  for (uint32_t i = 0; i < total_slots; ++i) {
    const auto& meta = slot_meta_registry_.Get(i);
    if (meta.domain == SlotStorageDomain::kDesignGlobal) {
      ++global_slot_count;
    } else {
      if (meta.owner_instance_id < instance_slot_counts.size()) {
        ++instance_slot_counts[meta.owner_instance_id];
      }
    }
  }

  // Assign dense coordination bases: global slots first, then per-instance.
  uint32_t next_base = global_slot_count;
  for (uint32_t i = 0; i < mutable_instances.size(); ++i) {
    mutable_instances[i]->local_signal_coord_base = next_base;
    next_base += instance_slot_counts[i];
  }

  // Validate total matches.
  if (next_base != total_slots) {
    throw common::InternalError(
        "Engine::AssignDenseCoordinationBases",
        std::format(
            "dense coordination base assignment mismatch: computed {} total "
            "but slot meta registry has {}",
            next_base, total_slots));
  }

  // Validate contiguity: for each instance-owned slot, the internal slot id
  // must equal base + local_offset where local offsets are 0, 1, 2, ...
  // in slot-meta order. If this invariant does not hold, the
  // local_signal_coord_base + LocalSignalId arithmetic is wrong.
  std::vector<uint32_t> next_expected(mutable_instances.size());
  for (uint32_t i = 0; i < mutable_instances.size(); ++i) {
    next_expected[i] = mutable_instances[i]->local_signal_coord_base;
  }

  for (uint32_t slot_id = 0; slot_id < total_slots; ++slot_id) {
    const auto& meta = slot_meta_registry_.Get(slot_id);
    if (meta.domain != SlotStorageDomain::kInstanceOwned) continue;

    uint32_t inst = meta.owner_instance_id;
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
              mutable_instances[inst]->local_signal_coord_base));
    }
    ++next_expected[inst];
  }
}

}  // namespace lyra::runtime
