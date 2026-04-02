#include <cstdint>
#include <cstring>
#include <span>
#include <variant>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/slot_meta.hpp"

namespace lyra::runtime {

namespace {

void ValidateSlotRootPointer(
    const void* ptr, uint32_t slot_id, const SlotMetaRegistry& registry,
    const void* design_state_base,
    std::span<const RuntimeInstance* const> instances, const char* caller) {
  if (!registry.IsPopulated() || design_state_base == nullptr) {
    return;
  }
  const auto& meta = registry.Get(slot_id);
  const auto* expected = ResolveSlotBase(meta, design_state_base, instances);
  if (static_cast<const uint8_t*>(ptr) != expected) {
    // Stopgap for direct-array-root owned-container rollout: the runtime
    // registry does not yet expose owned-container root identity, so we
    // use kAggregate as a proxy. This is fragile -- it may mask unrelated
    // aggregate-slot bugs. Non-aggregate kinds still fail hard on mismatch.
    // TODO(hankhsu): Add explicit owned-container kind to SlotMetaRegistry
    // so codegen and runtime speak the same classification language.
    if (registry.Get(slot_id).kind == SlotStorageKind::kAggregate) {
      return;
    }
    throw common::InternalError(
        caller, "pointer does not match slot root address");
  }
}

}  // namespace

void Engine::ScheduleNba(
    void* write_ptr, const void* notify_base_ptr, const void* value_ptr,
    const void* mask_ptr, uint32_t byte_size, uint32_t notify_slot_id) {
  ++stats_.core.nba_entries;

  if (write_ptr == nullptr || notify_base_ptr == nullptr ||
      value_ptr == nullptr || byte_size == 0) {
    throw common::InternalError(
        "Engine::ScheduleNba", "null pointer or zero byte_size");
  }

  if (mask_ptr == nullptr &&
      std::memcmp(write_ptr, value_ptr, byte_size) == 0) {
    ++stats_.core.nba_elided;
    return;
  }

  ValidateSlotRootPointer(
      notify_base_ptr, notify_slot_id, slot_meta_registry_, design_state_base_,
      const_instances_, "Engine::ScheduleNba");

  NbaEntry entry;
  entry.write_ptr = write_ptr;
  entry.notify_base_ptr = notify_base_ptr;
  entry.notify_slot_id = notify_slot_id;

  if (mask_ptr != nullptr) {
    NbaMaskedMerge p;
    p.byte_size = byte_size;
    p.value.AssignCopy(value_ptr, byte_size);
    p.mask.AssignCopy(mask_ptr, byte_size);
    entry.payload = std::move(p);
  } else {
    NbaFullOverwrite p;
    p.byte_size = byte_size;
    p.value.AssignCopy(value_ptr, byte_size);
    entry.payload = std::move(p);
  }

  nba_queue_.push_back(std::move(entry));
}

void Engine::ScheduleNbaCanonicalPacked(
    void* write_ptr, const void* notify_base_ptr, const void* value_ptr,
    const void* unk_ptr, uint32_t region_byte_size,
    uint32_t second_region_offset, uint32_t notify_slot_id) {
  ++stats_.core.nba_entries;

  if (write_ptr == nullptr || notify_base_ptr == nullptr ||
      value_ptr == nullptr || unk_ptr == nullptr || region_byte_size == 0) {
    throw common::InternalError(
        "Engine::ScheduleNbaCanonicalPacked",
        "null pointer or zero region_byte_size");
  }

  ValidateSlotRootPointer(
      notify_base_ptr, notify_slot_id, slot_meta_registry_, design_state_base_,
      const_instances_, "Engine::ScheduleNbaCanonicalPacked");

  NbaEntry entry;
  entry.write_ptr = write_ptr;
  entry.notify_base_ptr = notify_base_ptr;
  entry.notify_slot_id = notify_slot_id;

  NbaCanonicalPackedTwoPlane p;
  p.region_byte_size = region_byte_size;
  p.second_region_offset = second_region_offset;
  p.value.AssignCopy(value_ptr, region_byte_size);
  p.unk.AssignCopy(unk_ptr, region_byte_size);
  entry.payload = std::move(p);

  nba_queue_.push_back(std::move(entry));
}

// Per-mode NBA apply result: which dirty ranges to mark.
struct NbaDirtyResult {
  bool changed = false;
  // For single-range modes (full overwrite, masked merge):
  uint32_t dirty_size = 0;
  // For two-plane mode: separate tracking per plane.
  bool val_changed = false;
  bool unk_changed = false;
  uint32_t region_byte_size = 0;
  uint32_t second_region_offset = 0;
};

auto ApplyNba(const NbaFullOverwrite& p, void* write_ptr) -> NbaDirtyResult {
  auto target = std::span(static_cast<uint8_t*>(write_ptr), p.byte_size);
  bool changed = std::memcmp(target.data(), p.value.Data(), p.byte_size) != 0;
  if (changed) {
    std::memcpy(target.data(), p.value.Data(), p.byte_size);
  }
  return {.changed = changed, .dirty_size = p.byte_size};
}

auto ApplyNba(const NbaMaskedMerge& p, void* write_ptr) -> NbaDirtyResult {
  auto target = std::span(static_cast<uint8_t*>(write_ptr), p.byte_size);
  auto val = std::span(p.value.Data(), p.byte_size);
  auto mask = std::span(p.mask.Data(), p.byte_size);
  bool changed = false;
  for (uint32_t i = 0; i < p.byte_size; ++i) {
    uint8_t old_byte = target[i];
    uint8_t new_byte = (old_byte & ~mask[i]) | (val[i] & mask[i]);
    if (old_byte != new_byte) {
      changed = true;
    }
    target[i] = new_byte;
  }
  return {.changed = changed, .dirty_size = p.byte_size};
}

auto ApplyNba(const NbaCanonicalPackedTwoPlane& p, void* write_ptr)
    -> NbaDirtyResult {
  auto base = std::span(
      static_cast<uint8_t*>(write_ptr),
      p.second_region_offset + p.region_byte_size);
  uint32_t n = p.region_byte_size;

  auto val_region = base.subspan(0, n);
  bool val_changed = std::memcmp(val_region.data(), p.value.Data(), n) != 0;
  if (val_changed) {
    std::memcpy(val_region.data(), p.value.Data(), n);
  }

  auto unk_region = base.subspan(p.second_region_offset, n);
  bool unk_changed = std::memcmp(unk_region.data(), p.unk.Data(), n) != 0;
  if (unk_changed) {
    std::memcpy(unk_region.data(), p.unk.Data(), n);
  }

  return {
      .changed = val_changed || unk_changed,
      .val_changed = val_changed,
      .unk_changed = unk_changed,
      .region_byte_size = n,
      .second_region_offset = p.second_region_offset};
}

void Engine::ExecuteNbaRegion() {
  if (nba_queue_.empty()) {
    return;
  }
  for (const auto& entry : nba_queue_) {
    if (static_cast<const uint8_t*>(entry.write_ptr) <
        static_cast<const uint8_t*>(entry.notify_base_ptr)) {
      throw common::InternalError(
          "Engine::ExecuteRegion(kNBA)", "write_ptr before notify_base_ptr");
    }
    auto diff = static_cast<uint32_t>(
        static_cast<const uint8_t*>(entry.write_ptr) -
        static_cast<const uint8_t*>(entry.notify_base_ptr));

    auto result = std::visit(
        [&](const auto& p) { return ApplyNba(p, entry.write_ptr); },
        entry.payload);

    if (!result.changed) continue;
    ++stats_.core.nba_changed;

    // Dirty-mark dispatch: single-range for full-overwrite and masked-merge,
    // precise two-range for canonical packed two-plane.
    if (std::holds_alternative<NbaCanonicalPackedTwoPlane>(entry.payload)) {
      if (result.val_changed) {
        MarkDirtyRange(entry.notify_slot_id, diff, result.region_byte_size);
      }
      if (result.unk_changed) {
        MarkDirtyRange(
            entry.notify_slot_id, diff + result.second_region_offset,
            result.region_byte_size);
      }
    } else {
      MarkDirtyRange(entry.notify_slot_id, diff, result.dirty_size);
    }
  }
  nba_queue_.clear();
}

}  // namespace lyra::runtime
