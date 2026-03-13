#include <cstdint>
#include <cstring>
#include <span>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/slot_meta.hpp"

namespace lyra::runtime {

namespace {

// Fast-fail validation: notify_base_ptr must be the true slot root,
// not a projected sub-field pointer. Catches codegen bugs where
// write_ptr is passed instead of the slot base.
void ValidateSlotRootPointer(
    const void* ptr, uint32_t slot_id, const SlotMetaRegistry& registry,
    const void* design_state_base, const char* caller) {
  if (!registry.IsPopulated() || design_state_base == nullptr) {
    return;
  }
  auto state = std::span(
      static_cast<const uint8_t*>(design_state_base), registry.MaxExtent());
  const auto* expected = &state[registry.Get(slot_id).base_off];
  if (static_cast<const uint8_t*>(ptr) != expected) {
    throw common::InternalError(
        caller, "pointer does not match slot root address");
  }
}

// Apply a full-overwrite NBA entry: direct compare/copy.
// Returns true if the target was changed.
auto ApplyFullOverwriteNba(const NbaEntry& entry, std::span<uint8_t> target)
    -> bool {
  if (entry.mask.Size() != 0) {
    throw common::InternalError(
        "ApplyFullOverwriteNba", "mask must be empty for full overwrite");
  }
  bool changed =
      std::memcmp(target.data(), entry.value.Data(), entry.byte_size) != 0;
  if (changed) {
    std::memcpy(target.data(), entry.value.Data(), entry.byte_size);
  }
  return changed;
}

// Apply a masked-merge NBA entry: per-byte (old & ~mask) | (new & mask).
// Returns true if any byte was changed.
auto ApplyMaskedMergeNba(const NbaEntry& entry, std::span<uint8_t> target)
    -> bool {
  if (entry.mask.Size() != entry.byte_size) {
    throw common::InternalError("ApplyMaskedMergeNba", "mask size mismatch");
  }
  auto val = std::span(entry.value.Data(), entry.byte_size);
  auto mask = std::span(entry.mask.Data(), entry.byte_size);
  bool changed = false;
  for (uint32_t i = 0; i < entry.byte_size; ++i) {
    uint8_t old_byte = target[i];
    uint8_t new_byte = (old_byte & ~mask[i]) | (val[i] & mask[i]);
    if (old_byte != new_byte) {
      changed = true;
    }
    target[i] = new_byte;
  }
  return changed;
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

  // Early-out for full-overwrite NBAs where the value hasn't changed.
  // Skips buffer allocation, queue push, and later memcmp in kNBA region.
  // Only applied to unmasked writes; masked-merge has different semantics
  // (partial byte update) and is rare enough not to warrant the complexity.
  if (mask_ptr == nullptr &&
      std::memcmp(write_ptr, value_ptr, byte_size) == 0) {
    ++stats_.core.nba_elided;
    return;
  }

  ValidateSlotRootPointer(
      notify_base_ptr, notify_slot_id, slot_meta_registry_, design_state_base_,
      "Engine::ScheduleNba");

  NbaEntry entry;
  entry.write_ptr = write_ptr;
  entry.notify_base_ptr = notify_base_ptr;
  entry.byte_size = byte_size;
  entry.notify_slot_id = notify_slot_id;
  entry.value.AssignCopy(value_ptr, byte_size);

  if (mask_ptr != nullptr) {
    entry.mode = NbaWriteMode::kMaskedMerge;
    entry.mask.AssignCopy(mask_ptr, byte_size);
  } else {
    entry.mode = NbaWriteMode::kFullOverwrite;
  }

  nba_queue_.push_back(std::move(entry));
}

void Engine::ExecuteNbaRegion() {
  if (nba_queue_.empty()) {
    return;
  }
  for (const auto& entry : nba_queue_) {
    if (entry.value.Size() != entry.byte_size) {
      throw common::InternalError(
          "Engine::ExecuteRegion(kNBA)", "value size mismatch");
    }
    auto target =
        std::span(static_cast<uint8_t*>(entry.write_ptr), entry.byte_size);
    bool changed = false;

    switch (entry.mode) {
      case NbaWriteMode::kFullOverwrite:
        changed = ApplyFullOverwriteNba(entry, target);
        break;
      case NbaWriteMode::kMaskedMerge:
        changed = ApplyMaskedMergeNba(entry, target);
        break;
    }

    if (changed) {
      ++stats_.core.nba_changed;
      if (static_cast<const uint8_t*>(entry.write_ptr) <
          static_cast<const uint8_t*>(entry.notify_base_ptr)) {
        throw common::InternalError(
            "Engine::ExecuteRegion(kNBA)", "write_ptr before notify_base_ptr");
      }
      auto diff = static_cast<uint32_t>(
          static_cast<const uint8_t*>(entry.write_ptr) -
          static_cast<const uint8_t*>(entry.notify_base_ptr));
      MarkDirtyRange(entry.notify_slot_id, diff, entry.byte_size);
    }
  }
  nba_queue_.clear();
}

}  // namespace lyra::runtime
