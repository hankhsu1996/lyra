#include "lyra/runtime/trace_flush.hpp"

#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/string.hpp"
#include "lyra/runtime/trace_selection.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"
#include "lyra/runtime/update_set.hpp"
#include "lyra/trace/trace_event.hpp"
#include "lyra/trace/trace_manager.hpp"

namespace lyra::runtime {

namespace {

// Build a TraceValue snapshot for a single slot based on its storage kind.
// Centralizes "how to snapshot from DesignState memory using SlotMetaRegistry".
// Mutation sites never construct TraceValue directly; they just MarkSlotDirty.
//
// Args:
//   slot_id: slot identifier (for error messages)
//   meta: slot metadata from registry
//   slot_base: pointer to slot data (design_state_base + meta.base_off)
//
// Returns: TraceValue representing the current value of the slot.
auto SnapshotSlotValue(
    uint32_t slot_id, const SlotMeta& meta, const uint8_t* slot_base)
    -> trace::TraceValue {
  switch (meta.kind) {
    case SlotStorageKind::kPacked2:
    case SlotStorageKind::kAggregate: {
      auto data = std::span(slot_base, meta.total_bytes);
      return trace::PackedSnapshot{
          .byte_size = meta.total_bytes,
          .bytes = std::vector<uint8_t>(data.begin(), data.end())};
    }

    case SlotStorageKind::kPacked4: {
      // Validate plane bounds (catches ABI/codegen bugs)
      if (meta.planes.value_off + meta.planes.value_bytes > meta.total_bytes) {
        throw common::InternalError(
            "SnapshotSlotValue",
            std::format(
                "slot {} kPacked4: value plane exceeds total_bytes", slot_id));
      }
      if (meta.planes.unk_off + meta.planes.unk_bytes > meta.total_bytes) {
        throw common::InternalError(
            "SnapshotSlotValue",
            std::format(
                "slot {} kPacked4: unk plane exceeds total_bytes", slot_id));
      }
      if (meta.planes.value_off == meta.planes.unk_off) {
        throw common::InternalError(
            "SnapshotSlotValue",
            std::format("slot {} kPacked4: value_off == unk_off", slot_id));
      }
      auto data = std::span(slot_base, meta.total_bytes);
      return trace::PackedSnapshot{
          .byte_size = meta.total_bytes,
          .bytes = std::vector<uint8_t>(data.begin(), data.end())};
    }

    case SlotStorageKind::kString: {
      // Validate handle size (catches ABI bugs)
      if (meta.total_bytes != sizeof(void*)) {
        throw common::InternalError(
            "SnapshotSlotValue",
            std::format(
                "slot {} has kind=string but total_bytes={} (expected {})",
                slot_id, meta.total_bytes, sizeof(void*)));
      }
      // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
      auto* handle = *reinterpret_cast<void* const*>(slot_base);
      if (handle == nullptr) {
        return std::string{};
      }
      return std::string(LyraStringAsView(handle));
    }

    case SlotStorageKind::kHandle: {
      // Validate handle size (catches ABI bugs)
      if (meta.total_bytes != sizeof(void*)) {
        throw common::InternalError(
            "SnapshotSlotValue",
            std::format(
                "slot {} has kind=handle but total_bytes={} (expected {})",
                slot_id, meta.total_bytes, sizeof(void*)));
      }
      // Identity tracking: snapshot exactly sizeof(void*) bytes.
      constexpr auto kPtrSize = static_cast<uint32_t>(sizeof(void*));
      auto data = std::span(slot_base, kPtrSize);
      return trace::PackedSnapshot{
          .byte_size = kPtrSize,
          .bytes = std::vector<uint8_t>(data.begin(), data.end())};
    }
  }
  throw common::InternalError(
      "SnapshotSlotValue",
      std::format("unknown SlotStorageKind: {}", static_cast<int>(meta.kind)));
}

// Snapshot a slot's storage as a TraceValue.
// owner_slot_id is used only for diagnostic messages; the actual storage
// bytes are read from slot_base using meta.
auto SnapshotOwnerStorage(
    uint32_t owner_slot_id, const SlotMeta& meta, const uint8_t* slot_base)
    -> trace::TraceValue {
  return SnapshotSlotValue(owner_slot_id, meta, slot_base);
}

}  // namespace

void FlushDirtySlotsToTrace(
    trace::TraceManager& trace, const SlotMetaRegistry& slot_registry,
    const TraceSignalMetaRegistry& trace_registry,
    const void* design_state_base, const UpdateSet& updates,
    const TraceSelectionRegistry& selection) {
  bool has_ownership = slot_registry.IsPopulated();

  // Both registries must be populated together or neither.
  if (has_ownership != trace_registry.IsPopulated()) {
    throw common::InternalError(
        "FlushDirtySlotsToTrace",
        "slot_registry and trace_registry populated state mismatch");
  }

  std::span<const uint8_t> design_state(
      static_cast<const uint8_t*>(design_state_base),
      slot_registry.MaxExtent());

  if (!has_ownership) {
    // Legacy path: no ownership metadata. Direct dirty-slot trace flush.
    for (uint32_t slot_id : updates.DirtySlots()) {
      if (!selection.IsSelected(slot_id)) continue;
      const auto& meta = slot_registry.Get(slot_id);
      const auto* slot_base =
          design_state.subspan(meta.base_off, meta.total_bytes).data();
      trace.EmitValueChange(
          slot_id, SnapshotOwnerStorage(slot_id, meta, slot_base));
    }
    return;
  }

  // Owner-aware path: dirty flush with alias fanout.
  for (uint32_t owner_slot_id : updates.DirtySlots()) {
    // Backstop: forwarded alias slots must never appear in the dirty set.
    auto owner = slot_registry.GetStorageOwnerSlotId(owner_slot_id);
    if (owner != owner_slot_id) {
      throw common::InternalError(
          "FlushDirtySlotsToTrace",
          std::format(
              "forwarded alias slot {} appeared in dirty set (owner {})",
              owner_slot_id, owner));
    }

    // Snapshot owner storage once.
    const auto& meta = slot_registry.Get(owner_slot_id);
    const auto* slot_base =
        design_state.subspan(meta.base_off, meta.total_bytes).data();
    auto snapshot = SnapshotOwnerStorage(owner_slot_id, meta, slot_base);

    // Emit for the owner if selected.
    if (selection.IsSelected(owner_slot_id)) {
      trace.EmitValueChange(owner_slot_id, snapshot);
    }

    // Emit for all aliases sharing this owner, using the same snapshot.
    for (uint32_t alias : trace_registry.GetAliasGroup(owner_slot_id)) {
      if (alias == owner_slot_id) continue;
      if (!selection.IsSelected(alias)) continue;
      trace.EmitValueChange(alias, snapshot);
    }
  }
}

}  // namespace lyra::runtime
