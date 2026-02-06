#include "lyra/runtime/trace_flush.hpp"

#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/string.hpp"
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

}  // namespace

void FlushDirtySlotsToTrace(
    trace::TraceManager& trace, const SlotMetaRegistry& registry,
    const void* design_state_base, const UpdateSet& updates) {
  std::span<const uint8_t> design_state(
      static_cast<const uint8_t*>(design_state_base), registry.MaxExtent());
  for (uint32_t slot_id : updates.DirtySlots()) {
    const auto& meta = registry.Get(slot_id);
    const auto* slot_base =
        design_state.subspan(meta.base_off, meta.total_bytes).data();
    trace.EmitValueChange(slot_id, SnapshotSlotValue(slot_id, meta, slot_base));
  }
}

}  // namespace lyra::runtime
