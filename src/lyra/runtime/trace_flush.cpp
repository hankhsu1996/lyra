#include "lyra/runtime/trace_flush.hpp"

#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/instance_observability.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/string.hpp"
#include "lyra/runtime/trace_selection.hpp"
#include "lyra/runtime/update_set.hpp"
#include "lyra/trace/trace_event.hpp"
#include "lyra/trace/trace_manager.hpp"

namespace lyra::runtime {

namespace {

// Build a TraceValue snapshot for a global slot based on its storage kind.
auto SnapshotGlobalSlotValue(
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
      if (meta.planes.value_off + meta.planes.value_bytes > meta.total_bytes) {
        throw common::InternalError(
            "SnapshotGlobalSlotValue",
            std::format(
                "slot {} kPacked4: value plane exceeds total_bytes", slot_id));
      }
      if (meta.planes.unk_off + meta.planes.unk_bytes > meta.total_bytes) {
        throw common::InternalError(
            "SnapshotGlobalSlotValue",
            std::format(
                "slot {} kPacked4: unk plane exceeds total_bytes", slot_id));
      }
      if (meta.planes.value_off == meta.planes.unk_off) {
        throw common::InternalError(
            "SnapshotGlobalSlotValue",
            std::format("slot {} kPacked4: value_off == unk_off", slot_id));
      }
      auto data = std::span(slot_base, meta.total_bytes);
      return trace::PackedSnapshot{
          .byte_size = meta.total_bytes,
          .bytes = std::vector<uint8_t>(data.begin(), data.end())};
    }

    case SlotStorageKind::kString: {
      if (meta.total_bytes != sizeof(void*)) {
        throw common::InternalError(
            "SnapshotGlobalSlotValue",
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
      if (meta.total_bytes != sizeof(void*)) {
        throw common::InternalError(
            "SnapshotGlobalSlotValue",
            std::format(
                "slot {} has kind=handle but total_bytes={} (expected {})",
                slot_id, meta.total_bytes, sizeof(void*)));
      }
      constexpr auto kPtrSize = static_cast<uint32_t>(sizeof(void*));
      auto data = std::span(slot_base, kPtrSize);
      return trace::PackedSnapshot{
          .byte_size = kPtrSize,
          .bytes = std::vector<uint8_t>(data.begin(), data.end())};
    }
  }
  throw common::InternalError(
      "SnapshotGlobalSlotValue",
      std::format("unknown SlotStorageKind: {}", static_cast<int>(meta.kind)));
}

// Build a TraceValue snapshot for an instance-owned slot.
auto SnapshotLocalSlotValue(
    uint32_t local_signal_id, const InstanceSlotMeta& meta,
    const uint8_t* slot_base) -> trace::TraceValue {
  switch (meta.kind) {
    case SlotStorageKind::kPacked2:
    case SlotStorageKind::kAggregate: {
      auto data = std::span(slot_base, meta.total_bytes);
      return trace::PackedSnapshot{
          .byte_size = meta.total_bytes,
          .bytes = std::vector<uint8_t>(data.begin(), data.end())};
    }

    case SlotStorageKind::kPacked4: {
      if (meta.planes.value_off + meta.planes.value_bytes > meta.total_bytes) {
        throw common::InternalError(
            "SnapshotLocalSlotValue",
            std::format(
                "local {} kPacked4: value plane exceeds total_bytes",
                local_signal_id));
      }
      if (meta.planes.unk_off + meta.planes.unk_bytes > meta.total_bytes) {
        throw common::InternalError(
            "SnapshotLocalSlotValue",
            std::format(
                "local {} kPacked4: unk plane exceeds total_bytes",
                local_signal_id));
      }
      if (meta.planes.value_off == meta.planes.unk_off) {
        throw common::InternalError(
            "SnapshotLocalSlotValue",
            std::format(
                "local {} kPacked4: value_off == unk_off", local_signal_id));
      }
      auto data = std::span(slot_base, meta.total_bytes);
      return trace::PackedSnapshot{
          .byte_size = meta.total_bytes,
          .bytes = std::vector<uint8_t>(data.begin(), data.end())};
    }

    case SlotStorageKind::kString: {
      if (meta.total_bytes != sizeof(void*)) {
        throw common::InternalError(
            "SnapshotLocalSlotValue",
            std::format(
                "local {} has kind=string but total_bytes={} (expected {})",
                local_signal_id, meta.total_bytes, sizeof(void*)));
      }
      // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
      auto* handle = *reinterpret_cast<void* const*>(slot_base);
      if (handle == nullptr) {
        return std::string{};
      }
      return std::string(LyraStringAsView(handle));
    }

    case SlotStorageKind::kHandle: {
      if (meta.total_bytes != sizeof(void*)) {
        throw common::InternalError(
            "SnapshotLocalSlotValue",
            std::format(
                "local {} has kind=handle but total_bytes={} (expected {})",
                local_signal_id, meta.total_bytes, sizeof(void*)));
      }
      constexpr auto kPtrSize = static_cast<uint32_t>(sizeof(void*));
      auto data = std::span(slot_base, kPtrSize);
      return trace::PackedSnapshot{
          .byte_size = kPtrSize,
          .bytes = std::vector<uint8_t>(data.begin(), data.end())};
    }
  }
  throw common::InternalError(
      "SnapshotLocalSlotValue",
      std::format("unknown SlotStorageKind: {}", static_cast<int>(meta.kind)));
}

}  // namespace

void FlushGlobalDirtySlotsToTrace(
    trace::TraceManager& trace, const SlotMetaRegistry& slot_registry,
    const void* design_state_base, const UpdateSet& updates,
    const TraceSelectionRegistry& selection, uint32_t global_slot_count) {
  for (uint32_t slot_id : updates.DirtySlots()) {
    if (slot_id >= global_slot_count) {
      throw common::InternalError(
          "FlushGlobalDirtySlotsToTrace",
          std::format(
              "instance-owned slot {} in global update_set "
              "(global_slot_count={})",
              slot_id, global_slot_count));
    }
    if (!selection.IsSelected(slot_id)) continue;
    const auto& meta = slot_registry.Get(slot_id);
    const auto* slot_base = ResolveGlobalSlotBase(meta, design_state_base);
    trace.EmitGlobalValueChange(
        GlobalSignalId{slot_id},
        SnapshotGlobalSlotValue(slot_id, meta, slot_base));
  }
}

void FlushLocalDirtySlotsToTrace(
    trace::TraceManager& trace,
    std::span<RuntimeInstance* const> dirty_instances) {
  for (auto* inst : dirty_instances) {
    if (inst == nullptr) {
      throw common::InternalError(
          "FlushLocalDirtySlotsToTrace", "null instance in dirty list");
    }

    auto& obs = inst->observability;
    if (obs.local_signal_count == 0) {
      if (!obs.local_updates.IsEmpty()) {
        throw common::InternalError(
            "FlushLocalDirtySlotsToTrace",
            std::format(
                "instance '{}' has dirty local signals but "
                "local_signal_count=0",
                inst->scope.path_c_str));
      }
      continue;
    }

    if (obs.layout == nullptr) {
      throw common::InternalError(
          "FlushLocalDirtySlotsToTrace",
          std::format(
              "instance '{}' has local signals but no observability layout",
              inst->scope.path_c_str));
    }

    if (obs.trace_select.size() != obs.local_signal_count) {
      throw common::InternalError(
          "FlushLocalDirtySlotsToTrace",
          std::format(
              "instance '{}' trace_select size {} != local_signal_count {}",
              inst->scope.path_c_str, obs.trace_select.size(),
              obs.local_signal_count));
    }

    for (LocalSignalId lid : obs.local_updates.DirtySignals()) {
      if (lid.value >= obs.local_signal_count) {
        throw common::InternalError(
            "FlushLocalDirtySlotsToTrace",
            std::format(
                "instance '{}' dirty local signal {} out of range {}",
                inst->scope.path_c_str, lid.value, obs.local_signal_count));
      }

      if (obs.trace_select[lid.value] == 0) continue;

      const auto& meta = obs.layout->slot_meta[lid.value];
      const auto* slot_base = ResolveInstanceSlotBase(*inst, lid);
      trace.EmitLocalValueChange(
          inst, lid, SnapshotLocalSlotValue(lid.value, meta, slot_base));
    }
  }
}

}  // namespace lyra::runtime
