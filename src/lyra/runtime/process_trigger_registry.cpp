#include "lyra/runtime/process_trigger_registry.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <span>
#include <vector>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/process_trigger_abi.hpp"
#include "lyra/runtime/runtime_instance.hpp"

namespace lyra::runtime {

auto ParseProcessTriggerDescriptors(
    std::span<const uint32_t> words, uint32_t slot_count,
    uint32_t num_processes) -> std::vector<ProcessTriggerDescriptor> {
  if (words.empty()) return {};

  uint32_t num_entries = words[0];
  uint32_t expected_size = 1 + (num_entries * process_trigger_abi::kStride);
  if (words.size() != expected_size) {
    throw common::InternalError(
        "ParseProcessTriggerDescriptors",
        std::format(
            "structural size mismatch: expected {} words for {} entries, "
            "got {}",
            expected_size, num_entries, words.size()));
  }

  constexpr uint32_t kKnownFlags =
      process_trigger_abi::kFlagGroupable | process_trigger_abi::kFlagBodyLocal;

  std::vector<ProcessTriggerDescriptor> descriptors;
  descriptors.reserve(num_entries);

  for (uint32_t i = 0; i < num_entries; ++i) {
    uint32_t pos = 1 + (i * process_trigger_abi::kStride);
    uint32_t proc_idx = words[pos + process_trigger_abi::kFieldProcessIndex];
    uint32_t slot_id = words[pos + process_trigger_abi::kFieldSlotId];
    uint32_t edge_raw = words[pos + process_trigger_abi::kFieldEdgeKind];
    uint32_t flags = words[pos + process_trigger_abi::kFieldFlags];

    if (proc_idx >= num_processes) {
      throw common::InternalError(
          "ParseProcessTriggerDescriptors",
          std::format(
              "scheduled_process_index {} exceeds num_processes {} "
              "in entry {}",
              proc_idx, num_processes, i));
    }

    bool is_body_local = (flags & process_trigger_abi::kFlagBodyLocal) != 0;
    // Body-local slot ids are validated after relocation below.
    if (!is_body_local && slot_id >= slot_count) {
      throw common::InternalError(
          "ParseProcessTriggerDescriptors",
          std::format(
              "slot_id {} exceeds slot count {} in entry {}", slot_id,
              slot_count, i));
    }

    if (edge_raw > static_cast<uint32_t>(common::EdgeKind::kAnyChange)) {
      throw common::InternalError(
          "ParseProcessTriggerDescriptors",
          std::format("invalid edge kind {} in entry {}", edge_raw, i));
    }

    if ((flags & ~kKnownFlags) != 0) {
      throw common::InternalError(
          "ParseProcessTriggerDescriptors",
          std::format(
              "unknown flag bits {:#x} in entry {}", flags & ~kKnownFlags, i));
    }

    descriptors.push_back({
        .scheduled_process_index = proc_idx,
        .slot_id = slot_id,
        .edge = static_cast<common::EdgeKind>(edge_raw),
        .is_groupable = (flags & process_trigger_abi::kFlagGroupable) != 0,
    });
  }

  return descriptors;
}

auto BuildProcessTriggerRegistry(
    std::vector<ProcessTriggerDescriptor> descriptors)
    -> ProcessTriggerRegistry {
  ProcessTriggerRegistry registry;
  registry.descriptors = std::move(descriptors);

  if (registry.descriptors.empty()) return registry;

  // Collect groupable entries keyed by (slot_id, edge).
  struct GroupableEntry {
    uint32_t slot_id;
    common::EdgeKind edge;
    uint32_t scheduled_process_index;
  };
  std::vector<GroupableEntry> groupable;
  groupable.reserve(registry.descriptors.size());
  for (const auto& desc : registry.descriptors) {
    if (!desc.is_groupable) continue;
    groupable.push_back({
        .slot_id = desc.slot_id,
        .edge = desc.edge,
        .scheduled_process_index = desc.scheduled_process_index,
    });
  }

  if (groupable.empty()) return registry;

  // Sort by (slot_id, edge, process_index) for deterministic grouping.
  std::ranges::sort(groupable, [](const auto& a, const auto& b) {
    if (a.slot_id != b.slot_id) return a.slot_id < b.slot_id;
    if (a.edge != b.edge) return a.edge < b.edge;
    return a.scheduled_process_index < b.scheduled_process_index;
  });

  // Scan sorted entries, dedup process indices within each group key.
  registry.groups.reserve(groupable.size());
  registry.group_process_backing.reserve(groupable.size());

  uint32_t gi = 0;
  while (gi < groupable.size()) {
    uint32_t slot_id = groupable[gi].slot_id;
    auto edge = groupable[gi].edge;
    auto process_start =
        static_cast<uint32_t>(registry.group_process_backing.size());

    uint32_t last_proc = UINT32_MAX;
    while (gi < groupable.size() && groupable[gi].slot_id == slot_id &&
           groupable[gi].edge == edge) {
      if (groupable[gi].scheduled_process_index != last_proc) {
        registry.group_process_backing.push_back(
            groupable[gi].scheduled_process_index);
        last_proc = groupable[gi].scheduled_process_index;
      }
      ++gi;
    }

    auto process_count =
        static_cast<uint32_t>(registry.group_process_backing.size()) -
        process_start;

    registry.groups.push_back({
        .slot_id = slot_id,
        .edge = edge,
        .process_start = process_start,
        .process_count = process_count,
    });
  }

  return registry;
}

void Engine::InitProcessTriggerRegistry(
    std::span<const uint32_t> words, uint32_t num_connection, void** states) {
  auto descriptors = ParseProcessTriggerDescriptors(
      words, slot_meta_registry_.Size(), num_processes_);

  // Relocate body-local trigger entries to dense coordination coordinates.
  // Body-local entries carry kFlagBodyLocal in the word table. Re-read
  // the flags from the word table to apply instance-based relocation.
  auto proc_states = std::span(states, num_processes_);
  uint32_t slot_count = slot_meta_registry_.Size();
  if (!words.empty()) {
    uint32_t num_entries = words[0];
    for (uint32_t i = 0; i < num_entries && i < descriptors.size(); ++i) {
      uint32_t pos = 1 + (i * process_trigger_abi::kStride);
      uint32_t flags = words[pos + process_trigger_abi::kFieldFlags];
      if ((flags & process_trigger_abi::kFlagBodyLocal) != 0) {
        uint32_t proc_idx = descriptors[i].scheduled_process_index;
        if (proc_idx < num_connection) {
          throw common::InternalError(
              "InitProcessTriggerRegistry",
              std::format(
                  "body-local trigger entry {} targets connection process {}",
                  i, proc_idx));
        }
        if (proc_idx >= num_processes_) {
          throw common::InternalError(
              "InitProcessTriggerRegistry",
              std::format(
                  "body-local trigger entry {} targets invalid process {}", i,
                  proc_idx));
        }
        const auto* header =
            static_cast<const ProcessFrameHeader*>(proc_states[proc_idx]);
        if (header == nullptr || header->instance == nullptr) {
          throw common::InternalError(
              "InitProcessTriggerRegistry",
              std::format(
                  "body-local trigger entry {} has no owning instance "
                  "for process {}",
                  i, proc_idx));
        }
        descriptors[i].slot_id += header->instance->local_signal_coord_base;
        if (descriptors[i].slot_id >= slot_count) {
          throw common::InternalError(
              "InitProcessTriggerRegistry",
              std::format(
                  "relocated slot_id {} exceeds slot count {} in entry {}",
                  descriptors[i].slot_id, slot_count, i));
        }
      }
    }
  }

  process_trigger_registry_ =
      BuildProcessTriggerRegistry(std::move(descriptors));
}

}  // namespace lyra::runtime
