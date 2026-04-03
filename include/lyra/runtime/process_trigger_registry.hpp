#pragma once

#include <cstdint>
#include <span>
#include <vector>

#include "lyra/common/edge_kind.hpp"

namespace lyra::runtime {

// Typed runtime descriptor parsed from the process trigger word table.
// One entry per trigger fact row (a process with N triggers produces N rows).
struct ProcessTriggerDescriptor {
  uint32_t scheduled_process_index = 0;
  uint32_t slot_id = 0;
  common::EdgeKind edge = common::EdgeKind::kAnyChange;
  bool is_groupable = false;
  // R5: True if slot_id is a body-local LocalSignalId, not a flat
  // design-global slot. When true, instance_id identifies the owning
  // instance and slot_id is the local signal ordinal.
  bool is_local = false;
  uint32_t instance_id = 0;
};

// R5: Domain-aware trigger identity key. Two different instances can have
// the same local slot_id and edge but represent distinct trigger sources.
struct ProcessTriggerKey {
  bool is_local = false;
  uint32_t instance_id = 0;
  uint32_t slot_id = 0;
  common::EdgeKind edge = common::EdgeKind::kAnyChange;

  auto operator<=>(const ProcessTriggerKey&) const = default;
};

// Constructor-time trigger group: processes grouped by domain-aware key.
// Only groupable descriptors participate. References into flat backing.
struct TriggerGroup {
  ProcessTriggerKey key;
  uint32_t process_start = 0;
  uint32_t process_count = 0;
};

// Constructor-time process trigger registry.
// Owns parsed descriptors, trigger groups, and flat-backed group membership.
// Built once at constructor time, consumed by engine for scheduling metadata.
//
// Invariants (guaranteed by ParseProcessTriggerDescriptors and
// BuildProcessTriggerRegistry):
// - All scheduled_process_index values in descriptors and
//   group_process_backing are valid (< num_processes at parse time).
// - All slot_id values are valid (< slot_count at parse time).
// - group_process_backing indices are safe for direct indexing.
struct ProcessTriggerRegistry {
  std::vector<ProcessTriggerDescriptor> descriptors;
  std::vector<TriggerGroup> groups;
  std::vector<uint32_t> group_process_backing;
};

// Parse process trigger word table into typed runtime descriptors.
// Validates: structural size, slot_id range, edge kind range,
// scheduled_process_index range, unknown flag bits.
auto ParseProcessTriggerDescriptors(
    std::span<const uint32_t> words, uint32_t slot_count,
    uint32_t num_processes) -> std::vector<ProcessTriggerDescriptor>;

// Build constructor-time trigger registry from parsed descriptors.
// Takes ownership of descriptors. Groups by (slot_id, edge) for
// groupable descriptors only. Flat-backed, deterministic order,
// deduplicated process indices per group.
auto BuildProcessTriggerRegistry(
    std::vector<ProcessTriggerDescriptor> descriptors)
    -> ProcessTriggerRegistry;

}  // namespace lyra::runtime
