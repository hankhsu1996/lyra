#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/local_update_set.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"

namespace lyra::runtime {

struct RuntimeInstance;

// Instance-owned slot metadata for one body -- shared across all instances
// of the same specialization. Indexed by LocalSignalId.value.
//
// Does not contain owner_instance_id -- the owning instance is known at
// the lookup root. Does not contain storage_owner_slot_id -- alias
// forwarding is removed for instance-owned signals.
struct InstanceSlotMeta {
  uint32_t instance_rel_off = 0;
  uint32_t total_bytes = 0;
  SlotStorageKind kind = SlotStorageKind::kPacked2;
  PackedPlanes planes;
};

// Instance-owned trace metadata for one body -- shared across all instances
// of the same specialization. Indexed by LocalSignalId.value.
//
// Body-local signal names are stored in a separate string pool on the
// owning BodyObservableLayout. Hierarchical names are composed at the
// sink/render boundary, not stored pre-flattened.
struct BodyTraceMeta {
  uint32_t local_name_offset = 0;
  uint32_t bit_width = 0;
  TraceSignalKind kind = TraceSignalKind::kVariable;
};

// Per-body observable layout shared by all instances of the same
// specialization.
//
// Ownership: one BodyObservableLayout per shared body / specialization,
// owned by engine-level shared-body runtime metadata, not by each
// instance. RuntimeInstanceObservability::layout is a non-owning pointer.
//
// trace_name_pool is seeded with a sentinel '\0' at offset 0 so that
// offset 0 always means "no name". Real names start at offset >= 1.
struct BodyObservableLayout {
  std::vector<InstanceSlotMeta> slot_meta;
  std::vector<BodyTraceMeta> trace_meta;
  std::vector<char> trace_name_pool;
  std::vector<uint8_t> trace_select_default;

  // Lookup body-local signal name by LocalSignalId.
  // Pool offset 0 is the empty sentinel (pool is seeded with '\0' at offset 0).
  // Throws InternalError on out-of-range local id or corrupted pool offset.
  [[nodiscard]] auto TraceLocalName(LocalSignalId signal) const
      -> std::string_view;
};

// Per-instance observability state.
//
// Each RuntimeInstance owns one of these. The layout pointer is shared
// across all instances of the same specialization.
struct RuntimeInstanceObservability {
  const BodyObservableLayout* layout = nullptr;
  uint32_t local_signal_count = 0;

  LocalUpdateSet local_updates;
  std::vector<uint8_t> trace_select;
  std::vector<SlotSubscriptions> local_signal_subs;
  std::vector<uint32_t> activation_gen;

  // Transitional flat coordinate base for comb fixpoint interop.
  // Maps local_signal_id -> flat_slot_id as (flat_coord_base + local_id).
  // Runtime-internal only -- not part of the binary ABI or codegen contract.
  // Deleted when comb fixpoint reads local containers directly (Cut 3+).
  uint32_t flat_coord_base = 0;

  // Initialize all local vectors to local_signal_count.
  // Must be called after layout and local_signal_count are set.
  void Init();
};

// Compose hierarchical trace name at the sink/render boundary.
// Not stored pre-flattened. Only called when a sink actually needs
// the full name for rendering.
auto ComposeHierarchicalTraceName(
    const RuntimeInstance& inst, LocalSignalId local_signal,
    const BodyObservableLayout& layout) -> std::string;

// R5: Object-local slot resolution for instance-owned signals.
// The instance is the lookup root; LocalSignalId indexes into the
// body's InstanceSlotMeta table. No flat slot_id conversion needed.
[[nodiscard]] auto ResolveInstanceSlotBase(
    const RuntimeInstance& inst, LocalSignalId signal) -> const uint8_t*;
[[nodiscard]] auto ResolveInstanceSlotBaseMut(
    RuntimeInstance& inst, LocalSignalId signal) -> uint8_t*;

}  // namespace lyra::runtime
