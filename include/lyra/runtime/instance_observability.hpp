#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/runtime/engine_subscriptions.hpp"
#include "lyra/runtime/engine_types.hpp"
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
  // True if this slot is an owned container (backing data in appendix).
  // When true, backing_rel_off/backing_bytes locate the actual data in the
  // appendix region (body-relative offset >= inline_size).
  // instance_rel_off still points to the OwnedStorageHandle in inline region.
  bool is_container = false;
  // Body-relative byte offset of the backing data (appendix region).
  // Valid only when is_container is true.
  uint32_t backing_rel_off = 0;
  // Byte size of the backing data.
  // Valid only when is_container is true.
  uint32_t backing_bytes = 0;
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
  // Per-signal observer flag: 1 if any edge/change/container/rebind subscriber
  // exists for this local signal. Used to skip dirty marking for unwatched
  // signals. Updated at subscription install/remove time.
  std::vector<uint8_t> local_has_observers;
  std::vector<uint32_t> activation_gen;

  // R5: Per-instance comb trigger map, indexed by LocalSignalId.value.
  // Maps local signals to ranges in the engine's shared comb_trigger_backing_.
  std::vector<TriggerRange> local_comb_trigger_map;
  // List of local signal IDs that have comb triggers (for seeding dirty marks).
  std::vector<LocalSignalId> local_comb_trigger_slots;

  // R5: Per-instance connection trigger map, indexed by LocalSignalId.value.
  // Maps local signals to ranges in the engine's shared all_connections_.
  std::vector<TriggerRange> local_conn_trigger_map;

  // R5: Per-instance flush epoch. Advanced when FlushLocalSignalUpdates
  // dispatches local delta for this instance. Used by rebind watcher
  // freshness checks to skip unnecessary snapshot refresh.
  uint64_t local_flush_epoch = 0;

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
