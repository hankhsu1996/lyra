#pragma once

#include <cstdint>
#include <span>

namespace lyra::trace {
class TraceManager;
}

namespace lyra::runtime {

class Engine;
struct RuntimeInstance;
class SlotMetaRegistry;
class TraceSelectionRegistry;
class UpdateSet;

// R5: Flush dirty global slots to trace manager at end-of-time-slot.
// Iterates dirty global slots, snapshots each based on SlotMetaRegistry,
// emits GlobalValueChange events. Skips slots not selected by selection.
//
// Only design-global slots (slot_id < global_slot_count) are emitted.
// Instance-owned slot_ids may appear in the global UpdateSet from:
//   - Connection propagation (writes flat slot_ids for both domains)
//   - Subscription dispatch mirror (FlushSignalUpdates mirrors local
//     delta into update_set_ for legacy flat trigger dispatch)
// These are skipped here and flushed through local update sets by
// FlushLocalDirtySlotsToTrace.
//
// Does NOT clear the UpdateSet; caller is responsible for calling
// UpdateSet::Clear() afterward.
void FlushGlobalDirtySlotsToTrace(
    trace::TraceManager& trace, const SlotMetaRegistry& slot_registry,
    const void* design_state_base, const UpdateSet& updates,
    const TraceSelectionRegistry& selection, uint32_t global_slot_count);

// R5: Flush dirty local slots for all instances to trace manager.
// Iterates each instance's local_updates, snapshots via InstanceSlotMeta,
// emits LocalValueChange events. Skips signals not selected by per-instance
// trace_select.
//
// Invariants enforced (throws InternalError on violation):
//   - No null instance pointers in the instance list.
//   - Instance with dirty local signals must have local_signal_count > 0.
//   - Instance with local signals must have a non-null observability layout.
//   - trace_select must be sized to local_signal_count.
//   - Dirty local signal id must be within local_signal_count range.
// Only "not selected" is a normal skip; all other unexpected states are bugs.
//
// Does NOT clear local update sets; caller is responsible.
void FlushLocalDirtySlotsToTrace(
    const Engine& engine, trace::TraceManager& trace,
    std::span<RuntimeInstance* const> dirty_instances);

}  // namespace lyra::runtime
