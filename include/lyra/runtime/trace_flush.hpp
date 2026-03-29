#pragma once

#include <span>

namespace lyra::trace {
class TraceManager;
}

namespace lyra::runtime {

struct RuntimeInstance;
class SlotMetaRegistry;
class TraceSelectionRegistry;
class TraceSignalMetaRegistry;
class UpdateSet;

// Flush dirty slots to trace manager at end-of-time-slot.
// Iterates dirty slots, snapshots each based on SlotMetaRegistry, emits
// ValueChange events. For each dirty storage-owner slot, also emits
// ValueChange for all alias-visible entries sharing that owner (using the
// same snapshot). Skips slots not selected by selection.IsSelected().
//
// Backstop invariant: forwarded alias slots must never appear in the dirty
// set. Alias canonicalization is compile-time (EmitMutationTargetSignalId)
// and descriptor-time (RebuildCanonicalConnections), not runtime per-mark.
// Throws InternalError if a dirty alias is encountered.
//
// Does NOT clear the UpdateSet; caller is responsible for calling
// UpdateSet::Clear() afterward.
void FlushDirtySlotsToTrace(
    trace::TraceManager& trace, const SlotMetaRegistry& slot_registry,
    const TraceSignalMetaRegistry& trace_registry,
    const void* design_state_base,
    std::span<const RuntimeInstance* const> instances, const UpdateSet& updates,
    const TraceSelectionRegistry& selection);

}  // namespace lyra::runtime
