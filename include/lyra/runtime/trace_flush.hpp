#pragma once

namespace lyra::trace {
class TraceManager;
}

namespace lyra::runtime {

class SlotMetaRegistry;
class TraceSelectionRegistry;
class UpdateSet;

// Flush dirty slots to trace manager at end-of-time-slot.
// Iterates dirty slots, snapshots each based on SlotMetaRegistry, emits
// ValueChange events. Skips slots not selected by selection.IsSelected().
// Does NOT clear the UpdateSet; caller is responsible for calling
// UpdateSet::Clear() afterward.
void FlushDirtySlotsToTrace(
    trace::TraceManager& trace, const SlotMetaRegistry& registry,
    const void* design_state_base, const UpdateSet& updates,
    const TraceSelectionRegistry& selection);

}  // namespace lyra::runtime
