#pragma once

namespace lyra::trace {
class TraceManager;
}

namespace lyra::runtime {

class SlotMetaRegistry;
class UpdateSet;

// Flush dirty slots to trace manager at end-of-time-slot.
// Iterates dirty slots, snapshots each based on SlotMetaRegistry, emits
// ValueChange events. Does NOT clear the UpdateSet; caller is responsible
// for calling UpdateSet::Clear() afterward.
void FlushDirtySlotsToTrace(
    trace::TraceManager& trace, const SlotMetaRegistry& registry,
    const void* design_state_base, const UpdateSet& updates);

}  // namespace lyra::runtime
