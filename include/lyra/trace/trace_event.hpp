#pragma once

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {
struct RuntimeInstance;
}

namespace lyra::trace {

// Raw snapshot of packed storage (2-state or 4-state).
// Contains the exact bytes from the runtime storage representation.
// For 2-state: byte_size = value storage size.
// For 4-state: byte_size = 2 * value_size (value_plane + x_mask_plane).
// Consumer needs type metadata to interpret.
struct PackedSnapshot {
  uint32_t byte_size;
  std::vector<uint8_t> bytes;
};

// Value snapshot at event time. String variant holds a deep copy of the
// content.
using TraceValue = std::variant<PackedSnapshot, std::string>;

// End-of-time-slot committed context marker. Emitted once per time slot at
// flush time, immediately before any ValueChange events for that slot. The
// delta is the final delta cycle count after convergence, not the delta at
// which any particular value was last written. Sinks use this to set their
// current (time, delta) context for subsequent ValueChange formatting.
// Emitted for every time slot, including slots with no dirty signals.
struct TimeAdvance {
  uint64_t time = 0;
  uint32_t delta = 0;
};

// Domain-split value change events.
// Global: for package/global signals, identified by GlobalSignalId.
// Local: for instance-owned signals, identified by (RuntimeInstance*,
// LocalSignalId) pair. Instance pointer is the direct object-model anchor.
struct GlobalValueChange {
  runtime::GlobalSignalId signal_id;
  TraceValue value;
};

struct LocalValueChange {
  runtime::RuntimeInstance* instance;
  runtime::LocalSignalId signal_id;
  TraceValue value;
};

// Domain-split bulk memory write events.
// Emitted once per call ($readmemh, $readmemb, $fread into unpacked array),
// not per element.
struct GlobalMemoryDirty {
  runtime::GlobalSignalId signal_id;
};

struct LocalMemoryDirty {
  runtime::RuntimeInstance* instance;
  runtime::LocalSignalId signal_id;
};

using TraceEvent = std::variant<
    TimeAdvance, GlobalValueChange, LocalValueChange, GlobalMemoryDirty,
    LocalMemoryDirty>;

}  // namespace lyra::trace
