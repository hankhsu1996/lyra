#pragma once

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

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

struct TimeAdvance {
  uint64_t time;
};

// Scalar value change. Snapshot captures the *new* value after the store.
// For packed types, the snapshot is taken from slot storage (post-memcpy).
// For strings, the snapshot is a deep copy of the new string content.
struct ValueChange {
  uint32_t slot_id;
  TraceValue value;
};

// Bulk memory write ($readmemh, $readmemb, $fread into unpacked array).
// Emitted once per call, not per element.
// Best-effort: only emitted when the target Place is rooted in a design slot
// (PlaceRoot::kDesign). For non-design targets (temporaries, stack memory),
// no MemoryDirty event is emitted.
struct MemoryDirty {
  uint32_t slot_id;
};

using TraceEvent = std::variant<TimeAdvance, ValueChange, MemoryDirty>;

}  // namespace lyra::trace
