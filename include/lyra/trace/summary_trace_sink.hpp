#pragma once

#include <cstddef>
#include <cstdint>
#include <map>

#include "lyra/trace/trace_sink.hpp"

namespace lyra::trace {

// Default summary sink for --trace compatibility output.
//
// Incrementally accumulates aggregate counters (per-event-kind totals and
// per-slot breakdowns) and emits the __LYRA_TRACE__ format on PrintSummary().
//
// This is the built-in default sink, not the optimized long-term signal
// tracing path. Future text trace (O3) and VCD (O2) sinks will be the
// primary user-facing trace consumers; this sink will remain as a
// lightweight debug/summary tool.
//
// Slot counts use std::map for deterministic output ordering. If profiling
// shows this matters on the hot path, switch to a dense vector indexed by
// slot id (requires knowing the slot count at construction).
class SummaryTraceSink : public TraceSink {
 public:
  void OnEvent(const TraceEvent& event) override;

  void PrintSummary() const;

  // Reset all counters to zero. Called by TraceManager when starting a
  // new trace session (false->true enable transition).
  void Reset();

 private:
  struct SlotCounts {
    size_t value_changes = 0;
    size_t memory_dirty = 0;
  };

  size_t time_advances_ = 0;
  size_t value_changes_ = 0;
  size_t memory_dirty_ = 0;
  std::map<uint32_t, SlotCounts> slot_counts_;
};

}  // namespace lyra::trace
