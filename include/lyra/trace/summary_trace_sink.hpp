#pragma once

#include <cstddef>
#include <cstdint>
#include <map>

#include "lyra/runtime/signal_coord.hpp"
#include "lyra/trace/trace_sink.hpp"

namespace lyra::trace {

// Default summary sink for --trace compatibility output.
//
// Incrementally accumulates aggregate counters (per-event-kind totals and
// per-signal breakdowns) and emits the __LYRA_TRACE__ format on PrintSummary().
//
// R5: Tracks global and local signals separately. Global signals are keyed
// by GlobalSignalId. Local signals are keyed by (instance_id, LocalSignalId).
class SummaryTraceSink : public TraceSink {
 public:
  void OnEvent(const TraceEvent& event) override;

  void PrintSummary() const;

  // Reset all counters to zero. Called by TraceManager when starting a
  // new trace session (false->true enable transition).
  void Reset();

 private:
  struct SignalCounts {
    size_t value_changes = 0;
    size_t memory_dirty = 0;
  };

  // Key for local signal counts: (instance_id, local_signal_id).
  struct LocalKey {
    runtime::InstanceId instance_id;
    uint32_t signal_id;
    auto operator<=>(const LocalKey&) const = default;
  };

  size_t time_advances_ = 0;
  size_t value_changes_ = 0;
  size_t memory_dirty_ = 0;
  std::map<uint32_t, SignalCounts> global_counts_;
  std::map<LocalKey, SignalCounts> local_counts_;
};

}  // namespace lyra::trace
