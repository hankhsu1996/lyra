#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <map>

#include "lyra/trace/trace_sink.hpp"

namespace lyra::runtime {
class OutputDispatcher;
struct RuntimeInstance;
}  // namespace lyra::runtime

namespace lyra::trace {

// Default summary sink for --trace compatibility output.
//
// Incrementally accumulates aggregate counters (per-event-kind totals and
// per-signal breakdowns) and emits the __LYRA_TRACE__ format on PrintSummary().
//
// Tracks global and local signals separately. Global signals are keyed
// by GlobalSignalId. Local signals are keyed by (instance, LocalSignalId).
class SummaryTraceSink : public TraceSink {
 public:
  void OnEvent(const TraceEvent& event) override;

  void PrintSummary(lyra::runtime::OutputDispatcher& out) const;

  // Reset all counters to zero. Called by TraceManager when starting a
  // new trace session (false->true enable transition).
  void Reset();

 private:
  struct SignalCounts {
    size_t value_changes = 0;
    size_t memory_dirty = 0;
  };

  // Key for local signal counts: (instance, local_signal_id).
  // Uses std::less for pointer ordering to guarantee a total order.
  struct LocalKey {
    runtime::RuntimeInstance* instance;
    uint32_t signal_id;

    auto operator==(const LocalKey&) const -> bool = default;

    auto operator<(const LocalKey& o) const -> bool {
      if (instance != o.instance)
        return std::less<runtime::RuntimeInstance*>{}(instance, o.instance);
      return signal_id < o.signal_id;
    }
  };

  size_t time_advances_ = 0;
  size_t value_changes_ = 0;
  size_t memory_dirty_ = 0;
  std::map<uint32_t, SignalCounts> global_counts_;
  std::map<LocalKey, SignalCounts> local_counts_;
};

}  // namespace lyra::trace
