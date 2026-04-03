#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/trace/summary_trace_sink.hpp"
#include "lyra/trace/trace_event.hpp"
#include "lyra/trace/trace_sink.hpp"

namespace lyra::runtime {
class TraceSignalMetaRegistry;
}

namespace lyra::trace {

// Streaming trace dispatcher.
//
// Owns an enabled/disabled gate and a set of sinks. When enabled, each
// Emit* method constructs a TraceEvent and dispatches it to all registered
// sinks through a single uniform fanout path. When disabled, Emit* methods
// return immediately with no dispatch.
//
// Session lifecycle:
//   SetEnabled(true)  -- starts a new trace session; resets the built-in
//                        summary sink so counters begin at zero.
//   Emit* calls       -- dispatched to all sinks while enabled.
//   SetEnabled(false) -- ends the session. No further dispatch occurs.
//   PrintSummary()    -- prints the most recent session's summary.
//                        Safe to call after disable; outputs the frozen
//                        counters from the last session. If tracing was
//                        never enabled, prints zeros.
//
// Sink registration:
//   All sinks (built-in summary + external) share a single dispatch path.
//   External sinks must be installed before SetEnabled(true). Sinks added
//   mid-session observe only subsequent events with no replay or
//   initialization. The built-in summary sink is always registered.
//
// Ownership:
//   TraceManager owns a SummaryTraceSink as a value member and keeps a
//   raw pointer to it in the dispatch list. External sinks are owned via
//   unique_ptr in a separate container. This split is necessary because
//   PrintSummary() needs typed access to the summary sink, while dispatch
//   treats all sinks uniformly. Non-copyable/non-movable because the
//   dispatch list contains a self-referential pointer.
class TraceManager {
 public:
  TraceManager();
  ~TraceManager() = default;

  TraceManager(const TraceManager&) = delete;
  auto operator=(const TraceManager&) -> TraceManager& = delete;
  TraceManager(TraceManager&&) = delete;
  auto operator=(TraceManager&&) -> TraceManager& = delete;

  // Enable or disable trace dispatch.
  // false->true: starts a new session, resets summary counters.
  // true->false: ends the session, freezes summary state.
  void SetEnabled(bool enabled);
  [[nodiscard]] auto IsEnabled() const -> bool {
    return enabled_;
  }

  // Register an external sink. Ownership transfers to TraceManager.
  // Must be called before SetEnabled(true) for the sink to observe the
  // full session. Sinks added mid-session observe only subsequent events;
  // no replay or initialization occurs.
  void AddSink(std::unique_ptr<TraceSink> sink);

  void EmitTimeAdvance(uint64_t time, uint32_t delta = 0);

  // R5: Domain-split emit methods. Typed identity throughout.
  void EmitGlobalValueChange(
      runtime::GlobalSignalId signal_id, TraceValue value);
  void EmitLocalValueChange(
      runtime::InstanceId instance_id, runtime::LocalSignalId signal_id,
      TraceValue value);
  void EmitGlobalMemoryDirty(runtime::GlobalSignalId signal_id);
  void EmitLocalMemoryDirty(
      runtime::InstanceId instance_id, runtime::LocalSignalId signal_id);

  // Snapshot helpers.
  // SnapshotPacked: copies byte_size raw bytes from ptr.
  static auto SnapshotPacked(const void* ptr, uint32_t byte_size) -> TraceValue;
  // SnapshotString: deep-copies the string content via LyraStringAsView.
  static auto SnapshotString(const void* str_handle) -> TraceValue;

  // Set non-owning pointer to trace signal metadata registry.
  // Must be called before SetEnabled(true) for sinks to observe metadata.
  void SetSignalMeta(const runtime::TraceSignalMetaRegistry* meta);

  // Get trace signal metadata registry (may be null if not set).
  [[nodiscard]] auto GetSignalMeta() const
      -> const runtime::TraceSignalMetaRegistry* {
    return signal_meta_;
  }

  // Print the most recent session's summary to stdout. Delegates to the
  // built-in summary sink. If tracing was never enabled, prints zeros.
  void PrintSummary() const;

 private:
  void Dispatch(const TraceEvent& event);

  bool enabled_ = false;

  // Built-in summary sink, always registered in sinks_.
  // Reset on each false->true enable transition.
  SummaryTraceSink summary_sink_;

  // Uniform dispatch list. All sinks (including summary_sink_) are
  // dispatched through this single path. Non-owning pointers.
  std::vector<TraceSink*> sinks_;

  // Lifetime ownership for externally added sinks.
  std::vector<std::unique_ptr<TraceSink>> owned_sinks_;

  // Non-owning pointer to trace signal metadata. Set by Engine.
  const runtime::TraceSignalMetaRegistry* signal_meta_ = nullptr;
};

}  // namespace lyra::trace
