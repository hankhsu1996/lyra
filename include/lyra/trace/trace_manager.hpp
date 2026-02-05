#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/trace/trace_event.hpp"
#include "lyra/trace/trace_sink.hpp"

namespace lyra::trace {

class TraceManager {
 public:
  void SetEnabled(bool enabled) {
    enabled_ = enabled;
  }
  [[nodiscard]] bool IsEnabled() const {
    return enabled_;
  }

  void AddSink(std::unique_ptr<TraceSink> sink);

  void EmitTimeAdvance(uint64_t time);
  void EmitValueChange(uint32_t slot_id, TraceValue value);
  void EmitMemoryDirty(uint32_t slot_id);

  // Snapshot helpers.
  // SnapshotPacked: copies byte_size raw bytes from ptr.
  static auto SnapshotPacked(const void* ptr, uint32_t byte_size) -> TraceValue;
  // SnapshotString: deep-copies the string content via LyraStringAsView.
  static auto SnapshotString(const void* str_handle) -> TraceValue;

  // Post-run query.
  [[nodiscard]] auto Events() const -> const std::vector<TraceEvent>&;
  [[nodiscard]] auto CountValueChanges(uint32_t slot_id) const -> size_t;
  [[nodiscard]] auto CountMemoryDirty(uint32_t slot_id) const -> size_t;

  // Print summary to stdout.
  // Format: __LYRA_TRACE__: time_advances=N value_changes=M memory_dirty=K
  // Plus per-slot lines: __LYRA_TRACE_SLOT__: slot=S value_changes=V
  // memory_dirty=D
  void PrintSummary() const;

 private:
  void Record(TraceEvent event);

  bool enabled_ = false;
  std::vector<TraceEvent> events_;
  std::vector<std::unique_ptr<TraceSink>> sinks_;
};

}  // namespace lyra::trace
