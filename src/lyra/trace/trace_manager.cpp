#include "lyra/trace/trace_manager.hpp"

#include <cstdint>
#include <cstring>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/runtime/string.hpp"
#include "lyra/trace/trace_event.hpp"
#include "lyra/trace/trace_sink.hpp"

namespace lyra::trace {

TraceManager::TraceManager() {
  sinks_.push_back(&summary_sink_);
}

void TraceManager::SetEnabled(bool enabled) {
  if (enabled && !enabled_) {
    summary_sink_.Reset();
  }
  enabled_ = enabled;
}

void TraceManager::AddSink(std::unique_ptr<TraceSink> sink) {
  sinks_.push_back(sink.get());
  owned_sinks_.push_back(std::move(sink));
}

void TraceManager::EmitTimeAdvance(uint64_t time) {
  if (!enabled_) return;
  TimeAdvance event{.time = time};
  Dispatch(event);
}

void TraceManager::EmitValueChange(uint32_t slot_id, TraceValue value) {
  if (!enabled_) return;
  ValueChange event{.slot_id = slot_id, .value = std::move(value)};
  Dispatch(event);
}

void TraceManager::EmitMemoryDirty(uint32_t slot_id) {
  if (!enabled_) return;
  MemoryDirty event{.slot_id = slot_id};
  Dispatch(event);
}

auto TraceManager::SnapshotPacked(const void* ptr, uint32_t byte_size)
    -> TraceValue {
  std::vector<uint8_t> bytes(byte_size);
  std::memcpy(bytes.data(), ptr, byte_size);
  return PackedSnapshot{.byte_size = byte_size, .bytes = std::move(bytes)};
}

auto TraceManager::SnapshotString(const void* str_handle) -> TraceValue {
  if (str_handle == nullptr) {
    return std::string{};
  }
  std::string_view view = LyraStringAsView(str_handle);
  return std::string(view);
}

void TraceManager::SetSignalMeta(const runtime::TraceSignalMetaRegistry* meta) {
  signal_meta_ = meta;
}

void TraceManager::PrintSummary() const {
  summary_sink_.PrintSummary();
}

void TraceManager::Dispatch(const TraceEvent& event) {
  for (auto* sink : sinks_) {
    sink->OnEvent(event);
  }
}

}  // namespace lyra::trace
