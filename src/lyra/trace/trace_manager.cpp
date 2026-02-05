#include "lyra/trace/trace_manager.hpp"

#include <cstdint>
#include <cstring>
#include <format>
#include <map>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/string.hpp"
#include "lyra/trace/trace_event.hpp"
#include "lyra/trace/trace_sink.hpp"

namespace lyra::trace {

void TraceManager::AddSink(std::unique_ptr<TraceSink> sink) {
  sinks_.push_back(std::move(sink));
}

void TraceManager::EmitTimeAdvance(uint64_t time) {
  Record(TimeAdvance{.time = time});
}

void TraceManager::EmitValueChange(uint32_t slot_id, TraceValue value) {
  Record(ValueChange{.slot_id = slot_id, .value = std::move(value)});
}

void TraceManager::EmitMemoryDirty(uint32_t slot_id) {
  Record(MemoryDirty{.slot_id = slot_id});
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
  std::string_view view = LyraStringAsView(
      const_cast<void*>(
          str_handle));  // NOLINT(cppcoreguidelines-pro-type-const-cast)
  return std::string(view);
}

auto TraceManager::Events() const -> const std::vector<TraceEvent>& {
  return events_;
}

auto TraceManager::CountValueChanges(uint32_t slot_id) const -> size_t {
  size_t count = 0;
  for (const auto& event : events_) {
    if (const auto* vc = std::get_if<ValueChange>(&event)) {
      if (vc->slot_id == slot_id) {
        ++count;
      }
    }
  }
  return count;
}

auto TraceManager::CountMemoryDirty(uint32_t slot_id) const -> size_t {
  size_t count = 0;
  for (const auto& event : events_) {
    if (const auto* md = std::get_if<MemoryDirty>(&event)) {
      if (md->slot_id == slot_id) {
        ++count;
      }
    }
  }
  return count;
}

void TraceManager::PrintSummary() const {
  size_t time_advances = 0;
  size_t value_changes = 0;
  size_t memory_dirty = 0;

  // Per-slot counters
  struct SlotCounts {
    size_t value_changes = 0;
    size_t memory_dirty = 0;
  };
  std::map<uint32_t, SlotCounts> slot_counts;

  for (const auto& event : events_) {
    std::visit(
        [&](const auto& e) {
          using T = std::decay_t<decltype(e)>;
          if constexpr (std::is_same_v<T, TimeAdvance>) {
            ++time_advances;
          } else if constexpr (std::is_same_v<T, ValueChange>) {
            ++value_changes;
            slot_counts[e.slot_id].value_changes++;
          } else if constexpr (std::is_same_v<T, MemoryDirty>) {
            ++memory_dirty;
            slot_counts[e.slot_id].memory_dirty++;
          }
        },
        event);
  }

  lyra::runtime::WriteOutput(
      std::format(
          "__LYRA_TRACE__: time_advances={} value_changes={} memory_dirty={}\n",
          time_advances, value_changes, memory_dirty));

  for (const auto& [slot_id, counts] : slot_counts) {
    lyra::runtime::WriteOutput(
        std::format(
            "__LYRA_TRACE_SLOT__: slot={} value_changes={} memory_dirty={}\n",
            slot_id, counts.value_changes, counts.memory_dirty));
  }
}

void TraceManager::Record(TraceEvent event) {
  for (const auto& sink : sinks_) {
    sink->OnEvent(event);
  }
  events_.push_back(std::move(event));
}

}  // namespace lyra::trace
