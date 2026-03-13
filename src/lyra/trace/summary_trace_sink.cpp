#include "lyra/trace/summary_trace_sink.hpp"

#include <format>
#include <variant>

#include "lyra/runtime/output_sink.hpp"
#include "lyra/trace/trace_event.hpp"

namespace lyra::trace {

void SummaryTraceSink::OnEvent(const TraceEvent& event) {
  std::visit(
      [&](const auto& e) {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, TimeAdvance>) {
          ++time_advances_;
        } else if constexpr (std::is_same_v<T, ValueChange>) {
          ++value_changes_;
          ++slot_counts_[e.slot_id].value_changes;
        } else if constexpr (std::is_same_v<T, MemoryDirty>) {
          ++memory_dirty_;
          ++slot_counts_[e.slot_id].memory_dirty;
        }
      },
      event);
}

void SummaryTraceSink::PrintSummary() const {
  lyra::runtime::WriteOutput(
      std::format(
          "__LYRA_TRACE__: time_advances={} value_changes={} memory_dirty={}\n",
          time_advances_, value_changes_, memory_dirty_));

  for (const auto& [slot_id, counts] : slot_counts_) {
    lyra::runtime::WriteOutput(
        std::format(
            "__LYRA_TRACE_SLOT__: slot={} value_changes={} memory_dirty={}\n",
            slot_id, counts.value_changes, counts.memory_dirty));
  }
}

void SummaryTraceSink::Reset() {
  time_advances_ = 0;
  value_changes_ = 0;
  memory_dirty_ = 0;
  slot_counts_.clear();
}

}  // namespace lyra::trace
