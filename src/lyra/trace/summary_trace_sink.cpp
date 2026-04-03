#include "lyra/trace/summary_trace_sink.hpp"

#include <format>
#include <type_traits>
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
        } else if constexpr (std::is_same_v<T, GlobalValueChange>) {
          ++value_changes_;
          ++global_counts_[e.signal_id.value].value_changes;
        } else if constexpr (std::is_same_v<T, LocalValueChange>) {
          ++value_changes_;
          ++local_counts_[LocalKey{e.instance_id, e.signal_id.value}]
                .value_changes;
        } else if constexpr (std::is_same_v<T, GlobalMemoryDirty>) {
          ++memory_dirty_;
          ++global_counts_[e.signal_id.value].memory_dirty;
        } else if constexpr (std::is_same_v<T, LocalMemoryDirty>) {
          ++memory_dirty_;
          ++local_counts_[LocalKey{e.instance_id, e.signal_id.value}]
                .memory_dirty;
        }
      },
      event);
}

void SummaryTraceSink::PrintSummary() const {
  lyra::runtime::WriteOutput(
      std::format(
          "__LYRA_TRACE__: time_advances={} value_changes={} memory_dirty={}\n",
          time_advances_, value_changes_, memory_dirty_));

  for (const auto& [signal_id, counts] : global_counts_) {
    lyra::runtime::WriteOutput(
        std::format(
            "__LYRA_TRACE_SLOT__: slot={} value_changes={} memory_dirty={}\n",
            signal_id, counts.value_changes, counts.memory_dirty));
  }

  for (const auto& [key, counts] : local_counts_) {
    lyra::runtime::WriteOutput(
        std::format(
            "__LYRA_TRACE_SLOT__: instance={} local={} value_changes={} "
            "memory_dirty={}\n",
            key.instance_id, key.signal_id, counts.value_changes,
            counts.memory_dirty));
  }
}

void SummaryTraceSink::Reset() {
  time_advances_ = 0;
  value_changes_ = 0;
  memory_dirty_ = 0;
  global_counts_.clear();
  local_counts_.clear();
}

}  // namespace lyra::trace
