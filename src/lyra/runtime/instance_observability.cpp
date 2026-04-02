#include "lyra/runtime/instance_observability.hpp"

#include <cstdint>
#include <cstring>
#include <format>
#include <string>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/signal_coord.hpp"

namespace lyra::runtime {

void RuntimeInstanceObservability::Init() {
  if (layout == nullptr) {
    throw common::InternalError(
        "RuntimeInstanceObservability::Init", "layout must be set first");
  }
  if (local_signal_count != layout->slot_meta.size()) {
    throw common::InternalError(
        "RuntimeInstanceObservability::Init",
        std::format(
            "local_signal_count {} != layout->slot_meta.size() {}",
            local_signal_count, layout->slot_meta.size()));
  }
  if (layout->slot_meta.size() != layout->trace_meta.size()) {
    throw common::InternalError(
        "RuntimeInstanceObservability::Init",
        std::format(
            "slot_meta.size() {} != trace_meta.size() {} -- parallel "
            "indexing invariant violated",
            layout->slot_meta.size(), layout->trace_meta.size()));
  }

  std::vector<uint32_t> sizes;
  sizes.reserve(local_signal_count);
  for (const auto& meta : layout->slot_meta) {
    sizes.push_back(meta.total_bytes);
  }
  local_updates.Init(local_signal_count, sizes);

  trace_select = layout->trace_select_default;
  local_signal_subs.resize(local_signal_count);
  activation_gen.assign(local_signal_count, 0);
}

auto BodyObservableLayout::TraceLocalName(LocalSignalId signal) const
    -> std::string_view {
  if (signal.value >= trace_meta.size()) {
    throw common::InternalError(
        "BodyObservableLayout::TraceLocalName",
        std::format(
            "local_signal_id {} out of range {}", signal.value,
            trace_meta.size()));
  }
  uint32_t off = trace_meta[signal.value].local_name_offset;
  if (off >= trace_name_pool.size()) {
    throw common::InternalError(
        "BodyObservableLayout::TraceLocalName",
        std::format(
            "name offset {} out of range {}", off, trace_name_pool.size()));
  }
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  const char* begin = trace_name_pool.data() + off;
  auto remaining = trace_name_pool.size() - off;
  const auto* end =
      static_cast<const char*>(std::memchr(begin, '\0', remaining));
  if (end == nullptr) {
    throw common::InternalError(
        "BodyObservableLayout::TraceLocalName",
        std::format("unterminated name at offset {}", off));
  }
  return {begin, static_cast<size_t>(end - begin)};
}

auto ComposeHierarchicalTraceName(
    const RuntimeInstance& inst, LocalSignalId local_signal,
    const BodyObservableLayout& layout) -> std::string {
  auto local_name = layout.TraceLocalName(local_signal);
  if (inst.path_c_str == nullptr || inst.path_c_str[0] == '\0') {
    return std::string(local_name);
  }
  return std::format("{}.{}", inst.path_c_str, local_name);
}

}  // namespace lyra::runtime
