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
  local_has_observers.assign(local_signal_count, 0);
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
  const char* begin = &trace_name_pool[off];
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
  std::string_view path = inst.scope.path_c_str != nullptr
                              ? inst.scope.path_c_str
                              : std::string_view{};
  if (path.empty()) {
    return std::string(local_name);
  }
  return std::format("{}.{}", path, local_name);
}

auto ResolveInstanceSlotBase(const RuntimeInstance& inst, LocalSignalId signal)
    -> const uint8_t* {
  const auto& obs = inst.observability;
  if (obs.layout == nullptr) {
    throw common::InternalError(
        "ResolveInstanceSlotBase", "instance has no observable layout");
  }
  if (signal.value >= obs.layout->slot_meta.size()) {
    throw common::InternalError(
        "ResolveInstanceSlotBase",
        std::format(
            "local_signal_id {} out of range {}", signal.value,
            obs.layout->slot_meta.size()));
  }
  const auto& meta = obs.layout->slot_meta[signal.value];
  return ResolveInstanceStorageOffset(
      inst, meta.instance_rel_off, meta.total_bytes, "ResolveInstanceSlotBase");
}

auto ResolveInstanceSlotBaseMut(RuntimeInstance& inst, LocalSignalId signal)
    -> uint8_t* {
  const auto& obs = inst.observability;
  if (obs.layout == nullptr) {
    throw common::InternalError(
        "ResolveInstanceSlotBaseMut", "instance has no observable layout");
  }
  if (signal.value >= obs.layout->slot_meta.size()) {
    throw common::InternalError(
        "ResolveInstanceSlotBaseMut",
        std::format(
            "local_signal_id {} out of range {}", signal.value,
            obs.layout->slot_meta.size()));
  }
  const auto& meta = obs.layout->slot_meta[signal.value];
  return ResolveInstanceStorageOffset(
      inst, meta.instance_rel_off, meta.total_bytes,
      "ResolveInstanceSlotBaseMut");
}

}  // namespace lyra::runtime
