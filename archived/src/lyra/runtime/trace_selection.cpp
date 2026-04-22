#include "lyra/runtime/trace_selection.hpp"

#include <algorithm>
#include <cstdint>
#include <format>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

void TraceSelectionRegistry::Init(uint32_t slot_count) {
  if (configured_) {
    throw common::InternalError(
        "TraceSelectionRegistry::Init",
        "already configured; call Clear() before re-initializing");
  }
  selected_.assign(slot_count, 1);
  configured_ = true;
}

void TraceSelectionRegistry::Clear() {
  selected_.clear();
  configured_ = false;
}

void TraceSelectionRegistry::SelectAll() {
  if (!configured_) {
    throw common::InternalError(
        "TraceSelectionRegistry::SelectAll", "not configured; call Init first");
  }
  std::ranges::fill(selected_, 1);
}

void TraceSelectionRegistry::SelectNone() {
  if (!configured_) {
    throw common::InternalError(
        "TraceSelectionRegistry::SelectNone",
        "not configured; call Init first");
  }
  std::ranges::fill(selected_, 0);
}

void TraceSelectionRegistry::SetSelected(uint32_t slot_id, bool selected) {
  if (!configured_ || slot_id >= selected_.size()) {
    throw common::InternalError(
        "TraceSelectionRegistry::SetSelected",
        std::format(
            "slot_id {} out of range (configured={}, slots={})", slot_id,
            configured_, selected_.size()));
  }
  selected_[slot_id] = selected ? 1 : 0;
}

auto TraceSelectionRegistry::IsSelected(uint32_t slot_id) const -> bool {
  if (!configured_) return false;
  if (slot_id >= selected_.size()) {
    throw common::InternalError(
        "TraceSelectionRegistry::IsSelected",
        std::format(
            "slot_id {} out of range (have {} slots)", slot_id,
            selected_.size()));
  }
  return selected_[slot_id] != 0;
}

}  // namespace lyra::runtime
