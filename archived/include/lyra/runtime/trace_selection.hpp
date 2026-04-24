#pragma once

#include <cstdint>
#include <vector>

namespace lyra::runtime {

// Dense per-slot selection mask for producer-side trace filtering.
//
// Lifecycle:
//   Init(slot_count)  -- configures with all slots selected.
//   SelectAll/SelectNone/SetSelected -- mutations; throw if unconfigured.
//   IsSelected -- returns false if unconfigured; strict bounds if configured.
//   Clear()  -- resets to unconfigured state.
class TraceSelectionRegistry {
 public:
  TraceSelectionRegistry() = default;

  // Configure with slot_count slots, all selected by default.
  // Throws InternalError if already configured; call Clear() first.
  void Init(uint32_t slot_count);

  // Reset to unconfigured state.
  void Clear();

  void SelectAll();
  void SelectNone();
  void SetSelected(uint32_t slot_id, bool selected);
  [[nodiscard]] auto IsSelected(uint32_t slot_id) const -> bool;

  [[nodiscard]] auto IsConfigured() const -> bool {
    return configured_;
  }

 private:
  bool configured_ = false;
  std::vector<uint8_t> selected_;
};

}  // namespace lyra::runtime
