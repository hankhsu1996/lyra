#include "lyra/mir/placement.hpp"

#include <format>

#include "lyra/common/internal_error.hpp"

namespace lyra::mir {

auto GetInstancePlacement(const PlacementMap& map, uint32_t module_index)
    -> const InstancePlacement& {
  if (module_index >= map.instances.size()) {
    throw common::InternalError(
        "GetInstancePlacement",
        std::format(
            "module_index {} out of range (placement count={})", module_index,
            map.instances.size()));
  }
  return map.instances[module_index];
}

auto GetInstanceBaseSlot(const PlacementMap& map, uint32_t module_index)
    -> uint32_t {
  return GetInstancePlacement(map, module_index).design_state_base_slot;
}

}  // namespace lyra::mir
