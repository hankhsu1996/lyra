#include "lyra/llvm_backend/inspection_plan.hpp"

#include <cstdint>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout/storage_types.hpp"

namespace lyra::lowering::mir_to_llvm {

auto BuildInspectionPlan(
    const Layout& layout, std::span<const InspectedVarRef> refs)
    -> InspectionPlan {
  InspectionPlan plan;

  for (const auto& ref : refs) {
    auto slot_value = static_cast<uint32_t>(ref.slot_id.value);
    auto abs_off =
        ArenaByteOffset{layout.design.GetStorageByteOffset(ref.slot_id)};
    auto type_info = layout.design.slot_type_infos.at(slot_value);

    if (slot_value < layout.num_package_slots) {
      plan.globals.push_back(
          InspectedGlobalVar{
              .name = ref.name,
              .placement = DesignGlobalPlacement{abs_off},
              .type_info = type_info,
          });
    } else {
      auto owner = ResolveInstanceOwnedFlatSlot(
          layout.num_package_slots, layout.instance_slot_counts, slot_value);
      auto instance_id = owner.instance_id.value;
      auto instance_base =
          layout.GetInstanceStorageBase(ModuleIndex{instance_id});
      if (!instance_base.abs_byte_offset.has_value()) {
        throw common::InternalError(
            "BuildInspectionPlan",
            std::format(
                "instance {} has no storage base for slot {}", instance_id,
                slot_value));
      }
      auto rel_off = ToInstanceOffset(abs_off, *instance_base.abs_byte_offset);
      plan.instance_owned.push_back(
          InspectedInstanceVar{
              .name = ref.name,
              .placement =
                  InstanceOwnedPlacement{
                      .owner_instance_id = instance_id,
                      .rel_off = rel_off,
                  },
              .type_info = type_info,
          });
    }
  }

  return plan;
}

}  // namespace lyra::lowering::mir_to_llvm
