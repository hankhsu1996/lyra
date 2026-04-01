#include "lyra/llvm_backend/inspection_plan.hpp"

#include <cstdint>
#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout/storage_types.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::mir_to_llvm {

auto BuildInspectionPlan(
    const CodegenSession& session, std::span<const InspectedVarRef> refs)
    -> InspectionPlan {
  InspectionPlan plan;

  const auto& layout = *session.layout;
  const auto& provenance = session.realization.slot_trace_provenance;

  for (const auto& ref : refs) {
    auto slot_value = static_cast<uint32_t>(ref.slot_id.value);
    if (slot_value >= provenance.size()) {
      throw common::InternalError(
          "BuildInspectionPlan",
          std::format(
              "slot_id {} out of range for provenance (size {})", slot_value,
              provenance.size()));
    }

    const auto& prov = provenance[slot_value];

    if (prov.scope_kind == mir::SlotScopeKind::kPackage) {
      auto abs_off =
          ArenaByteOffset{layout.design.GetStorageByteOffset(ref.slot_id)};
      plan.globals.push_back(
          InspectedGlobalVar{
              .name = ref.name,
              .slot_id = ref.slot_id,
              .placement = DesignGlobalPlacement{abs_off},
          });
    } else {
      auto instance_id = prov.scope_ref;
      auto abs_off =
          ArenaByteOffset{layout.design.GetStorageByteOffset(ref.slot_id)};
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
              .slot_id = ref.slot_id,
              .placement =
                  InstanceOwnedPlacement{
                      .owner_instance_id = instance_id,
                      .rel_off = rel_off,
                  },
          });
    }
  }

  return plan;
}

}  // namespace lyra::lowering::mir_to_llvm
