#include "lyra/llvm_backend/connection_lowering.hpp"

#include <optional>

#include "lyra/llvm_backend/lower.hpp"
#include "lyra/mir/connection_endpoint.hpp"
#include "lyra/mir/construction_input.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerConnectionArtifacts(const LoweringInput& input)
    -> LoweredConnectionArtifacts {
  LoweredConnectionArtifacts result;

  auto to_flat_slot = [&](const mir::BoundEndpoint& ref) -> common::SlotId {
    const auto& obj = input.construction->objects.at(ref.object_index.value);
    return common::SlotId{obj.design_state_base_slot + ref.local_slot.value};
  };

  // Build kernel entries from bound_connections (recipe path).
  // BoundConnection does not carry trigger_observation by design:
  // the recipe-based path resolves only slot-based triggers, which
  // never require byte-level observation metadata.
  if (input.bound_connections != nullptr) {
    for (const auto& bc : *input.bound_connections) {
      bool is_p2c = bc.kind == mir::PortConnection::Kind::kDriveParentToChild;
      const auto& src_ep = is_p2c ? bc.parent_source : bc.child_target;
      const auto& dst_ep = is_p2c ? bc.child_target : bc.parent_source;
      result.kernel_entries.push_back(
          ConnectionKernelEntry{
              .process_id = {},
              .src_slot = to_flat_slot(src_ep),
              .dst_slot = to_flat_slot(dst_ep),
              .trigger_slot = to_flat_slot(bc.trigger),
              .src_object_index = src_ep.object_index,
              .src_local_slot = src_ep.local_slot,
              .dst_object_index = dst_ep.object_index,
              .dst_local_slot = dst_ep.local_slot,
              .trigger_object_index = bc.trigger.object_index,
              .trigger_local_slot = bc.trigger.local_slot,
              .trigger_edge = bc.trigger_edge,
              .trigger_observation = std::nullopt,
          });
    }
  }

  return result;
}

}  // namespace lyra::lowering::mir_to_llvm
