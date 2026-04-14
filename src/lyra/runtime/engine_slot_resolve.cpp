#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/slot_meta.hpp"

namespace lyra::runtime {

auto Engine::ResolveGlobalSlotBase(GlobalSignalId signal) const
    -> const uint8_t* {
  const auto& meta = slot_meta_registry_.Get(signal.value);
  return runtime::ResolveGlobalSlotBase(meta, design_state_base_);
}

auto Engine::ResolveGlobalSlotBaseMut(GlobalSignalId signal) -> uint8_t* {
  const auto& meta = slot_meta_registry_.Get(signal.value);
  return runtime::ResolveGlobalSlotBaseMut(meta, design_state_base_);
}

void Engine::ValidateInstanceOwnedSlotMeta() const {
  for (uint32_t slot_id = 0; slot_id < slot_meta_registry_.Size(); ++slot_id) {
    const auto& meta = slot_meta_registry_.Get(slot_id);
    if (meta.domain != SlotStorageDomain::kInstanceOwned) continue;

    // R2 invariant: every instance-owned slot is self-owning.
    if (meta.storage_owner_slot_id != slot_id) {
      throw common::InternalError(
          "Engine::ValidateInstanceOwnedSlotMeta",
          std::format(
              "slot {} storage_owner_slot_id {} is not self (R2 violated)",
              slot_id, meta.storage_owner_slot_id));
    }

    const auto* inst = FindInstance(meta.owner_instance_id);
    if (inst == nullptr) {
      throw common::InternalError(
          "Engine::ValidateInstanceOwnedSlotMeta",
          std::format(
              "slot {} has no instance for owner_instance_id {}", slot_id,
              meta.owner_instance_id.value));
    }
  }
}

}  // namespace lyra::runtime
