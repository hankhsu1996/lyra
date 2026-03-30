#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/slot_meta.hpp"

namespace lyra::runtime {

auto Engine::ResolveSlotBytes(uint32_t slot_id) const -> const uint8_t* {
  const auto& meta = slot_meta_registry_.Get(slot_id);
  if (meta.domain == SlotStorageDomain::kDesignGlobal) {
    return static_cast<const uint8_t*>(design_state_base_) +
           meta.design_base_off;
  }
  if (meta.owner_instance_id >= instances_.size()) {
    throw common::InternalError(
        "Engine::ResolveSlotBytes",
        std::format(
            "slot {} owner_instance_id {} out of range (instances={})", slot_id,
            meta.owner_instance_id, instances_.size()));
  }
  const auto* instance = instances_[meta.owner_instance_id];
  return ResolveInstanceStorageOffset(
      *instance, meta.instance_rel_off, meta.total_bytes,
      "Engine::ResolveSlotBytes");
}

auto Engine::ResolveSlotBytesMut(uint32_t slot_id) -> uint8_t* {
  const auto& meta = slot_meta_registry_.Get(slot_id);
  if (meta.domain == SlotStorageDomain::kDesignGlobal) {
    return static_cast<uint8_t*>(design_state_base_) + meta.design_base_off;
  }
  if (meta.owner_instance_id >= instances_.size()) {
    throw common::InternalError(
        "Engine::ResolveSlotBytesMut",
        std::format(
            "slot {} owner_instance_id {} out of range (instances={})", slot_id,
            meta.owner_instance_id, instances_.size()));
  }
  const auto* instance = instances_[meta.owner_instance_id];
  return ResolveInstanceStorageOffset(
      *instance, meta.instance_rel_off, meta.total_bytes,
      "Engine::ResolveSlotBytesMut");
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

    if (meta.owner_instance_id >= instances_.size()) {
      throw common::InternalError(
          "Engine::ValidateInstanceOwnedSlotMeta",
          std::format(
              "slot {} owner_instance_id {} >= instance count {}", slot_id,
              meta.owner_instance_id, instances_.size()));
    }
    if (instances_[meta.owner_instance_id] == nullptr) {
      throw common::InternalError(
          "Engine::ValidateInstanceOwnedSlotMeta",
          std::format(
              "slot {} owner_instance_id {} resolves to null instance", slot_id,
              meta.owner_instance_id));
    }
  }
}

}  // namespace lyra::runtime
