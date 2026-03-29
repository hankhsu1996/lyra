#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/slot_meta.hpp"

namespace lyra::runtime {

auto Engine::ResolveSlotBytes(uint32_t slot_id) const -> const uint8_t* {
  const auto& meta = slot_meta_registry_.Get(slot_id);
  // For kDesignGlobal slots: check if the canonical storage owner is
  // instance-owned. Forwarded aliases are kDesignGlobal but their canonical
  // owner may be kInstanceOwned in a different instance.
  const SlotMeta* resolved = &meta;
  if (meta.domain == SlotStorageDomain::kDesignGlobal) {
    const auto& owner_meta =
        slot_meta_registry_.Get(meta.storage_owner_slot_id);
    if (owner_meta.domain == SlotStorageDomain::kInstanceOwned) {
      resolved = &owner_meta;
    } else {
      return static_cast<const uint8_t*>(design_state_base_) +
             meta.design_base_off;
    }
  }
  if (resolved->owner_instance_id >= instances_.size()) {
    throw common::InternalError(
        "Engine::ResolveSlotBytes",
        std::format(
            "slot {} owner_instance_id {} out of range (instances={})", slot_id,
            resolved->owner_instance_id, instances_.size()));
  }
  const auto* instance = instances_[resolved->owner_instance_id];
  return ResolveInstanceStorageOffset(
      *instance, resolved->instance_rel_off, meta.total_bytes,
      "Engine::ResolveSlotBytes");
}

auto Engine::ResolveSlotBytesMut(uint32_t slot_id) -> uint8_t* {
  const auto& meta = slot_meta_registry_.Get(slot_id);
  const SlotMeta* resolved = &meta;
  if (meta.domain == SlotStorageDomain::kDesignGlobal) {
    const auto& owner_meta =
        slot_meta_registry_.Get(meta.storage_owner_slot_id);
    if (owner_meta.domain == SlotStorageDomain::kInstanceOwned) {
      resolved = &owner_meta;
    } else {
      return static_cast<uint8_t*>(design_state_base_) + meta.design_base_off;
    }
  }
  if (resolved->owner_instance_id >= instances_.size()) {
    throw common::InternalError(
        "Engine::ResolveSlotBytesMut",
        std::format(
            "slot {} owner_instance_id {} out of range (instances={})", slot_id,
            resolved->owner_instance_id, instances_.size()));
  }
  const auto* instance = instances_[resolved->owner_instance_id];
  return ResolveInstanceStorageOffset(
      *instance, resolved->instance_rel_off, meta.total_bytes,
      "Engine::ResolveSlotBytesMut");
}

void Engine::ValidateInstanceOwnedSlotMeta() const {
  for (uint32_t slot_id = 0; slot_id < slot_meta_registry_.Size(); ++slot_id) {
    const auto& meta = slot_meta_registry_.Get(slot_id);
    if (meta.domain != SlotStorageDomain::kInstanceOwned) continue;

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
