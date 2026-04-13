#include "lyra/runtime/slot_meta.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <span>
#include <string_view>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/slot_meta_abi.hpp"

namespace lyra::runtime {

namespace {

auto KindName(SlotStorageKind kind) -> std::string_view {
  switch (kind) {
    case SlotStorageKind::kPacked2:
      return "packed2";
    case SlotStorageKind::kPacked4:
      return "packed4";
    case SlotStorageKind::kString:
      return "string";
    case SlotStorageKind::kHandle:
      return "handle";
    case SlotStorageKind::kAggregate:
      return "aggregate";
  }
  return "unknown";
}

}  // namespace

auto ResolveInstanceStorageOffset(
    const RuntimeInstance& instance, uint32_t rel_off, uint32_t access_size,
    const char* caller) -> uint8_t* {
  const uint64_t inline_size = instance.storage.inline_size;
  const uint64_t appendix_size = instance.storage.appendix_size;
  const uint64_t total_size = inline_size + appendix_size;
  const uint64_t end = static_cast<uint64_t>(rel_off) + access_size;

  if (end > total_size) {
    throw common::InternalError(
        caller, std::format(
                    "instance storage range [{}..{}) exceeds total size {}",
                    rel_off, end, total_size));
  }

  auto as_bytes = [](std::byte* base, uint64_t size) {
    return std::span(reinterpret_cast<uint8_t*>(base), size);
  };

  if (rel_off < inline_size) {
    return &as_bytes(instance.storage.inline_base, inline_size)[rel_off];
  }

  return &as_bytes(
      instance.storage.appendix_base, appendix_size)[rel_off - inline_size];
}

auto ResolveGlobalSlotBase(const SlotMeta& meta, const void* design_state_base)
    -> const uint8_t* {
  if (meta.domain != SlotStorageDomain::kDesignGlobal) {
    throw common::InternalError(
        "ResolveGlobalSlotBase", std::format(
                                     "expected kDesignGlobal, got domain {}",
                                     static_cast<int>(meta.domain)));
  }
  auto base = std::span(
      static_cast<const uint8_t*>(design_state_base),
      meta.design_base_off + meta.total_bytes);
  return &base[meta.design_base_off];
}

auto ResolveGlobalSlotBaseMut(const SlotMeta& meta, void* design_state_base)
    -> uint8_t* {
  if (meta.domain != SlotStorageDomain::kDesignGlobal) {
    throw common::InternalError(
        "ResolveGlobalSlotBaseMut", std::format(
                                        "expected kDesignGlobal, got domain {}",
                                        static_cast<int>(meta.domain)));
  }
  auto base = std::span(
      static_cast<uint8_t*>(design_state_base),
      meta.design_base_off + meta.total_bytes);
  return &base[meta.design_base_off];
}

SlotMetaRegistry::SlotMetaRegistry(const uint32_t* words, uint32_t count) {
  slots_.reserve(count);

  std::span<const uint32_t> table(
      words, static_cast<size_t>(count) * slot_meta_abi::kStride);

  for (uint32_t i = 0; i < count; ++i) {
    auto row = table.subspan(
        static_cast<size_t>(i) * slot_meta_abi::kStride,
        slot_meta_abi::kStride);

    uint32_t raw_domain = row[slot_meta_abi::kFieldDomain];
    if (raw_domain > 1) {
      throw common::InternalError(
          "SlotMetaRegistry",
          std::format("unknown domain {} for slot {}", raw_domain, i));
    }
    auto domain = static_cast<SlotStorageDomain>(raw_domain);

    uint32_t design_base_off = row[slot_meta_abi::kFieldDesignBaseOff];
    uint32_t owner_instance_id = row[slot_meta_abi::kFieldOwnerInstanceId];
    uint32_t instance_rel_off = row[slot_meta_abi::kFieldInstanceRelOff];

    uint32_t raw_kind = row[slot_meta_abi::kFieldKind];
    if (raw_kind > 4) {
      throw common::InternalError(
          "SlotMetaRegistry",
          std::format("unknown kind {} for slot {}", raw_kind, i));
    }

    auto kind = static_cast<SlotStorageKind>(raw_kind);
    uint32_t total_bytes = row[slot_meta_abi::kFieldTotalBytes];
    uint32_t value_off = row[slot_meta_abi::kFieldValueOff];
    uint32_t value_bytes = row[slot_meta_abi::kFieldValueBytes];
    uint32_t unk_off = row[slot_meta_abi::kFieldUnkOff];
    uint32_t unk_bytes = row[slot_meta_abi::kFieldUnkBytes];
    uint32_t storage_owner = row[slot_meta_abi::kFieldStorageOwnerSlotId];

    if (storage_owner >= count) {
      throw common::InternalError(
          "SlotMetaRegistry",
          std::format(
              "slot {} storage_owner_slot_id {} out of range (count {})", i,
              storage_owner, count));
    }

    if (total_bytes == 0) {
      throw common::InternalError(
          "SlotMetaRegistry", std::format("zero total_bytes for slot {}", i));
    }

    if (kind != SlotStorageKind::kPacked4) {
      if (value_off != 0 || value_bytes != 0 || unk_off != 0 ||
          unk_bytes != 0) {
        throw common::InternalError(
            "SlotMetaRegistry",
            std::format("non-zero plane fields for non-packed4 slot {}", i));
      }
    } else {
      if (value_bytes == 0) {
        throw common::InternalError(
            "SlotMetaRegistry",
            std::format("zero value_bytes for packed4 slot {}", i));
      }
      if (unk_bytes == 0) {
        throw common::InternalError(
            "SlotMetaRegistry",
            std::format("zero unk_bytes for packed4 slot {}", i));
      }
      if (unk_off <= value_off) {
        throw common::InternalError(
            "SlotMetaRegistry",
            std::format(
                "unk_off ({}) must be > value_off ({}) for packed4 slot {}",
                unk_off, value_off, i));
      }
    }

    // Only design-global slots contribute to the design_state extent.
    if (domain == SlotStorageDomain::kDesignGlobal) {
      max_extent_ = std::max(max_extent_, design_base_off + total_bytes);
    }

    slots_.push_back(
        SlotMeta{
            .domain = domain,
            .design_base_off = design_base_off,
            .owner_instance_id = {owner_instance_id},
            .instance_rel_off = instance_rel_off,
            .total_bytes = total_bytes,
            .kind = kind,
            .planes =
                PackedPlanes{
                    .value_off = value_off,
                    .value_bytes = value_bytes,
                    .unk_off = unk_off,
                    .unk_bytes = unk_bytes,
                },
            .storage_owner_slot_id = storage_owner,
        });
  }

  // R2 invariant: every slot is self-owning.
  for (uint32_t i = 0; i < slots_.size(); ++i) {
    auto owner = slots_[i].storage_owner_slot_id;
    if (owner != i) {
      throw common::InternalError(
          "SlotMetaRegistry::SlotMetaRegistry",
          std::format(
              "slot {} storage_owner_slot_id {} is not self (R2 violated)", i,
              owner));
    }
  }
}

void SlotMetaRegistry::AppendSlot(SlotMeta meta) {
  if (meta.total_bytes == 0) {
    throw common::InternalError(
        "SlotMetaRegistry::AppendSlot",
        std::format("zero total_bytes for slot {}", slots_.size()));
  }
  if (meta.domain == SlotStorageDomain::kDesignGlobal) {
    max_extent_ =
        std::max(max_extent_, meta.design_base_off + meta.total_bytes);
  }
  slots_.push_back(meta);
}

void SlotMetaRegistry::ThrowOutOfRange(uint32_t slot_id) const {
  throw common::InternalError(
      "SlotMetaRegistry::Get",
      std::format("slot_id {} out of range (size {})", slot_id, slots_.size()));
}

void SlotMetaRegistry::DumpSummary(OutputDispatcher& out) const {
  out.DrainSimOutputBuffer();
  out.WriteProtocolRecord(
      std::format(
          "__LYRA_SLOT_META__: version={} count={}\n", slot_meta_abi::kVersion,
          slots_.size()));

  for (uint32_t i = 0; i < slots_.size(); ++i) {
    const auto& slot = slots_[i];
    std::string line;
    if (slot.domain == SlotStorageDomain::kDesignGlobal) {
      line = std::format(
          "__LYRA_SLOT_META__: slot={} domain=global kind={} "
          "design_base_off={} total_bytes={}",
          i, KindName(slot.kind), slot.design_base_off, slot.total_bytes);
    } else {
      line = std::format(
          "__LYRA_SLOT_META__: slot={} domain=instance kind={} "
          "owner_instance_id={} instance_rel_off={} total_bytes={}",
          i, KindName(slot.kind), slot.owner_instance_id, slot.instance_rel_off,
          slot.total_bytes);
    }

    if (slot.kind == SlotStorageKind::kPacked4) {
      line += std::format(
          " value_off={} value_bytes={} unk_off={} unk_bytes={}",
          slot.planes.value_off, slot.planes.value_bytes, slot.planes.unk_off,
          slot.planes.unk_bytes);
    }

    line += "\n";
    out.WriteProtocolRecord(line);
  }
}

}  // namespace lyra::runtime
