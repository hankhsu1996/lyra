#include "lyra/runtime/slot_meta.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <span>
#include <string_view>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/output_sink.hpp"
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

SlotMetaRegistry::SlotMetaRegistry(const uint32_t* words, uint32_t count) {
  slots_.reserve(count);

  std::span<const uint32_t> table(
      words, static_cast<size_t>(count) * slot_meta_abi::kStride);

  for (uint32_t i = 0; i < count; ++i) {
    auto row = table.subspan(
        static_cast<size_t>(i) * slot_meta_abi::kStride,
        slot_meta_abi::kStride);

    uint32_t raw_kind = row[slot_meta_abi::kFieldKind];
    if (raw_kind > 4) {
      throw common::InternalError(
          "SlotMetaRegistry",
          std::format("unknown kind {} for slot {}", raw_kind, i));
    }

    auto kind = static_cast<SlotStorageKind>(raw_kind);
    uint32_t base_off = row[slot_meta_abi::kFieldBaseOff];
    uint32_t total_bytes = row[slot_meta_abi::kFieldTotalBytes];
    uint32_t value_off = row[slot_meta_abi::kFieldValueOff];
    uint32_t value_bytes = row[slot_meta_abi::kFieldValueBytes];
    uint32_t unk_off = row[slot_meta_abi::kFieldUnkOff];
    uint32_t unk_bytes = row[slot_meta_abi::kFieldUnkBytes];

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

    max_extent_ = std::max(max_extent_, base_off + total_bytes);

    slots_.push_back(
        SlotMeta{
            .base_off = base_off,
            .total_bytes = total_bytes,
            .kind = kind,
            .planes =
                PackedPlanes{
                    .value_off = value_off,
                    .value_bytes = value_bytes,
                    .unk_off = unk_off,
                    .unk_bytes = unk_bytes,
                },
        });
  }
}

auto SlotMetaRegistry::Get(uint32_t slot_id) const -> const SlotMeta& {
  if (slot_id >= slots_.size()) {
    throw common::InternalError(
        "SlotMetaRegistry::Get",
        std::format(
            "slot_id {} out of range (size {})", slot_id, slots_.size()));
  }
  return slots_[slot_id];
}

auto SlotMetaRegistry::Size() const -> uint32_t {
  return static_cast<uint32_t>(slots_.size());
}

auto SlotMetaRegistry::MaxExtent() const -> uint32_t {
  return max_extent_;
}

auto SlotMetaRegistry::IsPopulated() const -> bool {
  return !slots_.empty();
}

void SlotMetaRegistry::DumpSummary() const {
  WriteOutput(
      std::format(
          "__LYRA_SLOT_META__: version={} count={}\n", slot_meta_abi::kVersion,
          slots_.size()));

  for (uint32_t i = 0; i < slots_.size(); ++i) {
    const auto& slot = slots_[i];
    std::string line = std::format(
        "__LYRA_SLOT_META__: slot={} kind={} base_off={} total_bytes={}", i,
        KindName(slot.kind), slot.base_off, slot.total_bytes);

    if (slot.kind == SlotStorageKind::kPacked4) {
      line += std::format(
          " value_off={} value_bytes={} unk_off={} unk_bytes={}",
          slot.planes.value_off, slot.planes.value_bytes, slot.planes.unk_off,
          slot.planes.unk_bytes);
    }

    line += "\n";
    WriteOutput(line);
  }
}

}  // namespace lyra::runtime
