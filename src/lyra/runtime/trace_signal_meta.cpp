#include "lyra/runtime/trace_signal_meta.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <string_view>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/trace_signal_meta_abi.hpp"

namespace lyra::runtime {

TraceSignalMetaRegistry::TraceSignalMetaRegistry(
    const uint32_t* words, uint32_t word_count, const char* pool,
    uint32_t pool_size) {
  if (word_count > 0 && words == nullptr) {
    throw common::InternalError(
        "TraceSignalMetaRegistry", "word_count > 0 but words is null");
  }
  if (pool_size > 0 && pool == nullptr) {
    throw common::InternalError(
        "TraceSignalMetaRegistry", "pool_size > 0 but pool is null");
  }

  if (pool != nullptr && pool_size > 0) {
    string_pool_.assign(pool, pool + pool_size);
  }

  if (word_count == 0) return;

  if (word_count % trace_signal_meta_abi::kStride != 0) {
    throw common::InternalError(
        "TraceSignalMetaRegistry",
        std::format(
            "word_count {} not divisible by stride {}", word_count,
            trace_signal_meta_abi::kStride));
  }

  auto count = word_count / trace_signal_meta_abi::kStride;
  metas_.reserve(count);

  for (uint32_t i = 0; i < count; ++i) {
    auto base = static_cast<size_t>(i) * trace_signal_meta_abi::kStride;

    uint32_t name_off = words[base + trace_signal_meta_abi::kFieldNameStrOff];
    uint32_t bit_width = words[base + trace_signal_meta_abi::kFieldBitWidth];
    uint32_t kind_raw = words[base + trace_signal_meta_abi::kFieldTraceKind];
    uint32_t storage_owner =
        words[base + trace_signal_meta_abi::kFieldStorageOwnerSlotId];

    if (name_off >= string_pool_.size()) {
      throw common::InternalError(
          "TraceSignalMetaRegistry",
          std::format(
              "slot {} name_str_off {} out of range (pool size {})", i,
              name_off, string_pool_.size()));
    }
    // Verify the offset points to a NUL-terminated string within pool bounds.
    auto start = string_pool_.begin() + name_off;
    auto nul = std::find(start, string_pool_.end(), '\0');
    if (nul == string_pool_.end()) {
      throw common::InternalError(
          "TraceSignalMetaRegistry",
          std::format(
              "slot {} name at offset {} not NUL-terminated", i, name_off));
    }
    if (kind_raw > static_cast<uint32_t>(TraceSignalKind::kParam)) {
      throw common::InternalError(
          "TraceSignalMetaRegistry",
          std::format("slot {} invalid trace kind {}", i, kind_raw));
    }

    metas_.push_back(
        TraceSignalMeta{
            .name_str_off = name_off,
            .bit_width = bit_width,
            .kind = static_cast<TraceSignalKind>(kind_raw),
            .storage_owner_slot_id = storage_owner,
        });
  }

  BuildAliasGroups(nullptr);
}

void TraceSignalMetaRegistry::BuildAliasGroups(
    const SlotMetaRegistry* slot_registry) {
  alias_groups_.clear();
  for (uint32_t i = 0; i < metas_.size(); ++i) {
    auto owner = metas_[i].storage_owner_slot_id;
    if (owner >= metas_.size()) {
      throw common::InternalError(
          "TraceSignalMetaRegistry::BuildAliasGroups",
          std::format(
              "slot {} storage_owner_slot_id {} out of range (size {})", i,
              owner, metas_.size()));
    }
    alias_groups_[owner].push_back(i);
  }
  // Verify every alias group: root must be self-owning, every member
  // must point to the group's owner.
  for (const auto& [owner, group] : alias_groups_) {
    if (metas_[owner].storage_owner_slot_id != owner) {
      throw common::InternalError(
          "TraceSignalMetaRegistry::BuildAliasGroups",
          std::format(
              "alias group owner {} is not self-owning (points to {})", owner,
              metas_[owner].storage_owner_slot_id));
    }
    for (uint32_t member : group) {
      if (metas_[member].storage_owner_slot_id != owner) {
        throw common::InternalError(
            "TraceSignalMetaRegistry::BuildAliasGroups",
            std::format(
                "alias group member {} points to {} but group owner is {}",
                member, metas_[member].storage_owner_slot_id, owner));
      }
      // Safety-net validation: alias-group-invariant trace/storage shape
      // must already have been canonicalized before runtime metadata
      // construction. Snapshot bytes are reused across the alias group,
      // so any field that affects snapshot format or trace rendering
      // must match.
      if (member != owner) {
        const auto& alias_meta = metas_[member];
        const auto& owner_meta = metas_[owner];
        if (alias_meta.bit_width != owner_meta.bit_width ||
            alias_meta.kind != owner_meta.kind) {
          throw common::InternalError(
              "TraceSignalMetaRegistry::BuildAliasGroups",
              std::format(
                  "alias {} trace shape (width={}, kind={}) != owner {} "
                  "(width={}, kind={})",
                  member, alias_meta.bit_width,
                  static_cast<int>(alias_meta.kind), owner,
                  owner_meta.bit_width, static_cast<int>(owner_meta.kind)));
        }
        // Cross-registry check: alias must have the same storage byte
        // extent as owner (snapshot reuse invariant).
        if (slot_registry != nullptr) {
          auto alias_bytes = slot_registry->Get(member).total_bytes;
          auto owner_bytes = slot_registry->Get(owner).total_bytes;
          if (alias_bytes != owner_bytes) {
            throw common::InternalError(
                "TraceSignalMetaRegistry::BuildAliasGroups",
                std::format(
                    "alias {} total_bytes {} != owner {} total_bytes {}",
                    member, alias_bytes, owner, owner_bytes));
          }
        }
      }
    }
  }
}

auto TraceSignalMetaRegistry::Get(uint32_t slot_id) const
    -> const TraceSignalMeta& {
  if (slot_id >= metas_.size()) {
    throw common::InternalError(
        "TraceSignalMetaRegistry::Get",
        std::format(
            "slot_id {} out of range (size {})", slot_id, metas_.size()));
  }
  return metas_[slot_id];
}

auto TraceSignalMetaRegistry::Name(uint32_t slot_id) const -> std::string_view {
  if (slot_id >= metas_.size()) {
    throw common::InternalError(
        "TraceSignalMetaRegistry::Name",
        std::format(
            "slot_id {} out of range (size {})", slot_id, metas_.size()));
  }
  return PoolString(metas_[slot_id].name_str_off);
}

auto TraceSignalMetaRegistry::Count() const -> size_t {
  return metas_.size();
}

auto TraceSignalMetaRegistry::IsPopulated() const -> bool {
  return !metas_.empty();
}

auto TraceSignalMetaRegistry::GetAliasGroup(uint32_t owner_slot_id) const
    -> std::span<const uint32_t> {
  if (!IsPopulated()) return {};
  if (owner_slot_id >= metas_.size()) {
    throw common::InternalError(
        "TraceSignalMetaRegistry::GetAliasGroup",
        std::format(
            "owner_slot_id {} out of range (size {})", owner_slot_id,
            metas_.size()));
  }
  auto it = alias_groups_.find(owner_slot_id);
  if (it == alias_groups_.end()) {
    throw common::InternalError(
        "TraceSignalMetaRegistry::GetAliasGroup",
        std::format(
            "owner_slot_id {} not found in alias groups", owner_slot_id));
  }
  return it->second;
}

void TraceSignalMetaRegistry::Reserve(size_t count) {
  metas_.reserve(count);
}

void TraceSignalMetaRegistry::AppendSignal(
    std::string_view name, uint32_t bit_width, TraceSignalKind kind,
    uint32_t storage_owner_slot_id) {
  // Ensure pool starts with NUL sentinel if empty.
  if (string_pool_.empty()) {
    string_pool_.push_back('\0');
  }
  auto name_off = static_cast<uint32_t>(string_pool_.size());
  string_pool_.insert(string_pool_.end(), name.begin(), name.end());
  string_pool_.push_back('\0');
  metas_.push_back(
      TraceSignalMeta{
          .name_str_off = name_off,
          .bit_width = bit_width,
          .kind = kind,
          .storage_owner_slot_id = storage_owner_slot_id,
      });
}

auto TraceSignalMetaRegistry::PoolString(uint32_t offset) const -> const char* {
  if (offset >= string_pool_.size()) {
    throw common::InternalError(
        "TraceSignalMetaRegistry::PoolString",
        std::format(
            "offset {} out of range (pool size {})", offset,
            string_pool_.size()));
  }
  return string_pool_.data() + offset;
}

}  // namespace lyra::runtime
