#include "lyra/llvm_backend/forwarding_map.hpp"

#include <algorithm>
#include <format>

#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::mir_to_llvm {

void ForwardingMap::AddAlias(
    mir::SlotId alias_slot, mir::SlotId canonical_slot) {
  if (alias_slot == canonical_slot) {
    throw common::InternalError(
        "ForwardingMap::AddAlias", "alias cannot point to itself");
  }
  if (alias_to_canonical_.contains(canonical_slot)) {
    throw common::InternalError(
        "ForwardingMap::AddAlias",
        "canonical slot is itself an alias (chains must be flattened)");
  }
  auto [it, inserted] =
      alias_to_canonical_.try_emplace(alias_slot, canonical_slot);
  if (!inserted && it->second != canonical_slot) {
    throw common::InternalError(
        "ForwardingMap::AddAlias",
        std::format(
            "conflicting canonical for alias slot {}: existing={}, new={}",
            alias_slot.value, it->second.value, canonical_slot.value));
  }
  alias_cache_dirty_ = true;
}

auto ForwardingMap::IsForwardedAlias(mir::SlotId slot) const -> bool {
  return alias_to_canonical_.contains(slot);
}

auto ForwardingMap::IsStorageOwner(mir::SlotId slot) const -> bool {
  return !alias_to_canonical_.contains(slot);
}

auto ForwardingMap::Resolve(mir::SlotId slot) const -> mir::SlotId {
  auto it = alias_to_canonical_.find(slot);
  if (it != alias_to_canonical_.end()) {
    return it->second;
  }
  return slot;
}

void ForwardingMap::RebuildAliasCache() const {
  alias_cache_.clear();
  alias_cache_.reserve(alias_to_canonical_.size());
  for (const auto& [alias, canonical] : alias_to_canonical_) {
    alias_cache_.push_back(alias);
  }
  std::sort(
      alias_cache_.begin(), alias_cache_.end(),
      [](mir::SlotId a, mir::SlotId b) { return a.value < b.value; });
  alias_cache_dirty_ = false;
}

auto ForwardingMap::Aliases() const -> std::span<const mir::SlotId> {
  if (alias_cache_dirty_) {
    RebuildAliasCache();
  }
  return alias_cache_;
}

auto ForwardingMap::ForwardedCount() const -> size_t {
  return alias_to_canonical_.size();
}

auto ForwardingMap::Empty() const -> bool {
  return alias_to_canonical_.empty();
}

}  // namespace lyra::lowering::mir_to_llvm
