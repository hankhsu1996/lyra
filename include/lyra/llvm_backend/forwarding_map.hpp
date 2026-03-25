#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <span>
#include <unordered_map>
#include <vector>

#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

// Single canonicalization result for the forwarding pipeline.
// Slot/canonical-storage based only. No connection-kernel concepts in the API.
//
// For this cut, connection-kernel representation is only the discovery
// substrate. ForwardingMap itself is the long-term canonicalization object.
class ForwardingMap {
 public:
  ForwardingMap() = default;

  // Add a forwarding alias. alias_slot is forwarded to canonical_slot.
  // Caller must flatten transitive chains before adding.
  // Throws InternalError if alias_slot == canonical_slot, if canonical_slot
  // is itself an alias, or if alias_slot is already mapped to a different
  // canonical.
  void AddAlias(mir::SlotId alias_slot, mir::SlotId canonical_slot);

  // Check if a slot is a forwarded alias (not a storage owner).
  [[nodiscard]] auto IsForwardedAlias(mir::SlotId slot) const -> bool;

  // Check if a slot is a storage owner (not a forwarded alias).
  // Equivalent to !IsForwardedAlias(slot).
  [[nodiscard]] auto IsStorageOwner(mir::SlotId slot) const -> bool;

  // Resolve to canonical storage owner. Returns input unchanged if not
  // forwarded.
  [[nodiscard]] auto Resolve(mir::SlotId slot) const -> mir::SlotId;

  // All forwarded alias slot ids, in deterministic sorted order.
  [[nodiscard]] auto Aliases() const -> std::span<const mir::SlotId>;

  [[nodiscard]] auto ForwardedCount() const -> size_t;

  [[nodiscard]] auto Empty() const -> bool;

 private:
  struct SlotHash {
    auto operator()(mir::SlotId id) const noexcept -> size_t {
      return std::hash<uint32_t>{}(id.value);
    }
  };

  std::unordered_map<mir::SlotId, mir::SlotId, SlotHash> alias_to_canonical_;

  // Cached sorted list of alias slot ids for Aliases() span.
  mutable std::vector<mir::SlotId> alias_cache_;
  mutable bool alias_cache_dirty_ = false;

  void RebuildAliasCache() const;
};

}  // namespace lyra::lowering::mir_to_llvm
