#pragma once

#include <cstdint>
#include <span>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace lyra::runtime {

class SlotMetaRegistry;

enum class TraceSignalKind : uint8_t { kVariable, kNet, kParam };

struct TraceSignalMeta {
  uint32_t name_str_off = 0;
  uint32_t bit_width = 0;
  TraceSignalKind kind = TraceSignalKind::kVariable;
  // Canonical storage owner slot id. Self for storage owners.
  // For forwarded aliases, points to the canonical owner.
  uint32_t storage_owner_slot_id = 0;
};

// Dense immutable registry of trace signal metadata, indexed by slot_id.
// Constructed once from serialized ABI word table + NUL-terminated string pool.
//
// All accessors are strict: out-of-range slot_id throws InternalError.
// The constructor validates stride, offset bounds, NUL-termination, and
// enum range for every entry.
//
// After construction, alias groups are derived from storage_owner_slot_id:
// entries sharing the same owner are grouped for trace flush alias expansion.
class TraceSignalMetaRegistry {
 public:
  TraceSignalMetaRegistry() = default;

  TraceSignalMetaRegistry(
      const uint32_t* words, uint32_t word_count, const char* pool,
      uint32_t pool_size);

  // Throws InternalError if slot_id >= Count().
  [[nodiscard]] auto Get(uint32_t slot_id) const -> const TraceSignalMeta&;

  // Throws InternalError if slot_id >= Count().
  [[nodiscard]] auto Name(uint32_t slot_id) const -> std::string_view;

  [[nodiscard]] auto Count() const -> size_t;
  [[nodiscard]] auto IsPopulated() const -> bool;

  // Get all slot indices that share the same storage owner as the given
  // owner slot. The returned span includes the owner itself. Returns an
  // empty span if not populated. Throws if populated but owner_slot_id
  // is invalid or missing from alias groups.
  [[nodiscard]] auto GetAliasGroup(uint32_t owner_slot_id) const
      -> std::span<const uint32_t>;

 private:
  [[nodiscard]] auto PoolString(uint32_t offset) const -> const char*;

 public:
  // Build or rebuild alias groups with optional cross-registry validation.
  // When slot_registry is provided, validates that alias members have the
  // same total_bytes as their owner (snapshot reuse invariant).
  // Called once from constructor (with nullptr) and again from
  // Engine::InitTraceSignalMeta (with real slot registry).
  void BuildAliasGroups(const SlotMetaRegistry* slot_registry);

 private:
  std::vector<TraceSignalMeta> metas_;
  std::vector<char> string_pool_;

  // Derived alias groups: owner_slot_id -> sorted list of slot indices
  // sharing that owner (including the owner itself).
  // Built once after construction from storage_owner_slot_id fields.
  std::unordered_map<uint32_t, std::vector<uint32_t>> alias_groups_;
};

}  // namespace lyra::runtime
