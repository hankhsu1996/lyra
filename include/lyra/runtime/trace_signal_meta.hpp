#pragma once

#include <cstdint>
#include <string_view>
#include <vector>

namespace lyra::runtime {

enum class TraceSignalKind : uint8_t { kVariable, kNet, kParam };

struct TraceSignalMeta {
  uint32_t name_str_off = 0;
  uint32_t bit_width = 0;
  TraceSignalKind kind = TraceSignalKind::kVariable;
};

// Dense immutable registry of trace signal metadata, indexed by slot_id.
// Constructed once from serialized ABI word table + NUL-terminated string pool.
//
// All accessors are strict: out-of-range slot_id throws InternalError.
// The constructor validates stride, offset bounds, NUL-termination, and
// enum range for every entry.
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

 private:
  [[nodiscard]] auto PoolString(uint32_t offset) const -> const char*;

  std::vector<TraceSignalMeta> metas_;
  std::vector<char> string_pool_;
};

}  // namespace lyra::runtime
