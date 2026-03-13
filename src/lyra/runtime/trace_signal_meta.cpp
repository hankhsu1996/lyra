#include "lyra/runtime/trace_signal_meta.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <string_view>

#include "lyra/common/internal_error.hpp"
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
        });
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
