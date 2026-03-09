#pragma once

#include <cstdint>
#include <format>
#include <vector>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

using WaitSiteId = uint32_t;
constexpr WaitSiteId kInvalidWaitSiteId = UINT32_MAX;

// Runtime contract for wait-shape classification:
//   kStatic: The engine can refresh the entire installed subscription set
//     without structural recomputation. All installed nodes must be
//     refreshable in place (no rebind nodes, no container element nodes).
//   kRebindable: Signal set is fixed but observation targets may move
//     (late-bound index expressions). Requires full rebuild each activation.
//   kDynamic: Reserved for future cases where trigger topology itself varies.
enum class WaitShapeKind : uint8_t {
  kStatic = 0,
  kRebindable = 1,
  kDynamic = 2,
};

struct CompiledWaitSite {
  WaitSiteId id = kInvalidWaitSiteId;
  uint32_t resume_block = UINT32_MAX;
  WaitShapeKind shape = WaitShapeKind::kDynamic;
  uint32_t num_triggers = 0;
  bool has_late_bound = false;
};

namespace wait_site_abi {

inline constexpr uint32_t kVersion = 1;
inline constexpr uint32_t kStride = 4;

inline constexpr uint32_t kFieldId = 0;
inline constexpr uint32_t kFieldResumeBlock = 1;
inline constexpr uint32_t kFieldShapeAndTriggers = 2;
inline constexpr uint32_t kFieldFlags = 3;

}  // namespace wait_site_abi

class WaitSiteRegistry {
 public:
  WaitSiteRegistry() = default;

  explicit WaitSiteRegistry(const uint32_t* words, uint32_t count) {
    if (words == nullptr || count == 0) return;

    // Non-empty table must have at least a header (version + num_sites).
    if (count < 2) {
      throw common::InternalError(
          "WaitSiteRegistry",
          std::format("table too short for header: {} words", count));
    }

    uint32_t pos = 0;
    uint32_t version = words[pos++];
    if (version != wait_site_abi::kVersion) {
      throw common::InternalError(
          "WaitSiteRegistry",
          std::format("unsupported wait_site_abi version {}", version));
    }

    uint32_t num_sites = words[pos++];
    uint32_t expected_count = 2 + num_sites * wait_site_abi::kStride;
    if (count != expected_count) {
      throw common::InternalError(
          "WaitSiteRegistry",
          std::format(
              "structural size mismatch: expected {} words for {} sites, "
              "got {}",
              expected_count, num_sites, count));
    }

    sites_.reserve(num_sites);
    for (uint32_t i = 0; i < num_sites; ++i) {
      uint32_t id = words[pos + wait_site_abi::kFieldId];
      uint32_t resume_block = words[pos + wait_site_abi::kFieldResumeBlock];
      uint32_t shape_and_triggers =
          words[pos + wait_site_abi::kFieldShapeAndTriggers];
      uint32_t flags = words[pos + wait_site_abi::kFieldFlags];

      // Dense invariant: entry id must equal positional index.
      if (id != i) {
        throw common::InternalError(
            "WaitSiteRegistry",
            std::format(
                "dense id invariant violated: entry {} has id {}", i, id));
      }

      auto shape_raw = shape_and_triggers & 0xFF;
      if (shape_raw > static_cast<uint8_t>(WaitShapeKind::kDynamic)) {
        throw common::InternalError(
            "WaitSiteRegistry",
            std::format("unknown WaitShapeKind {} in entry {}", shape_raw, i));
      }
      auto shape = static_cast<WaitShapeKind>(shape_raw);
      uint32_t num_triggers = shape_and_triggers >> 8;

      // Reject unknown flag bits (only bit 0 = has_late_bound is defined).
      if ((flags & ~1U) != 0) {
        throw common::InternalError(
            "WaitSiteRegistry",
            std::format("unknown flag bits {:#x} in entry {}", flags & ~1U, i));
      }
      bool has_late_bound = (flags & 1) != 0;

      sites_.push_back(
          CompiledWaitSite{
              .id = id,
              .resume_block = resume_block,
              .shape = shape,
              .num_triggers = num_triggers,
              .has_late_bound = has_late_bound});
      pos += wait_site_abi::kStride;
    }
  }

  [[nodiscard]] auto Get(WaitSiteId id) const -> const CompiledWaitSite& {
    if (id >= sites_.size()) {
      throw common::InternalError(
          "WaitSiteRegistry::Get",
          std::format("wait_site_id {} >= size {}", id, sites_.size()));
    }
    return sites_[id];
  }

  [[nodiscard]] auto Size() const -> uint32_t {
    return static_cast<uint32_t>(sites_.size());
  }

  [[nodiscard]] auto IsPopulated() const -> bool {
    return !sites_.empty();
  }

 private:
  std::vector<CompiledWaitSite> sites_;
};

}  // namespace lyra::runtime
