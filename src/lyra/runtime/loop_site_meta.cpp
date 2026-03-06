#include "lyra/runtime/loop_site_meta.hpp"

#include <cstdint>
#include <format>
#include <string>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

LoopSiteRegistry::LoopSiteRegistry(
    const uint32_t* words, uint32_t count, const char* pool,
    uint32_t pool_size) {
  if (pool != nullptr && pool_size > 0) {
    string_pool_.assign(pool, pool + pool_size);
  }

  sites_.reserve(count);
  for (uint32_t i = 0; i < count; ++i) {
    auto base = static_cast<size_t>(i) * loop_site_meta_abi::kStride;
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    uint32_t file_off = words[base + loop_site_meta_abi::kFieldFileStrOff];
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    uint32_t line = words[base + loop_site_meta_abi::kFieldLine];
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    uint32_t col = words[base + loop_site_meta_abi::kFieldCol];

    sites_.push_back(
        LoopSiteMeta{
            .loc =
                SvLoc{
                    .file_str_off = file_off,
                    .line = line,
                    .col = col,
                },
        });
  }
}

auto LoopSiteRegistry::Size() const -> uint32_t {
  return static_cast<uint32_t>(sites_.size());
}

auto LoopSiteRegistry::IsPopulated() const -> bool {
  return !sites_.empty();
}

auto LoopSiteRegistry::Get(uint32_t site_id) const -> const LoopSiteMeta& {
  if (site_id >= sites_.size()) {
    throw common::InternalError(
        "LoopSiteRegistry::Get",
        std::format(
            "site_id {} out of range (size {})", site_id, sites_.size()));
  }
  return sites_[site_id];
}

auto LoopSiteRegistry::PoolString(uint32_t offset) const -> const char* {
  if (string_pool_.empty() || offset >= string_pool_.size()) {
    return "";
  }
  return string_pool_.data() + offset;  // NOLINT
}

auto LoopSiteRegistry::Format(uint32_t site_id) const -> std::string {
  if (site_id >= sites_.size()) {
    return std::format("<loop site {}>", site_id);
  }

  const auto& site = sites_[site_id];
  if (site.loc.line == 0) {
    return "<unknown location>";
  }

  const char* file = PoolString(site.loc.file_str_off);
  if (file[0] == '\0') {
    return std::format("line {}", site.loc.line);
  }
  return std::format("{}:{}:{}", file, site.loc.line, site.loc.col);
}

}  // namespace lyra::runtime
