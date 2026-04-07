#include "lyra/runtime/back_edge_site_meta.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>

#include "lyra/common/internal_error.hpp"

namespace lyra::runtime {

BackEdgeSiteRegistry::BackEdgeSiteRegistry(
    const uint32_t* words, uint32_t count, const char* pool,
    uint32_t pool_size) {
  if (pool != nullptr && pool_size > 0) {
    auto pool_span = std::span(pool, pool_size);
    string_pool_.assign(pool_span.begin(), pool_span.end());
  }

  auto word_span = std::span(
      words, static_cast<size_t>(count) * back_edge_site_abi::kStride);
  sites_.reserve(count);
  for (uint32_t i = 0; i < count; ++i) {
    auto base = static_cast<size_t>(i) * back_edge_site_abi::kStride;
    uint32_t file_off = word_span[base + back_edge_site_abi::kFieldFileStrOff];
    uint32_t line = word_span[base + back_edge_site_abi::kFieldLine];
    uint32_t col = word_span[base + back_edge_site_abi::kFieldCol];

    sites_.push_back(
        BackEdgeSiteMeta{
            .loc =
                SvLoc{
                    .file_str_off = file_off,
                    .line = line,
                    .col = col,
                },
        });
  }
}

auto BackEdgeSiteRegistry::Size() const -> uint32_t {
  return static_cast<uint32_t>(sites_.size());
}

auto BackEdgeSiteRegistry::IsPopulated() const -> bool {
  return !sites_.empty();
}

auto BackEdgeSiteRegistry::Get(uint32_t site_id) const
    -> const BackEdgeSiteMeta& {
  if (site_id >= sites_.size()) {
    throw common::InternalError(
        "BackEdgeSiteRegistry::Get",
        std::format(
            "site_id {} out of range (size {})", site_id, sites_.size()));
  }
  return sites_[site_id];
}

auto BackEdgeSiteRegistry::PoolString(uint32_t offset) const -> const char* {
  if (string_pool_.empty() || offset >= string_pool_.size()) {
    return "";
  }
  return &string_pool_[offset];
}

auto BackEdgeSiteRegistry::Format(uint32_t site_id) const -> std::string {
  if (site_id >= sites_.size()) {
    return std::format("<back-edge site {}>", site_id);
  }

  const auto& site = sites_[site_id];
  if (site.loc.line == 0) {
    return "<unknown location>";
  }

  const char* file = PoolString(site.loc.file_str_off);
  if (*file == '\0') {
    return std::format("line {}", site.loc.line);
  }
  return std::format("{}:{}:{}", file, site.loc.line, site.loc.col);
}

}  // namespace lyra::runtime
