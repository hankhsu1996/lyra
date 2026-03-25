#include "lyra/realization/build_design_metadata.hpp"

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/back_edge_site_meta.hpp"

namespace lyra::realization {

namespace {

auto BuildBackEdgeSiteMeta(
    const std::vector<metadata::BackEdgeSiteInput>& sites)
    -> metadata::MetaWordTable {
  if (sites.empty()) {
    return {};
  }

  std::vector<char> pool;
  pool.push_back('\0');

  auto add_string = [&](const std::string& s) -> uint32_t {
    if (s.empty()) return 0;
    auto off = static_cast<uint32_t>(pool.size());
    pool.insert(pool.end(), s.begin(), s.end());
    pool.push_back('\0');
    return off;
  };

  std::vector<uint32_t> words;
  words.reserve(sites.size() * runtime::back_edge_site_abi::kStride);

  for (const auto& site : sites) {
    uint32_t file_off = add_string(site.file);
    words.push_back(file_off);
    words.push_back(site.line);
    words.push_back(site.col);
  }

  return {.words = std::move(words), .pool = std::move(pool)};
}

}  // namespace

auto BuildDesignMetadata(const metadata::DesignMetadataInputs& input)
    -> metadata::DesignMetadata {
  // Verify back_edge_site_index values are dense and monotonic.
  for (uint32_t i = 0; i < input.back_edge_sites.size(); ++i) {
    if (input.back_edge_sites[i].back_edge_site_index != i) {
      throw common::InternalError(
          "BuildDesignMetadata", "back_edge_site_index not dense/monotonic");
    }
  }

  auto back_edge_site_meta = BuildBackEdgeSiteMeta(input.back_edge_sites);

  if (!back_edge_site_meta.words.empty() &&
      back_edge_site_meta.words.size() % runtime::back_edge_site_abi::kStride !=
          0) {
    throw common::InternalError(
        "BuildDesignMetadata",
        "back_edge_site_meta words size not divisible by kStride");
  }

  return metadata::DesignMetadata{
      .back_edge_site_meta = std::move(back_edge_site_meta),
      .connection_descriptors = input.connection_descriptors,
  };
}

}  // namespace lyra::realization
