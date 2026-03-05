#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/runtime/process_meta.hpp"

namespace lyra::runtime {

struct LoopSiteMeta {
  SvLoc loc;
};

namespace loop_site_meta_abi {

inline constexpr uint32_t kVersion = 1;

// Words per entry: [file_str_off, line, col]
inline constexpr uint32_t kStride = 3;

inline constexpr uint32_t kFieldFileStrOff = 0;
inline constexpr uint32_t kFieldLine = 1;
inline constexpr uint32_t kFieldCol = 2;

}  // namespace loop_site_meta_abi

class LoopSiteRegistry {
 public:
  LoopSiteRegistry() = default;

  LoopSiteRegistry(
      const uint32_t* words, uint32_t count, const char* pool,
      uint32_t pool_size);

  [[nodiscard]] auto Size() const -> uint32_t;
  [[nodiscard]] auto IsPopulated() const -> bool;
  [[nodiscard]] auto Get(uint32_t site_id) const -> const LoopSiteMeta&;
  [[nodiscard]] auto Format(uint32_t site_id) const -> std::string;

 private:
  [[nodiscard]] auto PoolString(uint32_t offset) const -> const char*;

  std::vector<LoopSiteMeta> sites_;
  std::vector<char> string_pool_;
};

}  // namespace lyra::runtime
