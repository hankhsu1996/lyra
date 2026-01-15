#pragma once

#include <cstdint>

namespace lyra {

struct SourceSpan {
  uint32_t file_id = 0;
  uint32_t begin = 0;
  uint32_t end = 0;

  auto operator==(const SourceSpan&) const -> bool = default;
};

}  // namespace lyra
