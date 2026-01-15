#pragma once

#include <cstdint>

#include "lyra/common/source_manager.hpp"

namespace lyra {

struct SourceSpan {
  FileId file_id;
  uint32_t begin = 0;
  uint32_t end = 0;

  auto operator==(const SourceSpan&) const -> bool = default;
};

}  // namespace lyra
