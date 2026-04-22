#pragma once

#include <cstdint>
#include <string>

#include "lyra/common/source_manager.hpp"

namespace lyra {

struct SourceSpan {
  FileId file_id;
  uint32_t begin = 0;
  uint32_t end = 0;

  auto operator==(const SourceSpan&) const -> bool = default;
};

// Format a SourceSpan as "file:line:col" using the SourceManager.
// Returns empty string if the span or file is invalid.
auto FormatSourceLocation(const SourceSpan& span, const SourceManager& mgr)
    -> std::string;

}  // namespace lyra
