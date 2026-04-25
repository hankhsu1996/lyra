#pragma once

#include <cstdint>
#include <string>

#include "lyra/diag/source_manager.hpp"

namespace lyra::diag {

// A half-open byte range [begin, end) within a file.
struct SourceSpan {
  FileId file_id;
  std::uint32_t begin = 0;
  std::uint32_t end = 0;

  auto operator==(const SourceSpan&) const -> bool = default;
};

// Format as "<path>:<line>:<col>". Returns empty string when the span has no
// resolvable file.
auto FormatSourceLocation(const SourceSpan& span, const SourceManager& mgr)
    -> std::string;

}  // namespace lyra::diag
