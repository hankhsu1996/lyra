#include "lyra/common/source_span.hpp"

#include <format>

namespace lyra {

auto FormatSourceLocation(const SourceSpan& span, const SourceManager& mgr)
    -> std::string {
  if (!span.file_id) {
    return "";
  }

  const FileInfo* file = mgr.GetFile(span.file_id);
  if (file == nullptr) {
    return "";
  }

  // Count line and column by scanning content up to span.begin
  uint32_t line = 1;
  uint32_t col = 1;
  const std::string& content = file->content;
  for (uint32_t i = 0; i < span.begin && i < content.size(); ++i) {
    if (content[i] == '\n') {
      ++line;
      col = 1;
    } else {
      ++col;
    }
  }

  return std::format("{}:{}:{}", file->path, line, col);
}

}  // namespace lyra
