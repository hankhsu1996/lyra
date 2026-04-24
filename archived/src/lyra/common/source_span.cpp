#include "lyra/common/source_span.hpp"

#include <format>
#include <string>

#include "lyra/common/source_manager.hpp"

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

  auto [line, col] = mgr.OffsetToLineCol(span.file_id, span.begin);
  if (line == 0) {
    return "";
  }

  return std::format("{}:{}:{}", file->path, line, col);
}

}  // namespace lyra
