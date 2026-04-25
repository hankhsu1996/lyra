#pragma once

#include <cstdint>
#include <vector>

#include <slang/text/SourceLocation.h>

#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"

namespace lyra::frontend {

// Maps slang BufferID to Lyra diagnostic FileId. slang BufferID is dense
// within one slang SourceManager, so vector indexing is intentional.
class SlangSourceMapper {
 public:
  void Register(slang::BufferID buffer, diag::FileId file_id) {
    const auto idx = buffer.getId();
    if (idx >= file_ids_.size()) {
      file_ids_.resize(idx + 1, diag::kInvalidFileId);
    }
    file_ids_[idx] = file_id;
  }

  [[nodiscard]] auto Contains(slang::BufferID buffer) const -> bool {
    const auto idx = buffer.getId();
    return idx < file_ids_.size() && static_cast<bool>(file_ids_[idx]);
  }

  [[nodiscard]] auto SpanOf(slang::SourceRange range) const
      -> diag::SourceSpan {
    const auto idx = range.start().buffer().getId();
    if (idx >= file_ids_.size()) {
      return diag::SourceSpan{};
    }
    const auto fid = file_ids_[idx];
    if (!fid) {
      return diag::SourceSpan{};
    }
    return diag::SourceSpan{
        .file_id = fid,
        .begin = static_cast<std::uint32_t>(range.start().offset()),
        .end = static_cast<std::uint32_t>(range.end().offset()),
    };
  }

  [[nodiscard]] auto PointSpanOf(slang::SourceLocation loc) const
      -> diag::SourceSpan {
    return SpanOf(slang::SourceRange{loc, loc});
  }

 private:
  std::vector<diag::FileId> file_ids_;
};

}  // namespace lyra::frontend
