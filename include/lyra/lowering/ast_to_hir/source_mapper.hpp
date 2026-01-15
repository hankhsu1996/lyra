#pragma once

#include <cassert>
#include <unordered_map>

#include <slang/text/SourceLocation.h>

#include "lyra/common/source_manager.hpp"
#include "lyra/common/source_span.hpp"

namespace lyra::lowering::ast_to_hir {

class SourceMapper {
 public:
  void Register(slang::BufferID buffer, FileId file_id) {
    buffer_to_file_[buffer] = file_id;
  }

  [[nodiscard]] auto Contains(slang::BufferID buffer) const -> bool {
    return buffer_to_file_.contains(buffer);
  }

  [[nodiscard]] auto SpanOf(slang::SourceRange range) const -> SourceSpan {
    slang::BufferID buffer = range.start().buffer();
    auto it = buffer_to_file_.find(buffer);
    assert(it != buffer_to_file_.end() && "buffer must be registered");
    return SourceSpan{
        .file_id = it->second,
        .begin = static_cast<uint32_t>(range.start().offset()),
        .end = static_cast<uint32_t>(range.end().offset()),
    };
  }

 private:
  std::unordered_map<slang::BufferID, FileId> buffer_to_file_;
};

}  // namespace lyra::lowering::ast_to_hir
