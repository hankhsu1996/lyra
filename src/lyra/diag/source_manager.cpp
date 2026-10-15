#include "lyra/diag/source_manager.hpp"

#include <filesystem>
#include <format>
#include <string>

#include "lyra/diag/source_span.hpp"

namespace lyra::diag {

auto RuntimeOriginOf(SourceSpan span, const SourceManager& sources)
    -> std::string {
  const auto at = sources.PositionOf(span);
  if (!at) return {};
  return std::format(
      "{}:{}:{}", std::filesystem::path{at->file}.filename().string(), at->line,
      at->column);
}

}  // namespace lyra::diag
