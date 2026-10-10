#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/diag/kind.hpp"
#include "lyra/diag/source_span.hpp"

namespace lyra::diag {

// Where a piece of the source is, as a reader is told it.
struct SourcePosition {
  std::string file;
  std::uint32_t line = 0;
  std::uint32_t column = 0;
};

// What may be asked about a piece of the source by something that only holds
// one. The front end keeps the text, the lines and what each macro expanded
// to, so it is the front end that answers; this is the shape of the question,
// for the layers that do not know which front end that is.
class SourceManager {
 public:
  SourceManager() = default;
  SourceManager(const SourceManager&) = default;
  auto operator=(const SourceManager&) -> SourceManager& = default;
  SourceManager(SourceManager&&) = default;
  auto operator=(SourceManager&&) -> SourceManager& = default;
  virtual ~SourceManager() = default;

  // The file, line and column `span` starts at. Nothing where it is no place.
  [[nodiscard]] virtual auto PositionOf(SourceSpan span) const
      -> std::optional<SourcePosition> = 0;

  // A message as it is shown. One about a piece of the source says where that
  // is, shows the line it is on, and says how the text came to be there where
  // a macro or an included file put it; one about no place is the message
  // alone, in the same form.
  [[nodiscard]] virtual auto Show(
      DiagKind kind, SourceSpan span, std::string_view message,
      bool use_color) const -> std::string = 0;
};

}  // namespace lyra::diag
