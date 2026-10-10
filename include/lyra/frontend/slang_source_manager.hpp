#pragma once

#include <memory>
#include <optional>
#include <string>
#include <string_view>

#include <slang/text/SourceManager.h>

#include "lyra/diag/kind.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"

namespace lyra::frontend {

// slang answering for the source it read. Every answer is slang's own: where a
// place is in a file, and a message shown the way slang shows its own, so one
// run tells its reader about every place the same way. The manager is slang's
// and has to outlive this.
class SlangSourceManager final : public diag::SourceManager {
 public:
  explicit SlangSourceManager(const slang::SourceManager& sources);

  [[nodiscard]] auto PositionOf(diag::SourceSpan span) const
      -> std::optional<diag::SourcePosition> override;

  [[nodiscard]] auto Show(
      diag::DiagKind kind, diag::SourceSpan span, std::string_view message,
      bool use_color) const -> std::string override;

 private:
  struct Printer;

  const slang::SourceManager* sources_;
  std::shared_ptr<Printer> printer_;
};

}  // namespace lyra::frontend
