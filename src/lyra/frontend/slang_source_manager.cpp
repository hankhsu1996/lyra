#include "lyra/frontend/slang_source_manager.hpp"

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

#include <slang/diagnostics/DiagnosticEngine.h>
#include <slang/diagnostics/Diagnostics.h>
#include <slang/diagnostics/TextDiagnosticClient.h>
#include <slang/text/SourceLocation.h>
#include <slang/text/SourceManager.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/frontend/slang_source_span.hpp"

namespace lyra::frontend {

namespace {

// The code a message of Lyra's is issued under. slang knows a message by its
// code, and one it has no entry for is ignored until it is given a text and a
// severity, so this is one slang has none for: the text is whatever Lyra says,
// and the severity is set for each message as it is shown.
constexpr slang::DiagCode kLyraMessage{
    slang::DiagSubsystem::General, UINT16_MAX};

// slang says a message is one of four things, and those are the words it
// shows. What more Lyra distinguishes is said in the message itself.
auto SeverityOf(diag::DiagKind kind) -> slang::DiagnosticSeverity {
  switch (kind) {
    case diag::DiagKind::kError:
    case diag::DiagKind::kUnsupported:
    case diag::DiagKind::kHostError:
    case diag::DiagKind::kInternalError:
      return slang::DiagnosticSeverity::Error;
    case diag::DiagKind::kWarning:
      return slang::DiagnosticSeverity::Warning;
    case diag::DiagKind::kNote:
    case diag::DiagKind::kRemark:
      return slang::DiagnosticSeverity::Note;
  }
  throw InternalError("SeverityOf: invalid DiagKind");
}

}  // namespace

// One engine and one printer for as long as the source is answered for, so
// what slang says once for a run of messages -- which file included the one
// they are in -- is said once here too.
struct SlangSourceManager::Printer {
  explicit Printer(const slang::SourceManager& sources) : engine(sources) {
    engine.addClient(client);
    engine.setMessage(kLyraMessage, "{}");
  }

  slang::DiagnosticEngine engine;
  std::shared_ptr<slang::TextDiagnosticClient> client =
      std::make_shared<slang::TextDiagnosticClient>();
};

SlangSourceManager::SlangSourceManager(const slang::SourceManager& sources)
    : sources_(&sources), printer_(std::make_shared<Printer>(sources)) {
}

auto SlangSourceManager::PositionOf(diag::SourceSpan span) const
    -> std::optional<diag::SourcePosition> {
  const slang::SourceLocation written = UnpackedLocation(span.start);
  if (!written.valid()) {
    return std::nullopt;
  }
  const slang::SourceLocation in_a_file = sources_->getFileLoc(written);
  return diag::SourcePosition{
      .file = std::string(sources_->getFileName(in_a_file)),
      .line = static_cast<std::uint32_t>(sources_->getLineNumber(in_a_file)),
      .column =
          static_cast<std::uint32_t>(sources_->getColumnNumber(in_a_file))};
}

auto SlangSourceManager::Show(
    diag::DiagKind kind, diag::SourceSpan span, std::string_view message,
    bool use_color) const -> std::string {
  // No place is a place slang has a name for, and it shows a message there as
  // it shows any other, without the lines that say where.
  const slang::SourceLocation written = UnpackedLocation(span.start);
  const slang::SourceLocation start =
      written.valid() ? written : slang::SourceLocation::NoLocation;

  printer_->client->clear();
  printer_->client->showColors(use_color);
  printer_->engine.setSeverity(kLyraMessage, SeverityOf(kind));

  slang::Diagnostic shown(kLyraMessage, start);
  shown << message;
  if (span.end != span.start) {
    shown << slang::SourceRange{start, UnpackedLocation(span.end)};
  }
  printer_->engine.issue(shown);
  return printer_->client->getString();
}

}  // namespace lyra::frontend
