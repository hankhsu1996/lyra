#include "lyra/diag/render.hpp"

#include <exception>
#include <format>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"

namespace {

using lyra::diag::DiagCode;
using lyra::diag::DiagKind;
using lyra::diag::RenderOptions;
using lyra::diag::SourceSpan;

// Stands for whatever read the source, and shows a message by writing down
// everything it was asked to show.
class AsksRecorded final : public lyra::diag::SourceManager {
 public:
  [[nodiscard]] auto PositionOf(SourceSpan span) const
      -> std::optional<lyra::diag::SourcePosition> override {
    if (span == SourceSpan{}) {
      return std::nullopt;
    }
    return lyra::diag::SourcePosition{
        .file = "main.sv", .line = 1, .column = 1};
  }

  [[nodiscard]] auto Show(
      DiagKind kind, SourceSpan span, std::string_view message,
      bool use_color) const -> std::string override {
    return std::format(
        "{} at {}..{}{}: {}\n", NameOf(kind), span.start, span.end,
        use_color ? " in color" : "", message);
  }

 private:
  static auto NameOf(DiagKind kind) -> std::string_view {
    switch (kind) {
      case DiagKind::kError:
        return "error";
      case DiagKind::kUnsupported:
        return "unsupported";
      case DiagKind::kHostError:
        return "host error";
      case DiagKind::kInternalError:
        return "internal error";
      case DiagKind::kWarning:
        return "warning";
      case DiagKind::kNote:
        return "note";
      case DiagKind::kRemark:
        return "remark";
    }
    std::unreachable();
  }
};

auto Shown(const lyra::diag::Diagnostic& diag, RenderOptions opts = {})
    -> std::string {
  const AsksRecorded sources;
  opts.use_color = false;
  return lyra::diag::RenderDiagnostic(diag, sources, opts);
}

auto Shown(const lyra::diag::DiagnosticSink& sink, RenderOptions opts = {})
    -> std::string {
  const AsksRecorded sources;
  opts.use_color = false;
  return lyra::diag::RenderDiagnostics(sink, sources, opts);
}

// Every message is handed over whole to whatever read the source: its kind,
// its place as it was given, and what it says. One about no place is handed
// over the same way, as one about the place that is none, and a note follows
// the message it is on.
TEST(DiagRender, EveryMessageIsShownByWhateverReadTheSource) {
  const SourceSpan statement{.start = 41, .end = 52};
  const SourceSpan declaration{.start = 7, .end = 7};
  EXPECT_EQ(
      Shown(
          lyra::diag::Make(
              statement, DiagCode::kUnsupportedTypeKind,
              "this type is not yet supported")
              .WithNote(declaration, "declared here")
              .WithNote("nothing to point at")),
      "unsupported at 41..52: this type is not yet supported\n"
      "note at 7..7: declared here\n"
      "note at 0..0: nothing to point at\n");

  EXPECT_EQ(
      Shown(lyra::diag::Make(DiagCode::kHostNoInputFiles, "no input files")),
      "host error at 0..0: no input files\n");
}

// Whether color is used is the asker's to say.
TEST(DiagRender, ColorIsAskedForAndNeverAdded) {
  const AsksRecorded sources;
  EXPECT_EQ(
      lyra::diag::RenderDiagnostic(
          lyra::diag::Make(DiagCode::kHostIoError, "boom"), sources,
          RenderOptions{.use_color = true}),
      "host error at 0..0 in color: boom\n");
}

// The exception carries the invariant that was violated and the fact that it is
// a bug; saying it is an internal error belongs to whichever surface reports
// it, once, at a place or at none. The request to report it follows the count.
TEST(DiagRender, AnInternalErrorSaysSoOnce) {
  lyra::diag::DiagnosticSink sink;
  sink.Report(
      lyra::diag::InternalFailure(
          lyra::InternalError("llvm codegen: no runtime domain")));
  try {
    const lyra::diag::FailureContext statement(
        SourceSpan{.start = 3, .end = 9});
    throw lyra::InternalError("an invariant broke");
  } catch (const std::exception& failure) {
    sink.Report(lyra::diag::InternalFailure(failure));
  }
  EXPECT_EQ(
      Shown(sink),
      "internal error at 0..0: internal error: llvm codegen: no runtime "
      "domain\n"
      "internal error at 3..9: internal error: an invariant broke\n"
      "2 errors generated.\n"
      "This is a bug in Lyra. Please report at: "
      "https://github.com/hankhsu1996/lyra/issues\n");
}

TEST(DiagRender, SinkSummaryAggregatesCounts) {
  lyra::diag::DiagnosticSink sink;
  sink.Report(
      lyra::diag::Make(
          DiagCode::kUnsupportedStatementForm,
          "feature A is not supported yet"));
  sink.Report(lyra::diag::Make(DiagCode::kHostIoError, "cannot read 'foo.sv'"));
  sink.Report(lyra::diag::Make(DiagCode::kWarningPedantic, "pedantic"));

  EXPECT_TRUE(Shown(sink).ends_with("1 warning and 2 errors generated.\n"));
  EXPECT_TRUE(sink.HasErrors());
}

TEST(DiagRender, WarningsAndRemarksReachTheReportOnlyWhenShown) {
  lyra::diag::DiagnosticSink sink;
  sink.Report(
      lyra::diag::Make(DiagCode::kRemarkLostSharing, "could have shared"));
  EXPECT_FALSE(sink.HasErrors());

  EXPECT_EQ(Shown(sink), "");
  EXPECT_EQ(
      Shown(sink, RenderOptions{.show_remarks = true}),
      "remark at 0..0: could have shared\n");

  // A warning left out is left out of the count as well.
  sink.Report(lyra::diag::Make(DiagCode::kWarningPedantic, "pedantic"));
  EXPECT_EQ(Shown(sink, RenderOptions{.show_warnings = false}), "");
}

TEST(DiagRender, SinkEmptyHasNoSummary) {
  EXPECT_EQ(Shown(lyra::diag::DiagnosticSink{}), "");
}

TEST(DiagRender, KindDerivesFromCode) {
  using lyra::diag::DiagCodeKind;
  using lyra::diag::Make;

  EXPECT_EQ(
      Make(DiagCode::kUnsupportedTypeKind, "x").primary.kind,
      DiagKind::kUnsupported);
  EXPECT_EQ(
      Make(DiagCode::kErrorCaseEqualityOnRealOperand, "x").primary.kind,
      DiagKind::kError);
  EXPECT_EQ(
      Make(DiagCode::kHostIoError, "x").primary.kind, DiagKind::kHostError);
  EXPECT_EQ(
      Make(DiagCode::kWarningPedantic, "x").primary.kind, DiagKind::kWarning);
  EXPECT_EQ(
      Make(DiagCode::kRemarkLostSharing, "x").primary.kind, DiagKind::kRemark);

  for (const DiagCode code :
       {DiagCode::kUnsupportedTypeKind,
        DiagCode::kErrorCaseEqualityOnRealOperand, DiagCode::kHostIoError,
        DiagCode::kWarningPedantic, DiagCode::kRemarkLostSharing}) {
    EXPECT_EQ(Make(code, "x").primary.kind, DiagCodeKind(code));
  }
}

}  // namespace
