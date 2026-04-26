#include "lyra/diag/render.hpp"

#include <cstdint>
#include <gtest/gtest.h>
#include <string>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"

namespace {

constexpr char kAnsiEsc = '\x1b';

auto Has(const std::string& s, const std::string& needle) -> bool {
  return s.find(needle) != std::string::npos;
}

TEST(DiagRender, HostErrorPlain) {
  const auto out = lyra::diag::RenderDiagnostic(
      lyra::diag::Diagnostic::HostError(
          lyra::diag::DiagCode::kHostNoInputFiles, "no input files"),
      nullptr, lyra::diag::RenderOptions{.use_color = false});
  EXPECT_EQ(out, "lyra: error: no input files\n");
}

TEST(DiagRender, HostErrorColored) {
  const auto out = lyra::diag::RenderDiagnostic(
      lyra::diag::Diagnostic::HostError(
          lyra::diag::DiagCode::kHostIoError, "boom"),
      nullptr, lyra::diag::RenderOptions{.use_color = true});
  EXPECT_TRUE(Has(out, "lyra"));
  EXPECT_TRUE(Has(out, "error:"));
  EXPECT_TRUE(Has(out, "boom"));
  EXPECT_NE(out.find(kAnsiEsc), std::string::npos);
}

TEST(DiagRender, InternalErrorIsAlwaysPlain) {
  const auto out = lyra::diag::RenderInternalError("invariant violated");
  EXPECT_EQ(out, "lyra: internal error: invariant violated\n");
}

TEST(DiagRender, UnsupportedWithUnknownSpan) {
  const auto diag = lyra::diag::Diagnostic::Unsupported(
      lyra::diag::DiagCode::kUnsupportedForGenerate,
      "for-generate is not supported yet",
      lyra::diag::UnsupportedCategory::kFeature);
  const auto out = lyra::diag::RenderDiagnostic(
      diag, nullptr, lyra::diag::RenderOptions{.use_color = false});
  EXPECT_EQ(out, "lyra: unsupported: for-generate is not supported yet\n");
}

TEST(DiagRender, ErrorWithSpanIncludesSnippetAndLocation) {
  lyra::diag::SourceManager mgr;
  const std::string content = "module M;\n  bit [7:0] x;\nendmodule\n";
  const auto fid = mgr.AddFile("main.sv", content);

  const auto begin = static_cast<std::uint32_t>(content.find("bit [7:0] x"));
  const auto end = begin + 11;
  const lyra::diag::SourceSpan span{.file_id = fid, .begin = begin, .end = end};

  const auto diag = lyra::diag::Diagnostic::Unsupported(
      span, lyra::diag::DiagCode::kUnsupportedTypeKind,
      "only `int` and `logic` types are supported",
      lyra::diag::UnsupportedCategory::kType);
  const auto out = lyra::diag::RenderDiagnostic(
      diag, &mgr, lyra::diag::RenderOptions{.use_color = false});

  EXPECT_TRUE(Has(out, "main.sv:2:3:"));
  EXPECT_TRUE(Has(out, "unsupported:"));
  EXPECT_TRUE(Has(out, "only `int` and `logic` types are supported"));
  EXPECT_TRUE(Has(out, "bit [7:0] x;"));
  EXPECT_TRUE(Has(out, "^"));
  EXPECT_EQ(out.find(kAnsiEsc), std::string::npos);
}

TEST(DiagRender, ColoredOutputContainsAnsi) {
  lyra::diag::SourceManager mgr;
  const auto fid = mgr.AddFile("main.sv", "int x;\n");
  const lyra::diag::SourceSpan span{.file_id = fid, .begin = 0, .end = 3};

  const auto out = lyra::diag::RenderDiagnostic(
      lyra::diag::Diagnostic::HostError(
          span, lyra::diag::DiagCode::kHostIoError, "boom"),
      &mgr, lyra::diag::RenderOptions{.use_color = true});

  EXPECT_NE(out.find(kAnsiEsc), std::string::npos);
}

TEST(DiagRender, NoSnippetWhenDisabled) {
  lyra::diag::SourceManager mgr;
  const auto fid = mgr.AddFile("main.sv", "int x;\n");
  const lyra::diag::SourceSpan span{.file_id = fid, .begin = 0, .end = 3};
  const auto out = lyra::diag::RenderDiagnostic(
      lyra::diag::Diagnostic::HostError(
          span, lyra::diag::DiagCode::kHostIoError, "boom"),
      &mgr,
      lyra::diag::RenderOptions{
          .use_color = false, .show_source_snippet = false});
  EXPECT_TRUE(Has(out, "main.sv:1:1:"));
  EXPECT_TRUE(Has(out, "boom"));
  EXPECT_FALSE(Has(out, "int x;\n  "));
  EXPECT_FALSE(Has(out, "^"));
}

TEST(DiagRender, SinkSummaryAggregatesCounts) {
  lyra::diag::DiagnosticSink sink;
  sink.Report(
      lyra::diag::Diagnostic::Unsupported(
          lyra::diag::DiagCode::kUnsupportedForGenerate,
          "feature A is not supported yet",
          lyra::diag::UnsupportedCategory::kFeature));
  sink.Report(
      lyra::diag::Diagnostic::HostError(
          lyra::diag::DiagCode::kHostIoError, "cannot read 'foo.sv'"));
  sink.Report(
      lyra::diag::Diagnostic::Warning(
          lyra::diag::SourceSpan{}, lyra::diag::DiagCode::kWarningPedantic,
          "pedantic"));

  const auto out = lyra::diag::RenderDiagnostics(
      sink, nullptr, lyra::diag::RenderOptions{.use_color = false});
  EXPECT_TRUE(Has(out, "1 warning and 2 errors generated.\n"));
  EXPECT_TRUE(sink.HasErrors());
}

TEST(DiagRender, SinkEmptyHasNoSummary) {
  lyra::diag::DiagnosticSink sink;
  const auto out = lyra::diag::RenderDiagnostics(
      sink, nullptr, lyra::diag::RenderOptions{.use_color = false});
  EXPECT_EQ(out, "");
}

TEST(DiagRender, NoteAttachesAfterPrimary) {
  auto diag =
      lyra::diag::Diagnostic::Unsupported(
          lyra::diag::DiagCode::kUnsupportedForGenerate,
          "feature is not supported", lyra::diag::UnsupportedCategory::kFeature)
          .WithNote("see related design discussion");
  const auto out = lyra::diag::RenderDiagnostic(
      diag, nullptr, lyra::diag::RenderOptions{.use_color = false});
  const auto primary_pos = out.find("unsupported:");
  const auto note_pos = out.find("note:");
  ASSERT_NE(primary_pos, std::string::npos);
  ASSERT_NE(note_pos, std::string::npos);
  EXPECT_LT(primary_pos, note_pos);
}

TEST(DiagRender, UnsupportedCategoryPreservedAcrossKinds) {
  const auto type_diag = lyra::diag::Diagnostic::Unsupported(
      lyra::diag::SourceSpan{}, lyra::diag::DiagCode::kUnsupportedTypeKind,
      "wide types not supported", lyra::diag::UnsupportedCategory::kType);
  ASSERT_TRUE(type_diag.primary.category.has_value());
  EXPECT_EQ(
      *type_diag.primary.category, lyra::diag::UnsupportedCategory::kType);

  const auto op_diag = lyra::diag::Diagnostic::Unsupported(
      lyra::diag::DiagCode::kUnsupportedExpressionForm, "weird expression form",
      lyra::diag::UnsupportedCategory::kOperation);
  ASSERT_TRUE(op_diag.primary.category.has_value());
  EXPECT_EQ(
      *op_diag.primary.category, lyra::diag::UnsupportedCategory::kOperation);

  const auto feature_diag = lyra::diag::Diagnostic::Unsupported(
      lyra::diag::DiagCode::kUnsupportedForGenerate, "language feature gap",
      lyra::diag::UnsupportedCategory::kFeature);
  ASSERT_TRUE(feature_diag.primary.category.has_value());
  EXPECT_EQ(
      *feature_diag.primary.category,
      lyra::diag::UnsupportedCategory::kFeature);

  EXPECT_FALSE(
      lyra::diag::Diagnostic::HostError(
          lyra::diag::DiagCode::kHostIoError, "io")
          .primary.category.has_value());
  EXPECT_FALSE(
      lyra::diag::Diagnostic::Warning(
          lyra::diag::SourceSpan{}, lyra::diag::DiagCode::kWarningPedantic,
          "warn")
          .primary.category.has_value());

  // Notes carry only span+message; they have no category field by design.
  auto with_note = lyra::diag::Diagnostic::Unsupported(
                       lyra::diag::DiagCode::kUnsupportedForGenerate, "x",
                       lyra::diag::UnsupportedCategory::kFeature)
                       .WithNote("y");
  EXPECT_EQ(with_note.notes.front().message, "y");
}

}  // namespace
