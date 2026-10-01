// A failure of the compiler's own is raised far below whatever was walking the
// design, so what locates it is what the walk said on the way down. These hold
// that account to what a reader needs of it: one line, standing at the
// innermost place the work named and carrying every name it gave, from the
// thread the exception left them on, and nothing from work that finished or
// from a failure somebody already handled.

#include "lyra/diag/failure_context.hpp"

#include <cstdint>
#include <exception>
#include <future>
#include <gtest/gtest.h>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/diag/render.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"

namespace {

using lyra::diag::FailureContext;
using lyra::diag::SourceSpan;

auto At(std::uint32_t begin, std::uint32_t end) -> SourceSpan {
  return SourceSpan{
      .file_id = lyra::diag::FileId{.value = 1}, .begin = begin, .end = end};
}

void Break() {
  throw lyra::InternalError("an invariant broke");
}

TEST(FailureContext, AFailureStandsAtTheInnermostPlaceAndCarriesEveryName) {
  try {
    const auto unit = FailureContext::InUnit("Top");
    const FailureContext declaration(At(10, 90));
    const auto function = FailureContext::InFunction("f");
    const FailureContext statement(At(30, 60));
    const FailureContext operand(At(34, 36));
    Break();
    FAIL() << "the break did not throw";
  } catch (const std::exception& failure) {
    const lyra::diag::Diagnostic report = lyra::diag::InternalFailure(failure);
    EXPECT_EQ(report.primary.kind, lyra::diag::DiagKind::kInternalError);
    EXPECT_EQ(report.primary.span, lyra::diag::DiagSpan{At(34, 36)});
    EXPECT_EQ(
        report.primary.message,
        "an invariant broke (in function 'f', in unit 'Top')");
    EXPECT_TRUE(report.notes.empty());
  }
}

TEST(FailureContext, AFailureOutsideAnyPlaceStandsNowhere) {
  try {
    const auto function = FailureContext::InFunction("t");
    Break();
  } catch (const std::exception& failure) {
    const lyra::diag::Diagnostic report = lyra::diag::InternalFailure(failure);
    EXPECT_EQ(
        report.primary.span, lyra::diag::DiagSpan{lyra::diag::UnknownSpan{}});
    EXPECT_EQ(report.primary.message, "an invariant broke (in function 't')");
  }
}

TEST(FailureContext, AFailureWithNothingSaidIsTheMessageAlone) {
  try {
    Break();
  } catch (const std::exception& failure) {
    EXPECT_EQ(
        lyra::diag::InternalFailure(failure).primary.message,
        "an invariant broke");
  }
}

TEST(FailureContext, WorkThatFinishedSaysNothing) {
  try {
    {
      const auto done = FailureContext::InUnit("Finished");
      const FailureContext statement(At(1, 2));
    }
    const auto unit = FailureContext::InUnit("Failing");
    Break();
  } catch (const std::exception& failure) {
    const lyra::diag::Diagnostic report = lyra::diag::InternalFailure(failure);
    EXPECT_EQ(
        report.primary.span, lyra::diag::DiagSpan{lyra::diag::UnknownSpan{}});
    EXPECT_EQ(report.primary.message, "an invariant broke (in unit 'Failing')");
  }
}

TEST(FailureContext, ALibraryExceptionIsTheCompilersFailureToo) {
  try {
    const auto unit = FailureContext::InUnit("Top");
    const std::variant<int, std::string> held = 1;
    EXPECT_TRUE(std::get<std::string>(held).empty());
    FAIL() << "reading the alternative not held did not throw";
  } catch (const std::exception& failure) {
    const lyra::diag::Diagnostic report = lyra::diag::InternalFailure(failure);
    EXPECT_EQ(report.primary.kind, lyra::diag::DiagKind::kInternalError);
    EXPECT_TRUE(report.primary.message.ends_with("(in unit 'Top')"))
        << report.primary.message;
  }
}

TEST(FailureContext, AFailureSomethingHandledLeavesNothingForTheNext) {
  // Handled without asking what it had passed through.
  EXPECT_THROW(
      {
        const auto unit = FailureContext::InUnit("Handled");
        const FailureContext statement(At(1, 2));
        Break();
      },
      lyra::InternalError);
  try {
    const auto unit = FailureContext::InUnit("Next");
    Break();
  } catch (const std::exception& failure) {
    const lyra::diag::Diagnostic report = lyra::diag::InternalFailure(failure);
    EXPECT_EQ(
        report.primary.span, lyra::diag::DiagSpan{lyra::diag::UnknownSpan{}});
    EXPECT_EQ(report.primary.message, "an invariant broke (in unit 'Next')");
  }
}

TEST(FailureContext, EachThreadCollectsItsOwnWork) {
  const auto here = FailureContext::InUnit("OnThisThread");
  auto other = std::async(std::launch::async, [] {
    try {
      const auto unit = FailureContext::InUnit("OnTheOther");
      Break();
    } catch (const std::exception& failure) {
      return lyra::diag::InternalFailure(failure).primary.message;
    }
    return std::string{};
  });
  EXPECT_EQ(other.get(), "an invariant broke (in unit 'OnTheOther')");
}

// What a reader sees for two failures in one run: one line and the source for
// each, and the request to report them once, after the count.
TEST(FailureContext, TheRunAsksForTheReportOnce) {
  lyra::diag::SourceManager sources;
  const std::string content = "module M;\n  initial x = 1;\nendmodule\n";
  const auto file = sources.AddFile("main.sv", content);
  const auto begin = static_cast<std::uint32_t>(content.find("x = 1"));
  lyra::diag::DiagnosticSink sink;
  for (const std::string_view name : {"M", "N"}) {
    try {
      const auto unit = FailureContext::InUnit(name);
      const FailureContext statement(
          SourceSpan{.file_id = file, .begin = begin, .end = begin + 5});
      Break();
    } catch (const std::exception& failure) {
      sink.Report(lyra::diag::InternalFailure(failure));
    }
  }
  EXPECT_EQ(
      lyra::diag::RenderDiagnostics(
          sink, &sources, lyra::diag::RenderOptions{.use_color = false}),
      "main.sv:2:11: internal error: an invariant broke (in unit 'M')\n"
      "    2 |   initial x = 1;\n"
      "      |           ^~~~~\n"
      "main.sv:2:11: internal error: an invariant broke (in unit 'N')\n"
      "    2 |   initial x = 1;\n"
      "      |           ^~~~~\n"
      "2 errors generated.\n"
      "This is a bug in Lyra. Please report at: "
      "https://github.com/hankhsu1996/lyra/issues\n");
}

}  // namespace
