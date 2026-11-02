#include "lyra/runtime/diagnostic.hpp"

#include <gtest/gtest.h>
#include <string>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/file_table.hpp"
#include "tests/base/written_at.hpp"

namespace {

using lyra::InternalError;
using lyra::base::DiagMessage;
using lyra::base::kNoSourceLocation;
using lyra::base::SourceLocationScope;
using lyra::runtime::DiagnosticDispatcher;
using lyra::runtime::FileTable;
using lyra::runtime::Severity;
using lyra::test::kWrittenAt;

TEST(DiagnosticDispatcherTest, AReportUnderNoBindingIsAFailureOfTheTool) {
  FileTable files;
  DiagnosticDispatcher diagnostic(files);
  EXPECT_THROW(
      diagnostic.Report(Severity::kWarning, DiagMessage("a warning")),
      InternalError);
}

TEST(DiagnosticDispatcherTest, AReportIsPlacedWhereTheScopeBound) {
  testing::internal::CaptureStderr();
  {
    FileTable files;
    DiagnosticDispatcher diagnostic(files);
    const SourceLocationScope bound(kWrittenAt);
    diagnostic.Report(Severity::kWarning, DiagMessage("a warning"));
  }
  EXPECT_EQ(
      testing::internal::GetCapturedStderr(),
      "test.sv:1:1: warning: a warning\n");
}

TEST(
    DiagnosticDispatcherTest,
    AReportStatingNoLocationReadsNoPlaceWhateverIsBound) {
  testing::internal::CaptureStderr();
  {
    FileTable files;
    DiagnosticDispatcher diagnostic(files);
    const SourceLocationScope bound(kWrittenAt);
    diagnostic.Report(
        kNoSourceLocation, Severity::kWarning, DiagMessage("a warning"));
  }
  EXPECT_EQ(testing::internal::GetCapturedStderr(), "warning: a warning\n");
}

}  // namespace
