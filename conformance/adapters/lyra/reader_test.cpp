#include "conformance/adapters/lyra/reader.hpp"

#include <gtest/gtest.h>
#include <optional>

#include "conformance/framework/case.hpp"
#include "conformance/framework/observation.hpp"

namespace conformance::adapters::lyra {

using framework::Observation;
using framework::ReportKind;
namespace {

TEST(ReaderTest, LyraReportsAreReadOffTheirOwnShape) {
  Observation seen;
  ReadReports(
      "main.sv:16:5: warning: 'unique' violation; no condition matched\n"
      "note: at time 0\n"
      "main.sv:21:7: info: a probe report\n"
      "note: in 'Top.probe' at time 37\n"
      "main.sv:9:5: note: '$finish' at time 12\n"
      "main.sv:30:5: error: a message that says: info: inside it\n"
      "something no report looks like\n"
      "main.sv:31:5: error:  100% of it, spaced : \n"
      "main.sv:32:5: error: a message on\n"
      "two lines\n",
      seen);
  ASSERT_EQ(seen.reports.size(), 6U);
  // The message is the program's text exactly, spaces and all; a line it runs
  // on to is not placed, and $finish supplies no message of the program's.
  EXPECT_EQ(seen.reports[3].message, "a message that says: info: inside it");
  EXPECT_EQ(seen.reports[4].message, " 100% of it, spaced : ");
  EXPECT_EQ(seen.reports[2].message, std::nullopt);
  // The report's own word is the earliest in the line.
  EXPECT_EQ(seen.reports[3].kind, ReportKind::kError);
  EXPECT_EQ(seen.reports[3].line, 30U);
  EXPECT_EQ(seen.reports[0].kind, ReportKind::kViolation);
  EXPECT_EQ(seen.reports[0].message, std::nullopt);
  EXPECT_EQ(seen.reports[0].line, 16U);
  EXPECT_EQ(seen.reports[0].time, 0U);
  EXPECT_EQ(seen.reports[1].kind, ReportKind::kInfo);
  EXPECT_EQ(seen.reports[1].line, 21U);
  EXPECT_EQ(seen.reports[1].scope, "Top.probe");
  EXPECT_EQ(seen.reports[1].time, 37U);
  EXPECT_EQ(seen.reports[1].message, "a probe report");
  EXPECT_EQ(seen.reports[2].kind, ReportKind::kFinish);
  EXPECT_EQ(seen.reports[2].time, 12U);
  ASSERT_EQ(seen.unplaced.size(), 2U);
}

}  // namespace
}  // namespace conformance::adapters::lyra
