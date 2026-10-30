#include "conformance/adapters/verilator/reader.hpp"

#include <gtest/gtest.h>
#include <optional>

#include "conformance/framework/case.hpp"
#include "conformance/framework/observation.hpp"

namespace conformance::adapters::verilator {

using framework::Observation;
using framework::ReportKind;
namespace {

TEST(ReaderTest, VerilatorReportsAreReadAmongTheProgramsOutput) {
  Observation seen;
  ReadReports(
      "a line the program printed\n"
      "[0] %Error: main.sv:18: Assertion failed in Top.unnamedblk1: 'unique "
      "if' statement violated\n"
      "%Error: /abs/main.sv:18: Verilog $stop\n"
      "Aborting...\n"
      "[37] -Info: main.sv:21: Top.probe: a probe report\n"
      "[40] %Error: main.sv:16: Assertion failed in Top.unnamedblk1: priority "
      "case, but non-match found for '3'h5'\n"
      "[40] %Error: main.sv:18: Assertion failed in Top.unnamedblk1: unique0 "
      "case, but multiple matches found for '3'h3'\n"
      "[41] %Error: main.sv:30: Assertion failed in Top: a priority case, but "
      "the program's own\n"
      "- S i m u l a t i o n   R e p o r t: Verilator 5.045 devel\n"
      "- Verilator: end at 37ps; walltime 0.000 s; speed 1.013 us/s\n"
      "%Error: no place given\n",
      seen);
  ASSERT_EQ(seen.reports.size(), 6U);
  EXPECT_EQ(seen.reports[0].kind, ReportKind::kViolation);
  EXPECT_EQ(seen.reports[0].message, std::nullopt);
  EXPECT_EQ(seen.reports[0].scope, "Top.unnamedblk1");
  // A case statement's violation report names the statement and what it
  // found, and a program's message that only begins with such words is the
  // program's.
  EXPECT_EQ(seen.reports[3].kind, ReportKind::kViolation);
  EXPECT_EQ(seen.reports[3].message, std::nullopt);
  EXPECT_EQ(seen.reports[3].line, 16U);
  EXPECT_EQ(seen.reports[4].kind, ReportKind::kViolation);
  EXPECT_EQ(seen.reports[5].kind, ReportKind::kError);
  EXPECT_EQ(seen.reports[5].message, "a priority case, but the program's own");
  EXPECT_EQ(seen.reports[0].line, 18U);
  EXPECT_EQ(seen.reports[0].time, 0U);
  EXPECT_EQ(seen.reports[1].kind, ReportKind::kStop);
  EXPECT_EQ(seen.reports[1].message, std::nullopt);
  EXPECT_EQ(seen.reports[2].kind, ReportKind::kInfo);
  EXPECT_EQ(seen.reports[2].scope, "Top.probe");
  EXPECT_EQ(seen.reports[2].message, "a probe report");
  ASSERT_EQ(seen.unplaced.size(), 1U);
}

}  // namespace
}  // namespace conformance::adapters::verilator
