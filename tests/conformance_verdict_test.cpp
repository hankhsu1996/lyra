// The judgement a corpus case's run gets, and the readers that turn two tools'
// output into what it judges. Every verdict is checked both ways, since a judge
// that only ever says "conforms" passes any corpus.

#include "tests/framework/conformance_verdict.hpp"

#include <cstdint>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <variant>

#include "tests/framework/conformance_case.hpp"
#include "tests/framework/conformance_observation.hpp"
#include "tests/framework/lyra_adapter.hpp"
#include "tests/framework/verilator_adapter.hpp"

namespace lyra::test {
namespace {

auto RanToItsSentinel(int exit_code = 0) -> Observation {
  Observation seen;
  seen.end = Ran{.exit_code = exit_code};
  seen.checks_passed = true;
  return seen;
}

auto RanReadyForTheReport(int exit_code = 0) -> Observation {
  Observation seen;
  seen.end = Ran{.exit_code = exit_code};
  seen.ready_for_the_report = true;
  return seen;
}

// A run that was ready for its reports and went on past them all.
auto RanPastItsReports() -> Observation {
  Observation seen = RanReadyForTheReport();
  seen.checks_passed = true;
  return seen;
}

auto Report(ReportKind kind, std::optional<std::uint64_t> line = std::nullopt)
    -> ObservedReport {
  return ObservedReport{
      .kind = kind,
      .line = line,
      .time = std::nullopt,
      .scope = std::nullopt,
      .message = std::nullopt,
      .written = "a report"};
}

auto Asking(ReportKind kind, std::optional<std::uint64_t> line = std::nullopt)
    -> ReportPattern {
  ReportPattern pattern;
  pattern.kind = kind;
  pattern.line = line;
  return pattern;
}

template <class Expected>
auto Is(const Verdict& verdict) -> bool {
  return std::holds_alternative<Expected>(verdict);
}

TEST(ConformanceVerdictTest, AProgramThatRunsReachesItsSentinelCleanly) {
  EXPECT_TRUE(Is<Conforms>(Judge(RunsToItsChecks{}, RanToItsSentinel())));

  Observation unchecked = RanToItsSentinel();
  unchecked.checks_passed = false;
  EXPECT_TRUE(Is<DoesNotConform>(Judge(RunsToItsChecks{}, unchecked)));

  // A report the program's own work causes is no failure of a check.
  Observation erred = RanToItsSentinel();
  erred.reports.push_back(Report(ReportKind::kError));
  EXPECT_TRUE(Is<Conforms>(Judge(RunsToItsChecks{}, erred)));

  EXPECT_TRUE(
      Is<DoesNotConform>(Judge(RunsToItsChecks{}, RanToItsSentinel(1))));

  Observation refused;
  refused.end = Rejected{};
  EXPECT_TRUE(Is<DoesNotConform>(Judge(RunsToItsChecks{}, refused)));
}

TEST(ConformanceVerdictTest, ARejectionIsTheToolsAnswerAndNothingElse) {
  Observation refused;
  refused.end = Rejected{};
  EXPECT_TRUE(Is<Conforms>(Judge(RejectedBeforeRunning{}, refused)));
  EXPECT_TRUE(
      Is<DoesNotConform>(Judge(RejectedBeforeRunning{}, RanToItsSentinel())));

  // A harness that could not hand the tool the program, and a tool that
  // crashed, are not the tool refusing it.
  Observation unstarted;
  unstarted.end = HarnessFailed{"no such file"};
  EXPECT_TRUE(Is<NotRun>(Judge(RejectedBeforeRunning{}, unstarted)));
  Observation crashed;
  crashed.end = ToolFailed{"ended by signal 11"};
  EXPECT_TRUE(Is<DoesNotConform>(Judge(RejectedBeforeRunning{}, crashed)));
}

TEST(ConformanceVerdictTest, AReportClaimIsMetByItsKindAndWhatTheCaseNames) {
  ReportPattern asked = Asking(ReportKind::kInfo, 21);
  asked.time = 37;
  asked.scope = "Top.probe";
  asked.message = "a probe report";
  const IssuesReports claim{.reports = {asked}};

  Observation reported = RanToItsSentinel();
  reported.reports.push_back(
      ObservedReport{
          .kind = ReportKind::kInfo,
          .line = 21,
          .time = 37,
          .scope = "Top.probe",
          .message = "a probe report",
          .written = {}});
  EXPECT_TRUE(Is<Conforms>(Judge(claim, reported)));

  // The message is the program's own text, which the case wrote whole, so one
  // that only begins with it is another message.
  Observation longer = reported;
  longer.reports.front().message = "a probe report 2";
  EXPECT_TRUE(Is<DoesNotConform>(Judge(claim, longer)));
  // A reader that could not tell where the program's message ends gives none,
  // and a message not read is not one that differs.
  Observation no_message = reported;
  no_message.reports.front().message = std::nullopt;
  EXPECT_TRUE(Is<Unresolved>(Judge(claim, no_message)));
  Observation no_time = reported;
  no_time.reports.front().time = std::nullopt;
  EXPECT_TRUE(Is<Unresolved>(Judge(claim, no_time)));

  Observation elsewhere = reported;
  elsewhere.reports.front().scope = "Top";
  EXPECT_TRUE(Is<DoesNotConform>(Judge(claim, elsewhere)));

  // A report from another statement does not answer one asked of this one,
  // and one the tool placed nowhere cannot be told from it.
  Observation another = reported;
  another.reports.front().line = 30;
  EXPECT_TRUE(Is<DoesNotConform>(Judge(claim, another)));
  Observation unplaceable = reported;
  unplaceable.reports.front().line = std::nullopt;
  EXPECT_TRUE(Is<Unresolved>(Judge(claim, unplaceable)));

  Observation silent = RanToItsSentinel();
  EXPECT_TRUE(Is<DoesNotConform>(Judge(claim, silent)));

  Observation unread = RanToItsSentinel();
  unread.unplaced.emplace_back("%Error: something unread");
  EXPECT_TRUE(Is<Unresolved>(Judge(claim, unread)));
}

TEST(ConformanceVerdictTest, EachReportAskedForIsAnsweredByOneOfItsOwn) {
  const IssuesReports two{
      .reports = {
          Asking(ReportKind::kViolation), Asking(ReportKind::kViolation)}};
  Observation once = RanPastItsReports();
  once.reports.push_back(Report(ReportKind::kViolation));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(two, once)));
  Observation twice = once;
  twice.reports.push_back(Report(ReportKind::kViolation));
  EXPECT_TRUE(Is<Conforms>(Judge(two, twice)));
  // A third answers nothing asked, and nothing tells which two did.
  Observation thrice = twice;
  thrice.reports.push_back(Report(ReportKind::kViolation));
  EXPECT_TRUE(Is<Unresolved>(Judge(two, thrice)));
  // A run the first report ended may have been ended by the tool, as the
  // standard lets it, so it fails nothing; but the second report was never
  // observed, so nothing shows it would have been issued either. A claim of
  // that one report alone is decided by it.
  Observation ended_at_first = once;
  ended_at_first.checks_passed = false;
  EXPECT_TRUE(Is<Unresolved>(Judge(two, ended_at_first)));
  const IssuesReports one{.reports = {Asking(ReportKind::kViolation)}};
  EXPECT_TRUE(Is<Conforms>(Judge(one, ended_at_first)));
  Observation early_at_first = ended_at_first;
  early_at_first.ready_for_the_report = false;
  const Verdict early = Judge(two, early_at_first);
  ASSERT_TRUE(Is<DoesNotConform>(early));
  EXPECT_NE(
      std::get<DoesNotConform>(early).why.find("before the program reached"),
      std::string::npos);

  // A claim names a severity and is answered by it alone: a case's own
  // failing check is a $fatal, and it never stands in for the error asked.
  const IssuesReports error{.reports = {Asking(ReportKind::kError)}};
  const IssuesReports fatal{.reports = {Asking(ReportKind::kFatal)}};
  Observation checked = RanReadyForTheReport(1);
  checked.reports.push_back(Report(ReportKind::kFatal));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(error, checked)));
  EXPECT_TRUE(Is<Conforms>(Judge(fatal, checked)));
  Observation erred = RanReadyForTheReport(1);
  erred.reports.push_back(Report(ReportKind::kError));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(fatal, erred)));
}

TEST(ConformanceVerdictTest, AReportTiedToItsStatementIsCountedExactly) {
  const IssuesReports two{
      .reports = {
          Asking(ReportKind::kError, 20), Asking(ReportKind::kError, 20)}};
  Observation went_on = RanPastItsReports();
  went_on.reports.push_back(Report(ReportKind::kError, 20));
  const Verdict short_one = Judge(two, went_on);
  ASSERT_TRUE(Is<DoesNotConform>(short_one));
  EXPECT_NE(
      std::get<DoesNotConform>(short_one).why.find("2 times, and 1 was read"),
      std::string::npos);

  went_on.reports.push_back(Report(ReportKind::kError, 20));
  EXPECT_TRUE(Is<Conforms>(Judge(two, went_on)));

  // A line the reader could not place may be one more of them, so the count
  // cannot be confirmed, whether or not the claim names the line.
  Observation unread = went_on;
  unread.unplaced.emplace_back("%Error: something unread");
  EXPECT_TRUE(Is<Unresolved>(Judge(two, unread)));
  const IssuesReports kind_alone{.reports = {Asking(ReportKind::kError)}};
  Observation once_unread = RanPastItsReports();
  once_unread.reports.push_back(Report(ReportKind::kError));
  EXPECT_TRUE(Is<Conforms>(Judge(kind_alone, once_unread)));
  once_unread.unplaced.emplace_back("%Error: something unread");
  EXPECT_TRUE(Is<Unresolved>(Judge(kind_alone, once_unread)));

  // A third from the same line is that statement reporting once more than the
  // standard has it report, which its line leaves no doubt about.
  went_on.reports.push_back(Report(ReportKind::kError, 20));
  const Verdict one_over = Judge(two, went_on);
  ASSERT_TRUE(Is<DoesNotConform>(one_over));
  EXPECT_NE(
      std::get<DoesNotConform>(one_over).why.find(
          "2 error reports from line 20 here, and the run issued 3"),
      std::string::npos);

  // One from another line answers nothing asked and is no one's surplus.
  Observation beside = went_on;
  beside.reports.back().line = 30;
  EXPECT_TRUE(Is<Conforms>(Judge(two, beside)));
  // One the tool placed nowhere may be line 20's third.
  Observation unplaced_third = went_on;
  unplaced_third.reports.back().line = std::nullopt;
  EXPECT_TRUE(Is<Unresolved>(Judge(two, unplaced_third)));

  // Even a run that ended without its sentinel has said the statement
  // reported three times.
  Observation ended_over = went_on;
  ended_over.checks_passed = false;
  EXPECT_TRUE(Is<DoesNotConform>(Judge(two, ended_over)));
}

TEST(ConformanceVerdictTest, TheLineTiesAReportToItsStatementAndNothingElse) {
  // A second report from the line asked about, carrying another message, is
  // that statement reporting once more than asked.
  ReportPattern saw_two = Asking(ReportKind::kError, 13);
  saw_two.message = "final saw 2";
  const IssuesReports once{.reports = {saw_two}};
  Observation twice = RanPastItsReports();
  twice.reports.push_back(Report(ReportKind::kError, 13));
  twice.reports.back().message = "final saw 2";
  EXPECT_TRUE(Is<Conforms>(Judge(once, twice)));
  twice.reports.push_back(Report(ReportKind::kError, 13));
  twice.reports.back().message = "final saw 3";
  EXPECT_TRUE(Is<DoesNotConform>(Judge(once, twice)));

  // A surplus from one line is a failure even while another line's report is
  // missing and might be among what was not read.
  const IssuesReports two_lines{
      .reports = {
          Asking(ReportKind::kError, 20), Asking(ReportKind::kError, 30)}};
  Observation over_and_unread = RanPastItsReports();
  over_and_unread.reports.push_back(Report(ReportKind::kError, 20));
  over_and_unread.reports.push_back(Report(ReportKind::kError, 20));
  over_and_unread.reports.push_back(Report(ReportKind::kError));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(two_lines, over_and_unread)));

  // A claim of the kind alone may be answered by a report from any line, so a
  // second report from line 20 may be its answer rather than line 20's
  // surplus.
  const IssuesReports line_and_kind{
      .reports = {Asking(ReportKind::kError), Asking(ReportKind::kError, 20)}};
  Observation from_one_line = RanPastItsReports();
  from_one_line.reports.push_back(Report(ReportKind::kError, 20));
  from_one_line.reports.push_back(Report(ReportKind::kError, 20));
  EXPECT_TRUE(Is<Conforms>(Judge(line_and_kind, from_one_line)));
  from_one_line.reports.push_back(Report(ReportKind::kError, 20));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(line_and_kind, from_one_line)));

  // Reports are given to claims so that as many are answered as can be: the
  // claim naming a message takes the report carrying it, whichever claim is
  // written first.
  ReportPattern carrying = Asking(ReportKind::kError, 20);
  carrying.message = "x";
  const IssuesReports either{
      .reports = {Asking(ReportKind::kError, 20), carrying}};
  Observation both = RanPastItsReports();
  both.reports.push_back(Report(ReportKind::kError, 20));
  both.reports.back().message = "x";
  both.reports.push_back(Report(ReportKind::kError, 20));
  both.reports.back().message = "y";
  EXPECT_TRUE(Is<Conforms>(Judge(either, both)));
}

TEST(ConformanceVerdictTest, ARunEndsAtAReportOnlyWhereOneMayEndIt) {
  // A warning does not end the run, so a run that stopped after one, short of
  // the error asked for, issued no error.
  const IssuesReports warning_then_error{
      .reports = {
          Asking(ReportKind::kWarning, 10), Asking(ReportKind::kError, 20)}};
  Observation stopped = RanReadyForTheReport();
  stopped.reports.push_back(Report(ReportKind::kWarning, 10));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(warning_then_error, stopped)));
}

TEST(ConformanceVerdictTest, AStatusThatFailsIsNotHiddenByAnUnreadLine) {
  const IssuesReports warning{.reports = {Asking(ReportKind::kWarning, 5)}};
  Observation failed = RanToItsSentinel(1);
  failed.reports.push_back(Report(ReportKind::kWarning, 5));
  failed.unplaced.emplace_back("%Error: something unread");
  EXPECT_TRUE(Is<DoesNotConform>(Judge(warning, failed)));

  const IssuesNoReportOf none{.kinds = {ReportKind::kError}};
  Observation silent_failed = RanToItsSentinel(1);
  silent_failed.unplaced.emplace_back("%Error: something unread");
  EXPECT_TRUE(Is<DoesNotConform>(Judge(none, silent_failed)));
}

TEST(ConformanceVerdictTest, AReportTheRunGoesOnPastIsHeldToItsStatus) {
  const IssuesReports warning{.reports = {Asking(ReportKind::kWarning)}};
  Observation clean = RanToItsSentinel();
  clean.reports.push_back(Report(ReportKind::kWarning));
  EXPECT_TRUE(Is<Conforms>(Judge(warning, clean)));
  Observation failed = RanToItsSentinel(1);
  failed.reports.push_back(Report(ReportKind::kWarning));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(warning, failed)));
}

TEST(ConformanceVerdictTest, AReportTheRunMayEndAtIsPrecededByReadiness) {
  const IssuesReports violation{.reports = {Asking(ReportKind::kViolation)}};

  // Ready for the report is the marker such a case prints; the one a case
  // prints once every check has passed is not it.
  Observation ready = RanReadyForTheReport(1);
  ready.reports.push_back(Report(ReportKind::kViolation));
  EXPECT_TRUE(Is<Conforms>(Judge(violation, ready)));
  Observation completed = RanToItsSentinel();
  completed.reports.push_back(Report(ReportKind::kViolation));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(violation, completed)));

  // The case is ready before the report may be issued, so a report that ended
  // the run first was issued too early.
  Observation early;
  early.end = Ran{.exit_code = 1};
  early.reports.push_back(Report(ReportKind::kViolation));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(violation, early)));

  // A check after the report holds where the run reaches it.
  Observation went_on = ready;
  went_on.reports.push_back(Report(ReportKind::kFatal));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(violation, went_on)));
}

TEST(ConformanceVerdictTest, AnAbsenceIsOfOneKindOfReportOnly) {
  const IssuesNoReportOf claim{
      .kinds = {ReportKind::kViolation, ReportKind::kError}};

  Observation chatty = RanToItsSentinel();
  chatty.reports.push_back(Report(ReportKind::kInfo));
  EXPECT_TRUE(Is<Conforms>(Judge(claim, chatty)));

  Observation violated = RanToItsSentinel();
  violated.reports.push_back(Report(ReportKind::kViolation));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(claim, violated)));

  // Forbidding an error forbids the error that also ends the run.
  Observation ended = RanToItsSentinel();
  ended.reports.push_back(Report(ReportKind::kFatal));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(claim, ended)));

  Observation unread = RanToItsSentinel();
  unread.unplaced.emplace_back("%Error: something unread");
  EXPECT_TRUE(Is<Unresolved>(Judge(claim, unread)));
}

TEST(ConformanceVerdictTest, AUseModelIsAskedForAndNeverAssumed) {
  Observation lacking;
  lacking.end = ToolLacks{.use_model = "a use model", .clause = "LRM 3.12.1"};
  EXPECT_TRUE(Is<DoesNotConform>(Judge(RunsToItsChecks{}, lacking)));

  Observation stuck;
  stuck.end = DidNotFinish{};
  EXPECT_TRUE(Is<Unresolved>(Judge(RunsToItsChecks{}, stuck)));
}

TEST(ConformanceVerdictTest, ACaseNamedByARelativePathIsNotHandedToATool) {
  ConformanceCase c;
  c.entry = "tests/conformance/a/b/main.sv";
  EXPECT_TRUE(MissingInput(c).has_value());
}

TEST(ConformanceReaderTest, LyraReportsAreReadOffTheirOwnShape) {
  Observation seen;
  ReadLyraReports(
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

TEST(ConformanceReaderTest, VerilatorReportsAreReadAmongTheProgramsOutput) {
  Observation seen;
  ReadVerilatorReports(
      "a line the program printed\n"
      "[0] %Error: main.sv:18: Assertion failed in Top.unnamedblk1: 'unique "
      "if' statement violated\n"
      "%Error: /abs/main.sv:18: Verilog $stop\n"
      "Aborting...\n"
      "[37] -Info: main.sv:21: Top.probe: a probe report\n"
      "- S i m u l a t i o n   R e p o r t: Verilator 5.045 devel\n"
      "- Verilator: end at 37ps; walltime 0.000 s; speed 1.013 us/s\n"
      "%Error: no place given\n",
      seen);
  ASSERT_EQ(seen.reports.size(), 3U);
  EXPECT_EQ(seen.reports[0].kind, ReportKind::kViolation);
  EXPECT_EQ(seen.reports[0].message, std::nullopt);
  EXPECT_EQ(seen.reports[0].scope, "Top.unnamedblk1");
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
}  // namespace lyra::test
