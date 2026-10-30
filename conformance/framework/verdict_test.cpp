// The judgement a corpus case's run gets, and the readers that turn two tools'
// output into what it judges. Every verdict is checked both ways, since a judge
// that only ever says "conforms" passes any corpus.

#include "conformance/framework/verdict.hpp"

#include <array>
#include <csignal>
#include <cstdint>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "conformance/framework/case.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/process.hpp"
#include "conformance/framework/value_change_dump.hpp"

namespace conformance::framework {
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

TEST(VerdictTest, AProgramThatRunsReachesItsSentinelCleanly) {
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

TEST(VerdictTest, ARejectionIsTheToolsAnswerAndNothingElse) {
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

TEST(VerdictTest, AReportClaimIsMetByItsKindAndWhatTheCaseNames) {
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

TEST(VerdictTest, EachReportAskedForIsAnsweredByOneOfItsOwn) {
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

TEST(VerdictTest, AReportTiedToItsStatementIsCountedExactly) {
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

TEST(VerdictTest, TheLineTiesAReportToItsStatementAndNothingElse) {
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

TEST(VerdictTest, ARunEndsAtAReportOnlyWhereOneMayEndIt) {
  // A warning does not end the run, so a run that stopped after one, short of
  // the error asked for, issued no error.
  const IssuesReports warning_then_error{
      .reports = {
          Asking(ReportKind::kWarning, 10), Asking(ReportKind::kError, 20)}};
  Observation stopped = RanReadyForTheReport();
  stopped.reports.push_back(Report(ReportKind::kWarning, 10));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(warning_then_error, stopped)));
}

TEST(VerdictTest, AStatusThatFailsIsNotHiddenByAnUnreadLine) {
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

TEST(VerdictTest, AReportTheRunGoesOnPastIsHeldToItsStatus) {
  const IssuesReports warning{.reports = {Asking(ReportKind::kWarning)}};
  Observation clean = RanToItsSentinel();
  clean.reports.push_back(Report(ReportKind::kWarning));
  EXPECT_TRUE(Is<Conforms>(Judge(warning, clean)));
  Observation failed = RanToItsSentinel(1);
  failed.reports.push_back(Report(ReportKind::kWarning));
  EXPECT_TRUE(Is<DoesNotConform>(Judge(warning, failed)));
}

TEST(VerdictTest, AReportTheRunMayEndAtIsPrecededByReadiness) {
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

TEST(VerdictTest, AnAbsenceIsOfOneKindOfReportOnly) {
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

TEST(VerdictTest, AUseModelIsAskedForAndNeverAssumed) {
  Observation lacking;
  lacking.end = ToolLacks{.use_model = "a use model", .clause = "LRM 3.12.1"};
  EXPECT_TRUE(Is<DoesNotConform>(Judge(RunsToItsChecks{}, lacking)));

  Observation stuck;
  stuck.end = StoppedAtALimit{"the time it was given"};
  EXPECT_TRUE(Is<Unresolved>(Judge(RunsToItsChecks{}, stuck)));
}

// A limit the caller set -- time, memory, or the machine's own -- says nothing
// of the program, so a run stopped at one is never a failure of the tool,
// while one the tool's own fault ended is.
TEST(VerdictTest, ARunStoppedAtALimitIsUnresolvedAndACrashIsNot) {
  const ProcessOutcome timed_out{
      .end = TimedOut{}, .stdout_text = {}, .stderr_text = {}};
  const ProcessOutcome killed{
      .end = Signalled{.signal = SIGKILL},
      .stdout_text = {},
      .stderr_text = {}};
  const ProcessOutcome crashed{
      .end = Signalled{.signal = SIGSEGV},
      .stdout_text = {},
      .stderr_text = {}};
  const ProcessOutcome exhausted{
      .end = Signalled{.signal = SIGABRT},
      .stdout_text = {},
      .stderr_text =
          "terminate called after throwing an instance of "
          "'std::bad_alloc'\n"};
  const ProcessOutcome said_so{
      .end = Exited{.code = 1},
      .stdout_text = {},
      .stderr_text = "error: the simulation ran out of memory\n"};
  constexpr std::array<std::string_view, 1> kNoWords = {"no tool writes this"};
  constexpr std::array<std::string_view, 1> kAbort = {"std::bad_alloc"};
  constexpr std::array<std::string_view, 1> kSaid = {"ran out of memory"};

  EXPECT_TRUE(
      std::holds_alternative<StoppedAtALimit>(EndOf(timed_out, kNoWords)));
  EXPECT_TRUE(std::holds_alternative<StoppedAtALimit>(EndOf(killed, kNoWords)));
  EXPECT_TRUE(std::holds_alternative<ToolFailed>(EndOf(crashed, kNoWords)));
  EXPECT_TRUE(std::holds_alternative<ToolFailed>(EndOf(exhausted, kNoWords)));
  EXPECT_TRUE(
      std::holds_alternative<StoppedAtALimit>(EndOf(exhausted, kAbort)));
  EXPECT_TRUE(std::holds_alternative<Ran>(EndOf(said_so, kNoWords)));
  EXPECT_TRUE(std::holds_alternative<StoppedAtALimit>(EndOf(said_so, kSaid)));

  Observation stopped;
  stopped.end = EndOf(killed, kNoWords);
  EXPECT_TRUE(Is<Unresolved>(Judge(RunsToItsChecks{}, stopped)));
}

// A dump is free format, its identifier codes and the text of its header are
// the tool's, and a vector may be written shortened to the bits that do not
// extend to the left (LRM 21.7.2, 21.7.2.2); none of that is a difference.
// What each name is declared as, in which scope, and the value it holds at
// each simulated time are, the time read in the unit the dump states.
TEST(VerdictTest, DumpsAreComparedAsTheStandardReadsThem) {
  constexpr std::string_view kExpected =
      "$timescale 1ns $end\n"
      "$scope module Top $end\n"
      "$var reg 4 A a [3:0] $end\n"
      "$var real 64 B r $end\n"
      "$var event 1 C e $end\n"
      "$scope task bump $end\n"
      "$var reg 1 D d $end\n"
      "$upscope $end\n"
      "$upscope $end\n"
      "$enddefinitions $end\n"
      "#0\n"
      "$dumpvars b1 A r1.5 B 0D $end\n"
      "#2\n"
      "$dumpoff bxxxx A $end\n"
      "#4\n"
      "1C\n";
  constexpr std::string_view kSameOtherwiseSpelled =
      "$date today $end $version some tool $end $timescale 1 ns $end\n"
      "$scope module Top $end $var reg 4 ! a $end\n"
      "$var real 64 \" r $end $var event 1 # e $end\n"
      "$scope task bump $end $var reg 1 $ d $end $upscope $end\n"
      "$upscope $end $enddefinitions $end\n"
      "#0 $dumpvars b0001 ! r1.50 \" 0$ $end #2 $dumpoff bx ! $end\n"
      "#4 1#\n";
  EXPECT_EQ(DumpDifference(kExpected, kSameOtherwiseSpelled), std::nullopt);

  const auto replaced = [&](std::string_view from, std::string_view to) {
    std::string text{kSameOtherwiseSpelled};
    return text.replace(text.find(from), from.size(), to);
  };
  // The same times in a finer unit are the same simulated times; the same
  // numbers in another unit are not.
  EXPECT_EQ(
      DumpDifference(
          kExpected,
          "$timescale 100 ps $end\n"
          "$scope module Top $end $var reg 4 ! a $end\n"
          "$var real 64 \" r $end $var event 1 # e $end\n"
          "$scope task bump $end $var reg 1 $ d $end $upscope $end\n"
          "$upscope $end $enddefinitions $end\n"
          "#0 $dumpvars b0001 ! r1.5 \" 0$ $end #20 $dumpoff bx ! $end\n"
          "#40 1#\n"),
      std::nullopt);
  EXPECT_NE(DumpDifference(kExpected, replaced("1 ns", "1 ps")), std::nullopt);
  EXPECT_NE(
      DumpDifference(kExpected, replaced("$timescale 1 ns $end", "")),
      std::nullopt);

  EXPECT_NE(
      DumpDifference(kExpected, replaced("b0001", "b0011")), std::nullopt);
  EXPECT_NE(
      DumpDifference(kExpected, replaced("$dumpoff", "$dumpall")),
      std::nullopt);
  EXPECT_NE(DumpDifference(kExpected, replaced("#4", "#5")), std::nullopt);
  EXPECT_NE(
      DumpDifference(kExpected, replaced("task", "function")), std::nullopt);
  EXPECT_NE(
      DumpDifference(
          kExpected, replaced(
                         "$upscope $end $enddefinitions",
                         "$var reg 1 % extra $end $upscope $end "
                         "$enddefinitions")),
      std::nullopt);
  // A trigger the expected dump records is one the run owes.
  EXPECT_NE(DumpDifference(kExpected, replaced("#4 1#", "")), std::nullopt);
  // A command whose content is the tool's is still held to its grammar.
  EXPECT_NE(
      DumpDifference(
          kExpected, replaced("$version some tool $end", "$version some tool")),
      std::nullopt);
  EXPECT_NE(
      DumpDifference(kExpected, replaced("reg 4 !", "reg four !")),
      std::nullopt);
  EXPECT_NE(DumpDifference(kExpected, "not a dump at all"), std::nullopt);
}

// The sections a time opens are told apart by what each records, not by the
// order they stand in, which nothing fixes. An event holds no value for a
// section to list, so what a section records of one is no difference, while
// its triggering is. A scope declaring nothing says nothing, so whether one is
// written is too.
TEST(VerdictTest, ADumpIsReadForWhatItSaysAndNothingElse) {
  constexpr std::string_view kExpected =
      "$timescale 1ns $end\n"
      "$scope module Top $end $var reg 1 A a $end $var event 1 E e $end\n"
      "$upscope $end $enddefinitions $end\n"
      "#0 $dumpvars 1A $end $dumpoff xA $end #3 1E\n";
  constexpr std::string_view kHeader =
      "$timescale 1ns $end\n"
      "$scope module Top $end $var reg 1 ! a $end $var event 1 \" e $end\n";
  EXPECT_EQ(
      DumpDifference(
          kExpected, std::string{kHeader} +
                         "$scope module empty $end $upscope $end\n"
                         "$upscope $end $enddefinitions $end\n"
                         "#0 $dumpvars 1! 1\" $end $dumpoff x! $end #3 1\"\n"),
      std::nullopt);
  EXPECT_EQ(
      DumpDifference(
          kExpected, std::string{kHeader} +
                         "$upscope $end $enddefinitions $end\n"
                         "#0 $dumpoff x! $end $dumpvars 1! $end #3 1\"\n"),
      std::nullopt);
  EXPECT_NE(
      DumpDifference(
          kExpected, std::string{kHeader} +
                         "$upscope $end $enddefinitions $end\n"
                         "#0 $dumpvars x! $end $dumpoff 1! $end #3 1\"\n"),
      std::nullopt);
  EXPECT_NE(
      DumpDifference(
          kExpected, std::string{kHeader} +
                         "$upscope $end $enddefinitions $end\n"
                         "#0 $dumpvars 1! $end $dumpoff x! $end\n"),
      std::nullopt);
}

// A declaration's size counts its bits, which a real and an event do not have,
// so their size is the tool's while a vector's is not.
TEST(VerdictTest, OnlyAVariableWithBitsIsHeldToItsSize) {
  constexpr std::string_view kExpected =
      "$timescale 1ns $end "
      "$scope module Top $end $var real 64 R r $end $var reg 4 A a $end\n"
      "$upscope $end $enddefinitions $end #0 $dumpvars r1 R b1 A $end\n";
  EXPECT_EQ(
      DumpDifference(
          kExpected,
          "$timescale 1ns $end "
          "$scope module Top $end $var real 1 R r $end $var reg 4 A a $end\n"
          "$upscope $end $enddefinitions $end #0 $dumpvars r1 R b1 A $end\n"),
      std::nullopt);
  EXPECT_NE(
      DumpDifference(
          kExpected,
          "$timescale 1ns $end "
          "$scope module Top $end $var real 64 R r $end $var reg 8 A a $end\n"
          "$upscope $end $enddefinitions $end #0 $dumpvars r1 R b1 A $end\n"),
      std::nullopt);
}

// An extended dump's port value is its state characters and two strengths,
// each told by itself (LRM 21.7.4.3).
TEST(VerdictTest, ExtendedDumpPortsAreComparedByStateAndStrength) {
  constexpr std::string_view kExpected =
      "$timescale 1ns $end "
      "$scope module Top.u $end\n"
      "$var port [3:0] <0 d $end\n"
      "$upscope $end\n"
      "$enddefinitions $end\n"
      "#0 $dumpports pDUDU 6 6 <0 $end\n";
  EXPECT_EQ(
      DumpDifference(
          kExpected,
          "$timescale 1ns $end "
          "$scope module Top.u $end $var port   [3:0]   <0   d $end\n"
          "$upscope $end $enddefinitions $end #0\n$dumpports\npDUDU  6 6 <0\n"
          "$end\n"),
      std::nullopt);
  EXPECT_NE(
      DumpDifference(
          kExpected,
          "$timescale 1ns $end "
          "$scope module Top.u $end $var port [3:0] <0 d $end $upscope $end\n"
          "$enddefinitions $end #0 $dumpports pDUDU 6 0 <0 $end\n"),
      std::nullopt);
}

// A case is held to every file it expects as well as to its claim: one the run
// did not write, or wrote otherwise, does not conform, and the claim's own
// verdict stands where it is not a conforming one.
TEST(VerdictTest, ACaseIsHeldToTheFilesItExpects) {
  Case c;
  c.claim = RunsToItsChecks{};
  c.expected_outputs.push_back(
      ExpectedOutput{
          .name = "wave.vcd",
          .text = "$timescale 1ns $end "
                  "$scope module Top $end $var reg 1 ! a $end $upscope $end "
                  "$enddefinitions $end #0 $dumpvars 1! $end"});

  Observation wrote = RanToItsSentinel();
  wrote.outputs.push_back(
      WrittenOutput{
          .name = "wave.vcd",
          .text = "$timescale 1ns $end "
                  "$scope module Top $end $var reg 1 % a $end $upscope $end "
                  "$enddefinitions $end #0 $dumpvars 1% $end"});
  EXPECT_TRUE(Is<Conforms>(JudgeCase(c, wrote)));

  Observation wrote_otherwise = wrote;
  wrote_otherwise.outputs.front().text =
      "$timescale 1ns $end "
      "$scope module Top $end $var reg 1 % a $end $upscope $end "
      "$enddefinitions $end #0 $dumpvars 0% $end";
  EXPECT_TRUE(Is<DoesNotConform>(JudgeCase(c, wrote_otherwise)));

  Observation wrote_nothing = RanToItsSentinel();
  wrote_nothing.outputs.push_back(
      WrittenOutput{.name = "wave.vcd", .text = std::nullopt});
  EXPECT_TRUE(Is<DoesNotConform>(JudgeCase(c, wrote_nothing)));

  Observation stopped = wrote;
  stopped.end = StoppedAtALimit{"the time it was given"};
  EXPECT_TRUE(Is<Unresolved>(JudgeCase(c, stopped)));
}

TEST(VerdictTest, ACaseNamedByARelativePathIsNotHandedToATool) {
  Case c;
  c.entry = "conformance/cases/a/b/main.sv";
  EXPECT_TRUE(MissingInput(c).has_value());
}

}  // namespace
}  // namespace conformance::framework
