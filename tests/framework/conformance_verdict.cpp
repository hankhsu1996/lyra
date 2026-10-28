#include "conformance_verdict.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "conformance_case.hpp"
#include "conformance_observation.hpp"
#include "lyra/base/overloaded.hpp"

namespace lyra::test {
namespace {

// A report at fatal severity is a report at error severity that also ends the
// run, so forbidding an error forbids it too. It never answers a claim of an
// error: a case's own failing check is a $fatal.
auto Forbids(ReportKind forbidden, ReportKind seen) -> bool {
  return forbidden == seen ||
         (forbidden == ReportKind::kError && seen == ReportKind::kFatal);
}

auto Matches(const ReportPattern& asked, const ObservedReport& seen) -> bool {
  return asked.kind == seen.kind && (!asked.line || seen.line == asked.line) &&
         (!asked.time || seen.time == asked.time) &&
         (!asked.scope || seen.scope == asked.scope) &&
         (!asked.message ||
          seen.message.find(*asked.message) != std::string::npos);
}

auto DescribePattern(const ReportPattern& pattern) -> std::string {
  const std::string_view kind = SpellingOf(pattern.kind);
  const bool vowel = kind.starts_with('i') || kind.starts_with('e');
  std::string text = std::format("{} {} report", vowel ? "an" : "a", kind);
  if (pattern.line) {
    text += std::format(" from line {}", *pattern.line);
  }
  if (pattern.time) {
    text += std::format(" at time {}", *pattern.time);
  }
  if (pattern.scope) {
    text += std::format(" in '{}'", *pattern.scope);
  }
  if (pattern.message) {
    text += std::format(" carrying '{}'", *pattern.message);
  }
  return text;
}

// The reports a claim asks for, each answered by a report of its own: a claim
// of two violations is not met by one. What is left is the reports the run
// issued that answered nothing asked.
struct Answered {
  std::vector<const ReportPattern*> answered;
  std::vector<const ReportPattern*> missing;
  std::vector<const ObservedReport*> left;
};

auto Answer(
    const std::vector<ReportPattern>& asked,
    const std::vector<ObservedReport>& issued) -> Answered {
  Answered out;
  std::vector<bool> taken(issued.size(), false);
  for (const ReportPattern& pattern : asked) {
    bool found = false;
    for (std::size_t i = 0; i < issued.size() && !found; ++i) {
      if (!taken[i] && Matches(pattern, issued[i])) {
        taken[i] = true;
        found = true;
      }
    }
    (found ? out.answered : out.missing).push_back(&pattern);
  }
  for (std::size_t i = 0; i < issued.size(); ++i) {
    if (!taken[i]) {
      out.left.push_back(&issued[i]);
    }
  }
  return out;
}

// A claim naming nothing but the kind of report it asks for, which any report
// of that kind answers as well as another.
auto Undistinguished(const ReportPattern& pattern) -> bool {
  return !pattern.line && !pattern.time && !pattern.scope && !pattern.message;
}

auto Unchecked(std::string_view marker) -> Verdict {
  return DoesNotConform{std::format(
      "the program ended without printing '{}', so a check failed or the "
      "checks did not all run",
      marker)};
}

// A program that ran is judged against what it claims. Every claim but a
// rejection needs its marker, which is printed only once every check before
// it has passed; a run without it never reached the point the claim is about.
// The status a run ends with is read only where nothing the claim asks for may
// end it: a tool may end a run at a report, and how it then exits is its own.
auto JudgeRun(const Claim& claim, const Ran& ran, const Observation& seen)
    -> Verdict {
  const Verdict unchecked = Unchecked(kAllChecksPassed);
  const auto unplaced = [&](std::string_view about) -> Verdict {
    return Unresolved{std::format(
        "{}, and the run wrote lines that read as this tool's reports but "
        "could not be placed: {}",
        about, seen.unplaced.front())};
  };
  return std::visit(
      Overloaded{
          // A correct program may well be one the standard requires reports
          // of -- a failed $cast, an $error that the run outlives -- and a case
          // says a check failed by $fatal, which leaves no sentinel. So a
          // program that runs is judged by its sentinel and its status alone.
          [&](const RunsToItsChecks&) -> Verdict {
            if (!seen.checks_passed) {
              return unchecked;
            }
            if (ran.exit_code != 0) {
              return DoesNotConform{std::format(
                  "the run passed its checks and ended with status {}",
                  ran.exit_code)};
            }
            return Conforms{};
          },
          [&](const RejectedBeforeRunning&) -> Verdict {
            return DoesNotConform{
                "the standard requires this program to be rejected, and the "
                "tool ran it"};
          },
          // Where the run may end at a report, the case prints that it is
          // ready for it before the report, and its sentinel only once the run
          // has gone on past every report. A check after a report holds where
          // the run reaches it: a $fatal there that no claim asked for is a
          // check that failed.
          [&](const IssuesReports& asked) -> Verdict {
            const bool may_end = std::ranges::any_of(
                asked.reports,
                [](const ReportPattern& r) { return MayEndTheRun(r.kind); });
            const std::string_view marker =
                may_end ? kReadyForTheReport : kAllChecksPassed;
            const bool marked =
                may_end ? seen.ready_for_the_report : seen.checks_passed;
            const Answered answered = Answer(asked.reports, seen.reports);
            const bool asks_fatal =
                std::ranges::any_of(asked.reports, [](const ReportPattern& r) {
                  return r.kind == ReportKind::kFatal;
                });
            const auto failed_check = std::ranges::find_if(
                answered.left, [&](const ObservedReport* r) {
                  return r->kind == ReportKind::kFatal && !asks_fatal;
                });
            if (failed_check != answered.left.end()) {
              return DoesNotConform{std::format(
                  "a check of the case failed: {}", (*failed_check)->written)};
            }
            // The case is ready for its report before the standard lets the
            // report be issued, so a run ended at that report short of the
            // marker was ended by a report the tool issued too early.
            const auto too_early = std::ranges::find_if(
                answered.answered,
                [](const ReportPattern* p) { return MayEndTheRun(p->kind); });
            if (!marked && too_early != answered.answered.end()) {
              return DoesNotConform{std::format(
                  "the tool issued {} before the program reached the point "
                  "the standard issues it after, and ended the run there",
                  DescribePattern(**too_early))};
            }
            if (!marked) {
              return Unchecked(marker);
            }
            // A run that ended at a report, short of going on past them all, is
            // held to the reports before that end; one that went on is held to
            // every report asked for.
            const bool ended_early = may_end && !seen.checks_passed;
            if (ended_early && !answered.answered.empty()) {
              return Conforms{};
            }
            if (answered.missing.empty()) {
              // A report asked for by its kind alone cannot be told from
              // another of that kind, so one more than asked for leaves which
              // answered the claim undecided.
              const auto more = std::ranges::find_if(
                  answered.left, [&](const ObservedReport* r) {
                    return std::ranges::any_of(
                        asked.reports, [&](const ReportPattern& p) {
                          return Undistinguished(p) && Matches(p, *r);
                        });
                  });
              if (more != answered.left.end()) {
                return Unresolved{std::format(
                    "the run issued more reports of the kind asked for than "
                    "the case asks, and nothing tells which answers it: {}",
                    (*more)->written)};
              }
              if (!may_end && ran.exit_code != 0) {
                return DoesNotConform{std::format(
                    "the run passed its checks and ended with status {}",
                    ran.exit_code)};
              }
              return Conforms{};
            }
            const ReportPattern& first = *answered.missing.front();
            const std::string missing = std::format(
                "the standard requires {} here, and none was read",
                DescribePattern(first));
            // A report the tool gave no line for may be the one asked for,
            // and cannot be told from it.
            const bool unplaceable =
                first.line && std::ranges::any_of(
                                  answered.left, [&](const ObservedReport* r) {
                                    return !r->line && r->kind == first.kind;
                                  });
            if (unplaceable) {
              return Unresolved{std::format(
                  "{}; a report of that kind was issued with no line to tie "
                  "it to the statement",
                  missing)};
            }
            if (!seen.unplaced.empty()) {
              return unplaced(missing);
            }
            return DoesNotConform{missing};
          },
          [&](const IssuesNoReportOf& forbidden) -> Verdict {
            if (!seen.checks_passed) {
              return unchecked;
            }
            const auto issued = std::ranges::find_if(
                seen.reports, [&](const ObservedReport& r) {
                  return std::ranges::any_of(
                      forbidden.kinds,
                      [&](ReportKind kind) { return Forbids(kind, r.kind); });
                });
            if (issued != seen.reports.end()) {
              return DoesNotConform{std::format(
                  "the standard allows no {} report here, and the run "
                  "issued one: {}",
                  SpellingOf(issued->kind), issued->written)};
            }
            if (!seen.unplaced.empty()) {
              return unplaced(
                  "none of the reports the standard forbids here "
                  "was read");
            }
            if (ran.exit_code != 0) {
              return DoesNotConform{std::format(
                  "the run passed its checks and ended with status {}",
                  ran.exit_code)};
            }
            return Conforms{};
          }},
      claim);
}

}  // namespace

auto Judge(const Claim& claim, const Observation& observation) -> Verdict {
  return std::visit(
      Overloaded{
          [](const HarnessFailed& failed) -> Verdict {
            return NotRun{failed.why};
          },
          [](const ToolLacks& lacks) -> Verdict {
            return DoesNotConform{std::format(
                "the tool offers no {}, which {} requires of every tool",
                lacks.use_model, lacks.clause)};
          },
          [&](const Rejected&) -> Verdict {
            if (std::holds_alternative<RejectedBeforeRunning>(claim)) {
              return Conforms{};
            }
            return DoesNotConform{
                "the tool rejected a program the standard accepts"};
          },
          [](const ToolFailed& failed) -> Verdict {
            return DoesNotConform{failed.how};
          },
          [](const DidNotFinish&) -> Verdict {
            return Unresolved{"the run did not end in the time it was given"};
          },
          [&](const Ran& ran) -> Verdict {
            return JudgeRun(claim, ran, observation);
          }},
      observation.end);
}

auto Describe(const Verdict& verdict) -> std::string {
  return std::visit(
      Overloaded{
          [](const Conforms&) -> std::string { return "conforms"; },
          [](const DoesNotConform& v) -> std::string {
            return std::format("does not conform: {}", v.why);
          },
          [](const Unresolved& v) -> std::string {
            return std::format("unresolved: {}", v.why);
          },
          [](const NotRun& v) -> std::string {
            return std::format("not run: {}", v.why);
          }},
      verdict);
}

}  // namespace lyra::test
