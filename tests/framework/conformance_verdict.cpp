#include "conformance_verdict.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "conformance_case.hpp"
#include "conformance_observation.hpp"
#include "lyra/base/overloaded.hpp"
#include "value_change_dump.hpp"

namespace lyra::test {
namespace {

// A report at fatal severity is a report at error severity that also ends the
// run, so forbidding an error forbids it too. It never answers a claim of an
// error: a case's own failing check is a $fatal.
auto Forbids(ReportKind forbidden, ReportKind seen) -> bool {
  return forbidden == seen ||
         (forbidden == ReportKind::kError && seen == ReportKind::kFatal);
}

template <class T>
auto EqualWhereAsked(
    const std::optional<T>& asked, const std::optional<T>& seen) -> bool {
  return !asked || seen == asked;
}

// The message a claim names is the whole of what the program supplied, which
// a reader has already taken out of the tool's own wording, so one that merely
// begins with it is another value: "final saw 2" is not "final saw 23".
auto Matches(const ReportPattern& asked, const ObservedReport& seen) -> bool {
  return asked.kind == seen.kind && EqualWhereAsked(asked.line, seen.line) &&
         EqualWhereAsked(asked.time, seen.time) &&
         EqualWhereAsked(asked.scope, seen.scope) &&
         EqualWhereAsked(asked.message, seen.message);
}

// A field the claim names and the report did not give is not read, which is
// not the same as one that differs.
template <class T>
auto EqualOrNotRead(const std::optional<T>& asked, const std::optional<T>& seen)
    -> bool {
  return !asked || !seen || seen == asked;
}

// The report may be the one asked for: nothing it gave differs from the claim,
// though something the claim names was not read from it.
auto MayMatch(const ReportPattern& asked, const ObservedReport& seen) -> bool {
  return asked.kind == seen.kind && EqualOrNotRead(asked.line, seen.line) &&
         EqualOrNotRead(asked.time, seen.time) &&
         EqualOrNotRead(asked.scope, seen.scope) &&
         EqualOrNotRead(asked.message, seen.message);
}

auto Times(std::size_t n) -> std::string {
  return n == 1 ? std::string("once") : std::format("{} times", n);
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

// Claims and reports are paired so that as many claims are answered as can
// be. Taking the first report that fits each claim in turn can leave a claim
// unanswered that another pairing answers -- a claim of a line taking the one
// report that also carries the message another claim names -- so a claim that
// finds every fitting report taken moves the claim holding one to another
// report where it can (an augmenting path).
class Pairing {
 public:
  Pairing(
      std::span<const ReportPattern> asked,
      std::span<const ObservedReport> issued)
      : asked_(asked), issued_(issued), claim_of_(issued.size()) {
  }

  auto Answer() -> Answered {
    Answered out;
    for (std::size_t p = 0; p < asked_.size(); ++p) {
      tried_.assign(issued_.size(), false);
      (Place(p) ? out.answered : out.missing).push_back(&asked_[p]);
    }
    for (std::size_t i = 0; i < issued_.size(); ++i) {
      if (!claim_of_[i]) {
        out.left.push_back(&issued_[i]);
      }
    }
    return out;
  }

 private:
  auto Place(std::size_t p) -> bool {
    for (std::size_t i = 0; i < issued_.size(); ++i) {
      if (tried_[i] || !Matches(asked_[p], issued_[i])) {
        continue;
      }
      tried_[i] = true;
      if (!claim_of_[i] || Place(*claim_of_[i])) {
        claim_of_[i] = p;
        return true;
      }
    }
    return false;
  }

  std::span<const ReportPattern> asked_;
  std::span<const ObservedReport> issued_;
  std::vector<std::optional<std::size_t>> claim_of_;
  std::vector<bool> tried_;
};

// The line ties a report to the statement that issued it, and nothing else in
// a report names a statement. A report from a line a claim names answers that
// line's claims, or a claim of its kind naming no line, which any line's report
// may answer; one more than those can take is the statement reporting an
// execution the standard does not have it report.
auto Overreported(const IssuesReports& asked, const Observation& seen)
    -> std::optional<std::string> {
  for (const ReportPattern& p : asked.reports) {
    if (!p.line) {
      continue;
    }
    const auto from_the_line = [&](const auto& r) {
      return r.kind == p.kind && r.line == p.line;
    };
    const auto wanted = std::ranges::count_if(asked.reports, from_the_line);
    const auto anywhere = std::ranges::count_if(
        asked.reports,
        [&](const ReportPattern& q) { return q.kind == p.kind && !q.line; });
    const auto issued = std::ranges::count_if(seen.reports, from_the_line);
    if (issued > wanted + anywhere) {
      return std::format(
          "the standard requires {} {} report{} from line {} here, and the "
          "run issued {}",
          wanted, SpellingOf(p.kind), wanted == 1 ? "" : "s", *p.line, issued);
    }
  }
  return std::nullopt;
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
            const Answered answered =
                Pairing(asked.reports, seen.reports).Answer();
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
            const auto ending = std::ranges::find_if(
                answered.answered,
                [](const ReportPattern* p) { return MayEndTheRun(p->kind); });
            if (!marked && ending != answered.answered.end()) {
              return DoesNotConform{std::format(
                  "the tool issued {} before the program reached the point "
                  "the standard issues it after, and ended the run there",
                  DescribePattern(**ending))};
            }
            if (!marked) {
              return Unchecked(marker);
            }
            // What is shown wrong is said before anything that leaves the
            // verdict open, since nothing left unread can make it right.
            if (std::optional<std::string> over = Overreported(asked, seen)) {
              return DoesNotConform{std::move(*over)};
            }
            if (!may_end && ran.exit_code != 0) {
              return DoesNotConform{std::format(
                  "the run passed its checks and ended with status {}",
                  ran.exit_code)};
            }
            if (answered.missing.empty()) {
              // A count is exact, so a report that may be one more of those
              // asked for -- of a kind asked for by kind alone, or lacking
              // what would tell it apart -- leaves the count undecided, and so
              // does a line the reader could not place.
              const auto more = std::ranges::find_if(
                  answered.left, [&](const ObservedReport* r) {
                    return std::ranges::any_of(
                        asked.reports, [&](const ReportPattern& p) {
                          return MayMatch(p, *r);
                        });
                  });
              if (more != answered.left.end()) {
                return Unresolved{std::format(
                    "every report the case asks for was read, and the run "
                    "issued one that may be one more of them: {}",
                    (*more)->written)};
              }
              if (!seen.unplaced.empty()) {
                return unplaced(
                    "every report the case asks for was read, and one more "
                    "may be among what was not");
              }
              return Conforms{};
            }
            const ReportPattern& first = *answered.missing.front();
            const auto times = std::ranges::count(asked.reports, first);
            const auto read = std::ranges::count_if(
                answered.answered,
                [&](const ReportPattern* p) { return *p == first; });
            const std::string missing =
                times == 1
                    ? std::format(
                          "the standard requires {} here, and none was read",
                          DescribePattern(first))
                    : std::format(
                          "the standard requires {} here {}, and {} {} read",
                          DescribePattern(first),
                          Times(static_cast<std::size_t>(times)), read,
                          read == 1 ? "was" : "were");
            // A run that ended at a report the standard lets it end at, short
            // of going on past them all, may have been ended there by the
            // tool, so that fails nothing; but a report it never reached was
            // never observed, so nothing shows whether it would have been
            // issued.
            if (ending != answered.answered.end() && !seen.checks_passed) {
              return Unresolved{std::format(
                  "{}; the run ended at a report the standard lets it end at, "
                  "before the rest were observed",
                  missing)};
            }
            // A report missing something the claim names -- a line, a time, a
            // scope, a message the reader could not cut out -- may be the one
            // asked for, and cannot be told from it.
            const auto unread = std::ranges::find_if(
                answered.left, [&](const ObservedReport* r) {
                  return std::ranges::any_of(
                      answered.missing,
                      [&](const ReportPattern* p) { return MayMatch(*p, *r); });
                });
            if (unread != answered.left.end()) {
              return Unresolved{std::format(
                  "{}; the run issued a report lacking something a claim "
                  "names, which may be one of those not read: {}",
                  missing, (*unread)->written)};
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
            if (ran.exit_code != 0) {
              return DoesNotConform{std::format(
                  "the run passed its checks and ended with status {}",
                  ran.exit_code)};
            }
            if (!seen.unplaced.empty()) {
              return unplaced(
                  "none of the reports the standard forbids here "
                  "was read");
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
          [](const StoppedAtALimit& stopped) -> Verdict {
            return Unresolved{std::format(
                "the run was stopped at {} before it ended", stopped.limit)};
          },
          [&](const Ran& ran) -> Verdict {
            return JudgeRun(claim, ran, observation);
          }},
      observation.end);
}

auto JudgeCase(const ConformanceCase& test_case, const Observation& observation)
    -> Verdict {
  Verdict verdict = Judge(test_case.claim, observation);
  if (!std::holds_alternative<Conforms>(verdict)) {
    return verdict;
  }
  for (const ExpectedOutput& expected : test_case.expected_outputs) {
    const auto written = std::ranges::find(
        observation.outputs, expected.name, &WrittenOutput::name);
    if (written == observation.outputs.end() || !written->text) {
      return DoesNotConform{
          std::format("the run wrote no '{}'", expected.name)};
    }
    if (const std::optional<std::string> difference =
            DumpDifference(expected.text, *written->text)) {
      return DoesNotConform{
          std::format("'{}' differs: {}", expected.name, *difference)};
    }
  }
  return verdict;
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
