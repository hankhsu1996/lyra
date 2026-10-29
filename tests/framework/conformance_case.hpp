#pragma once

#include <cstdint>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace lyra::test {

// The text a case prints once every check it makes has passed. A case that
// reports success without having run its checks is the failure mode the corpus
// has to exclude, so success is this string appearing rather than the program
// merely surviving.
inline constexpr std::string_view kAllChecksPassed = "All checks passed";

// What a case whose claim is a report the run may end at prints before the
// statement that issues the report. It says every check before it passed and
// the run reached the statement. Such a case prints the sentinel only once the
// run has gone on past every report, so the two say different things, and a
// tool may end the run between them.
inline constexpr std::string_view kReadyForTheReport = "Ready for the report";

// The source a case is entered through. A case is a directory, and this is the
// file in it that states the claim and makes the checks.
inline constexpr std::string_view kCaseEntrySource = "main.sv";

// The library map a case's directory may hold. The standard's own examples
// assume a file of this name beside the sources, read before any of them
// (LRM 33.3.1).
inline constexpr std::string_view kCaseLibraryMap = "lib.map";

// The directory beside a case's sources holding the files its run is held to.
inline constexpr std::string_view kCaseExpectedOutputs = "expected";

// The directory beside a corpus holding the helpers its cases may include.
inline constexpr std::string_view kHarnessDirectory = "harness";

// The kinds of report the standard names. A severity task's report carries the
// severity it was called at (LRM 20.10), a failing assertion's default action
// is a call of $error (LRM 16.14.1), and an operation's run-time error is a
// report at error severity. A violation report (LRM 12.4.2.1) and the message
// $finish or $stop prints (LRM 20.2) are reports of their own kind, with no
// severity the standard fixes.
enum class ReportKind : std::uint8_t {
  kInfo,
  kWarning,
  kError,
  kFatal,
  kViolation,
  kFinish,
  kStop,
};

auto SpellingOf(ReportKind kind) -> std::string_view;

// Whether the standard lets a tool end the run at a report of this kind.
// $fatal ends it (LRM 20.10), $stop suspends it and what follows is the tool's
// (LRM 20.2), a violation is reported through the tool's own mechanism
// (LRM 12.4.2.1), and nothing promises the run goes on after an error. A
// warning or a message of no severity leaves it running, and $finish ends it
// through the final procedures (LRM 9.2.3), so a case may check after those.
auto MayEndTheRun(ReportKind kind) -> bool;

// A report a claim asks for, by what the standard fixes of it. A severity
// task's report shall carry the line of its call, the scope it is made in and
// the time (LRM 20.10) -- a failing assertion's default action included, since
// it is a call of $error (LRM 16.14.1) -- so a claim may name any of them, and
// the line is what ties the report to the statement that should issue it. The
// one text the standard puts in a report is the message the program itself
// supplied, so that is the only text a claim may name; whatever a tool words
// around it is the tool's own.
struct ReportPattern {
  ReportKind kind = ReportKind::kInfo;
  std::optional<std::uint64_t> line;
  std::optional<std::uint64_t> time;
  std::optional<std::string> scope;
  std::optional<std::string> message;

  auto operator==(const ReportPattern&) const -> bool = default;
};

// What a case claims the standard requires of its run, which is one of four
// things. A program that runs reaches its sentinel having passed every check.
// One the standard forbids is refused before it runs. One whose requirement is
// reports issues each of them, one report answering one claim; where the run
// may end at one, the case prints that it is ready for the report before it,
// and a check after the report holds only if the run reaches it. One whose
// requirement is the absence of some kinds of report issues none of them; a
// tool may write anything else it likes, so silence as a whole is never a
// requirement.
struct RunsToItsChecks {};

struct RejectedBeforeRunning {};

struct IssuesReports {
  std::vector<ReportPattern> reports;
};

struct IssuesNoReportOf {
  std::vector<ReportKind> kinds;
};

using Claim = std::variant<
    RunsToItsChecks, RejectedBeforeRunning, IssuesReports, IssuesNoReportOf>;

// The two use models every tool shall offer for which files form a
// compilation unit (LRM 3.12.1). Which one a tool takes when asked for neither
// is its own choice, so a case whose meaning depends on it says which.
enum class CompilationUnits : std::uint8_t {
  kSingle,
  kSeparate,
};

// One conformance case: a SystemVerilog program stating what IEEE 1800 requires
// and checking itself against it. A case is a directory, so what belongs to it
// is where a file sits rather than something a directive has to say; the
// directives that remain state what the standard cannot be read from the
// sources alone.
// A file the program has the tool write -- a dump -- whose content the case
// states by hand, compared as the standard reads that kind of file, since its
// spelling is free where its meaning is not.
struct ExpectedOutput {
  // The name the program writes it under, in the directory it runs in.
  std::string name;
  std::string text;
};

struct ConformanceCase {
  // The case directory's path under the corpus root, which is its identity: the
  // outer directory is the LRM clause it tests and the inner one is the subject
  // within that clause.
  std::string id;
  std::filesystem::path directory;
  // The source stating the claim and making the checks.
  std::filesystem::path entry;
  // The sources compiled ahead of the entry source, in the order given. A
  // reference reaches only the part of a compilation-unit scope declared before
  // it (LRM 3.12.1), so what the case is written against is compiled first.
  std::vector<std::filesystem::path> supporting_sources;
  // Native sources providing the foreign symbols this case's imports name
  // (LRM 35.4).
  std::vector<std::filesystem::path> link_sources;
  // The library map saying which library each source belongs to (LRM 33.3.1),
  // for a case whose subject is more than one library.
  std::optional<std::filesystem::path> library_map;
  // The instances to elaborate. A case that names none elaborates `Top`.
  std::vector<std::string> tops;
  // The simulated program's own arguments, where LRM 21.6 plusargs reach a
  // design.
  std::vector<std::string> program_args;
  // The files the run is held to, from the case's `expected` directory.
  std::vector<ExpectedOutput> expected_outputs;
  Claim claim;
  std::optional<CompilationUnits> compilation_units;
};

// A decimal number written as the whole of `text`, or nothing where it is not
// one.
auto ReadDecimal(std::string_view text) -> std::optional<std::uint64_t>;

// Every case under `corpus_root`, ordered by id. A directory holding the entry
// source is a case; one that does not is a clause, so the corpus's shape is
// read directly rather than inferred from what the cases mention.
auto LoadConformanceCases(const std::filesystem::path& corpus_root)
    -> std::vector<ConformanceCase>;

}  // namespace lyra::test
