#include "lyra_adapter.hpp"

#include <array>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "conformance_case.hpp"
#include "conformance_observation.hpp"
#include "process.hpp"

namespace lyra::test {
namespace {

// How long one command may take before it is called stuck. It is a guard
// against a run that never ends, not a budget for how fast a case builds: a
// case builds its program with whatever compiler the executing machine has, and
// one with no prepared header takes several times as long as one with, so the
// guard sits well above the slowest honest build.
constexpr std::chrono::seconds kStuckAfter{300};

// How Lyra says a build ended without a program for a reason other than the
// program being wrong: a construct it does not carry out yet, and a host tool
// it ran failing on what Lyra wrote or linked.
constexpr std::string_view kNotYetSupported = "is not yet supported";
constexpr std::array<std::string_view, 2> kHostToolFailures = {
    "error: compiling ", "error: linking "};

// Why a build that ended without a program ended, where it was not the program
// being refused.
auto BuildFailure(std::string_view diagnostics) -> std::optional<ToolFailed> {
  if (diagnostics.find(kInternalErrorReport) != std::string_view::npos) {
    return ToolFailed{"the compiler reported a bug of its own"};
  }
  if (diagnostics.find(kNotYetSupported) != std::string_view::npos) {
    return ToolFailed{"the compiler refused a construct it does not carry out"};
  }
  for (const std::string_view failure : kHostToolFailures) {
    if (diagnostics.find(failure) != std::string_view::npos) {
      return ToolFailed{"the compiler did not build a program from its output"};
    }
  }
  return std::nullopt;
}

// The words a line of Lyra's opens a report with, after where the report was
// made: a severity task's own severity, or one of the words a compiler's
// message opens with.
constexpr std::array<std::string_view, 5> kReportWords = {
    "info", "warning", "error", "fatal", "note"};

struct ReportLine {
  std::string_view place;
  std::string_view word;
  std::string_view body;
};

// The earliest word in the line is the report's own; a later one is in its
// body.
auto SplitReportLine(std::string_view line) -> std::optional<ReportLine> {
  std::optional<ReportLine> earliest;
  std::size_t earliest_at = std::string_view::npos;
  for (const std::string_view word : kReportWords) {
    const std::string opening = std::string(word) + ": ";
    if (line.starts_with(opening)) {
      return ReportLine{
          .place = {}, .word = word, .body = line.substr(opening.size())};
    }
    const std::string placed = ": " + opening;
    const std::size_t at = line.find(placed);
    if (at < earliest_at) {
      earliest_at = at;
      earliest = ReportLine{
          .place = line.substr(0, at),
          .word = word,
          .body = line.substr(at + placed.size())};
    }
  }
  return earliest;
}

// The source line of where a report was made, which Lyra writes as
// `file:line:column`.
auto ReadPlaceLine(std::string_view place) -> std::optional<std::uint64_t> {
  const std::size_t column = place.rfind(':');
  if (column == std::string_view::npos) {
    return std::nullopt;
  }
  const std::size_t line = place.rfind(':', column - 1);
  if (line == std::string_view::npos) {
    return std::nullopt;
  }
  return ReadDecimal(place.substr(line + 1, column - line - 1));
}

// A qualified statement's violation report names its keyword quoted, then the
// word violation (LRM 12.4.2.1).
auto IsViolation(std::string_view body) -> bool {
  if (!body.starts_with('\'')) {
    return false;
  }
  const std::size_t close = body.find('\'', 1);
  return close != std::string_view::npos &&
         body.substr(close + 1).starts_with(" violation");
}

// The message $finish or $stop prints: the task quoted, then the time.
auto ReadSimulationControl(std::string_view body)
    -> std::optional<ObservedReport> {
  for (const auto& [task, kind] :
       {std::pair{std::string_view{"'$finish' at time "}, ReportKind::kFinish},
        std::pair{std::string_view{"'$stop' at time "}, ReportKind::kStop}}) {
    if (body.starts_with(task)) {
      std::string_view time = body.substr(task.size());
      time = time.substr(0, time.find(','));
      return ObservedReport{
          .kind = kind,
          .line = std::nullopt,
          .time = ReadDecimal(time),
          .scope = std::nullopt,
          .message = std::string(body),
          .written = {}};
    }
  }
  return std::nullopt;
}

// The context a report's following note gives: the scope its procedure runs
// in, where it has one, and the time.
void ReadContext(std::string_view body, ObservedReport& report) {
  constexpr std::string_view kIn = "in '";
  constexpr std::string_view kAt = "at time ";
  if (body.starts_with(kIn)) {
    const std::size_t close = body.find('\'', kIn.size());
    if (close == std::string_view::npos) {
      return;
    }
    report.scope = std::string(body.substr(kIn.size(), close - kIn.size()));
    body = body.substr(close + 1);
    if (body.starts_with(' ')) {
      body.remove_prefix(1);
    }
  }
  if (body.starts_with(kAt)) {
    report.time = ReadDecimal(body.substr(kAt.size()));
  }
}

auto KindOfWord(std::string_view word) -> ReportKind {
  if (word == "warning") {
    return ReportKind::kWarning;
  }
  if (word == "error") {
    return ReportKind::kError;
  }
  if (word == "fatal") {
    return ReportKind::kFatal;
  }
  return ReportKind::kInfo;
}

}  // namespace

void ReadLyraReports(std::string_view stream, Observation& seen) {
  std::istringstream lines{std::string(stream)};
  std::string line;
  // The report a note that follows it belongs to.
  std::optional<std::size_t> last;
  while (std::getline(lines, line)) {
    const std::optional<ReportLine> split = SplitReportLine(line);
    if (!split) {
      seen.unplaced.push_back(line);
      last.reset();
      continue;
    }
    if (split->word == "note") {
      if (std::optional<ObservedReport> control =
              ReadSimulationControl(split->body)) {
        control->line = ReadPlaceLine(split->place);
        control->written = line;
        seen.reports.push_back(std::move(*control));
        last.reset();
      } else if (last) {
        ReadContext(split->body, seen.reports[*last]);
      } else {
        seen.unplaced.push_back(line);
      }
      continue;
    }
    seen.reports.push_back(
        ObservedReport{
            .kind = IsViolation(split->body) ? ReportKind::kViolation
                                             : KindOfWord(split->word),
            .line = ReadPlaceLine(split->place),
            .time = std::nullopt,
            .scope = std::nullopt,
            .message = std::string(split->body),
            .written = line});
    last = seen.reports.size() - 1;
  }
}

auto LyraDesignArguments(const ConformanceCase& test_case)
    -> std::vector<std::string> {
  std::vector<std::string> args;
  for (const std::string& top : test_case.tops) {
    args.emplace_back("--top");
    args.push_back(top);
  }
  if (test_case.compilation_units == CompilationUnits::kSingle) {
    args.emplace_back("--single-unit");
  }
  if (test_case.library_map.has_value()) {
    args.emplace_back("--libmap");
    args.push_back(test_case.library_map->string());
  }
  // A reference reaches only the part of a compilation-unit scope declared
  // before it (LRM 3.12.1), so the sources the case is written against come
  // ahead of the one holding its checks.
  for (const std::filesystem::path& supporting : test_case.supporting_sources) {
    args.push_back(supporting.string());
  }
  args.push_back(test_case.entry.string());
  return args;
}

auto ObserveWithLyra(
    const LyraTool& tool, const ConformanceCase& test_case,
    const std::filesystem::path& directory) -> Observation {
  Observation seen;
  if (std::optional<HarnessFailed> missing = MissingInput(test_case)) {
    seen.end = std::move(*missing);
    return seen;
  }
  const std::filesystem::path program = directory / "program";
  std::vector<std::string> build{"build", "-o", program.string()};
  build.insert(
      build.end(), tool.build_options.begin(), tool.build_options.end());
  for (const std::filesystem::path& native : test_case.link_sources) {
    build.emplace_back("--dpi-link");
    build.push_back(native.string());
  }
  const std::vector<std::string> design = LyraDesignArguments(test_case);
  build.insert(build.end(), design.begin(), design.end());

  const ProcessOutcome built =
      RunChildProcess(tool.exe, build, kStuckAfter, directory);
  RecordCommand(seen, tool.exe, build, built);
  const RunEnd build_end = EndOf(built);
  const auto* build_ran = std::get_if<Ran>(&build_end);
  if (build_ran == nullptr) {
    seen.end = build_end;
    return seen;
  }
  if (build_ran->exit_code != 0) {
    if (std::optional<ToolFailed> failed = BuildFailure(built.stderr_text)) {
      seen.end = std::move(*failed);
    } else {
      seen.end = Rejected{};
    }
    return seen;
  }

  const ProcessOutcome run =
      RunChildProcess(program, test_case.program_args, kStuckAfter, directory);
  RecordCommand(seen, program, test_case.program_args, run);
  seen.end = EndOf(run);
  ReadMarkers(run.stdout_text, seen);
  ReadLyraReports(run.stderr_text, seen);
  return seen;
}

}  // namespace lyra::test
