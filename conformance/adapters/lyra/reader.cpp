#include "conformance/adapters/lyra/reader.hpp"

#include <algorithm>
#include <array>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <iterator>
#include <optional>
#include <span>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "conformance/framework/case.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/process.hpp"

namespace conformance::adapters::lyra {

using framework::Case;
using framework::CompilationUnits;
using framework::EndOf;
using framework::FindOnPath;
using framework::HarnessFailed;
using framework::MissingInput;
using framework::Observation;
using framework::ObservedReport;
using framework::ProcessOutcome;
using framework::Program;
using framework::Ran;
using framework::ReadDecimal;
using framework::ReadMarkers;
using framework::ReadOutputs;
using framework::Rejected;
using framework::ReportKind;
using framework::RunEnd;
using framework::RunStep;
using framework::ToolFailed;
using framework::ToolIdentity;
using framework::VersionOf;
namespace {

// What Lyra reads from the environment that can change a build it is handed
// through a pipe: where its cache of kept builds is, which decides whether a
// build is made again or taken as kept.
constexpr std::array<std::string_view, 2> kEnvironmentRead = {
    "XDG_CACHE_HOME", "HOME"};

// The C++ compiler Lyra builds and links every program with, found the way
// Lyra finds it: the one the build options name with `--cxx`, else the first
// of `clang++` and `c++` on PATH. A verdict can move with that compiler, so a
// run names it beside Lyra.
auto HostCompiler(std::span<const std::string> build_options)
    -> std::optional<std::filesystem::path> {
  const auto named = std::ranges::find(build_options, "--cxx");
  if (named != build_options.end() && std::next(named) != build_options.end()) {
    return std::filesystem::path(*std::next(named));
  }
  for (const std::string_view usual : {"clang++", "c++"}) {
    if (auto found = FindOnPath(usual)) {
      return found;
    }
  }
  return std::nullopt;
}

// How Lyra says a build ended without a program for a reason other than the
// program being wrong: a construct it does not carry out yet, and a host tool
// it ran failing on what Lyra wrote or linked.
constexpr std::string_view kNotYetSupported = "is not yet supported";
constexpr std::array<std::string_view, 2> kHostToolFailures = {
    "error: compiling ", "error: linking "};

// How the compiler and a program it built each say they ran out of memory.
constexpr std::array<std::string_view, 1> kCompilerOutOfMemory = {
    "the compiler ran out of memory"};
constexpr std::array<std::string_view, 1> kSimulationOutOfMemory = {
    "the simulation ran out of memory"};

// Why a build that ended without a program ended, where it was not the program
// being refused.
auto BuildFailure(std::string_view diagnostics) -> std::optional<ToolFailed> {
  if (diagnostics.find(kInternalErrorReport) != std::string_view::npos) {
    return ToolFailed{std::string(framework::kToolInternalError)};
  }
  if (diagnostics.find(kNotYetSupported) != std::string_view::npos) {
    return ToolFailed{std::string(framework::kToolUnsupportedConstruct)};
  }
  for (const std::string_view failure : kHostToolFailures) {
    if (diagnostics.find(failure) != std::string_view::npos) {
      return ToolFailed{std::string(framework::kToolGeneratedCodeDidNotBuild)};
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

// The message $finish or $stop prints: the task quoted, then the time. It is
// all the tool's, since neither task takes a message of the program's.
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
          .message = std::nullopt,
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

void ReadReports(std::string_view stream, Observation& seen) {
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
    // Everything after the report's word is the program's message, where the
    // program supplied one. A violation is worded by the tool alone, and so
    // are an error the run raises itself and a failing assertion's default
    // report, which read the same as an $error; a claim never names a message
    // for those, so the tool's words there match nothing.
    const bool violation = IsViolation(split->body);
    seen.reports.push_back(
        ObservedReport{
            .kind =
                violation ? ReportKind::kViolation : KindOfWord(split->word),
            .line = ReadPlaceLine(split->place),
            .time = std::nullopt,
            .scope = std::nullopt,
            .message = violation ? std::nullopt
                                 : std::optional<std::string>(split->body),
            .written = line});
    last = seen.reports.size() - 1;
  }
}

auto DesignArguments(const Case& test_case) -> std::vector<std::string> {
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

Reader::Reader(Tool tool) : tool_(std::move(tool)) {
}

auto Reader::Identity() const -> ToolIdentity {
  std::vector<Program> programs{Program{
      .role = "lyra", .path = tool_.exe, .version = VersionOf(tool_.exe)}};
  if (const auto cxx = HostCompiler(tool_.build_options)) {
    programs.push_back(
        Program{
            .role = "c++ compiler", .path = *cxx, .version = VersionOf(*cxx)});
  }
  return ToolIdentity{
      .name = "lyra",
      .programs = std::move(programs),
      .options = tool_.build_options,
      .environment_read = {kEnvironmentRead.begin(), kEnvironmentRead.end()}};
}

auto Reader::Observe(
    const Case& test_case, const std::filesystem::path& directory,
    std::optional<std::chrono::steady_clock::time_point> deadline) const
    -> Observation {
  const Tool& tool = tool_;
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
  const std::vector<std::string> design = DesignArguments(test_case);
  build.insert(build.end(), design.begin(), design.end());

  const ProcessOutcome built =
      RunStep(seen, tool.exe, build, directory, deadline);
  const RunEnd build_end = EndOf(built, kCompilerOutOfMemory);
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
      RunStep(seen, program, test_case.program_args, directory, deadline);
  seen.end = EndOf(run, kSimulationOutOfMemory);
  ReadOutputs(test_case, directory, seen);
  ReadMarkers(run.stdout_text, seen);
  ReadReports(run.stderr_text, seen);
  return seen;
}

}  // namespace conformance::adapters::lyra
