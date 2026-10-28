#include "verilator_adapter.hpp"

#include <array>
#include <chrono>
#include <cstdint>
#include <filesystem>
#include <format>
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

// A guard against a build or a run that never ends. Verilator compiles every
// case's C++ with the host compiler, which is the slow half.
constexpr std::chrono::seconds kStuckAfter{600};

// The line Verilator ends a refusal of the source with, which a failure to
// build what it wrote from an accepted source never carries, and the words it
// marks a refusal with that is not of the program but of its own: a construct
// it does not carry out, and a bug of its own.
constexpr std::string_view kRefusalEnding = "%Error: Exiting due to";
constexpr std::string_view kUnsupported = "%Error-UNSUPPORTED";
constexpr std::string_view kInternalError = "Internal Error";

// How a build that produced no program ended, read off everything it wrote.
auto BuildEnd(std::string_view written) -> RunEnd {
  if (written.find(kInternalError) != std::string_view::npos) {
    return ToolFailed{"the tool reported a bug of its own"};
  }
  if (written.find(kUnsupported) != std::string_view::npos) {
    return ToolFailed{"the tool refused a construct it does not carry out"};
  }
  if (written.find(kRefusalEnding) != std::string_view::npos) {
    return Rejected{};
  }
  return ToolFailed{"the tool did not build a program from its output"};
}

// The run summary Verilator's program prints on every run, which is no report.
constexpr std::array<std::string_view, 2> kSummaryOpenings = {
    "- S i m u l a t i o n   R e p o r t", "- Verilator: "};

// The words a report of Verilator's opens with, after an optional time, and
// the kind each names.
constexpr std::array<std::pair<std::string_view, ReportKind>, 5> kTags = {{
    {"%Fatal: ", ReportKind::kFatal},
    {"%Error: ", ReportKind::kError},
    {"%Warning: ", ReportKind::kWarning},
    {"-Info: ", ReportKind::kInfo},
    {"- ", ReportKind::kInfo},
}};

// The source line of where a report was made, written `file:line: `, and what
// follows it.
struct Placed {
  std::uint64_t line = 0;
  std::string_view rest;
};

auto ReadPlace(std::string_view text) -> std::optional<Placed> {
  const std::size_t file_end = text.find(':');
  if (file_end == std::string_view::npos) {
    return std::nullopt;
  }
  const std::size_t line_end = text.find(": ", file_end + 1);
  if (line_end == std::string_view::npos) {
    return std::nullopt;
  }
  const std::optional<std::uint64_t> line =
      ReadDecimal(text.substr(file_end + 1, line_end - file_end - 1));
  if (!line) {
    return std::nullopt;
  }
  return Placed{.line = *line, .rest = text.substr(line_end + 2)};
}

// One line either stream carried. The program's own output and the run summary
// are passed over; a report is placed where its words allow and left unplaced
// where they do not.
void ReadLine(std::string_view line, Observation& seen) {
  for (const std::string_view opening : kSummaryOpenings) {
    if (line.starts_with(opening)) {
      return;
    }
  }
  std::string_view text = line;
  std::optional<std::uint64_t> time;
  if (text.starts_with('[')) {
    const std::size_t close = text.find("] ");
    if (close == std::string_view::npos) {
      return;
    }
    time = ReadDecimal(text.substr(1, close - 1));
    if (!time) {
      return;
    }
    text.remove_prefix(close + 2);
  }
  for (const auto& [tag, severity] : kTags) {
    if (!text.starts_with(tag)) {
      continue;
    }
    const std::optional<Placed> placed = ReadPlace(text.substr(tag.size()));
    if (!placed) {
      if (tag == "- ") {
        return;
      }
      seen.unplaced.emplace_back(line);
      return;
    }
    const std::string_view rest = placed->rest;
    ObservedReport report{
        .kind = severity,
        .line = placed->line,
        .time = time,
        .scope = std::nullopt,
        .message = std::nullopt,
        .written = std::string(line)};
    if (rest == "Verilog $stop") {
      report.kind = ReportKind::kStop;
    } else if (rest == "Verilog $finish") {
      report.kind = ReportKind::kFinish;
    } else if (tag == "- ") {
      return;
    } else {
      constexpr std::string_view kFailedIn = "Assertion failed in ";
      std::string_view scoped = rest;
      if (scoped.starts_with(kFailedIn)) {
        scoped.remove_prefix(kFailedIn.size());
      }
      // A qualified statement's violation report names the statement and says
      // it was violated (LRM 12.4.2.1), in the tool's words alone.
      const bool violation = rest.ends_with("statement violated");
      if (violation) {
        report.kind = ReportKind::kViolation;
      }
      // The program's message follows the scope; where no scope is written,
      // nothing marks where the tool's words end and the message begins.
      const std::size_t scope_end = scoped.find(": ");
      if (scope_end != std::string_view::npos) {
        report.scope = std::string(scoped.substr(0, scope_end));
        if (!violation) {
          report.message = std::string(scoped.substr(scope_end + 2));
        }
      }
    }
    seen.reports.push_back(std::move(report));
    return;
  }
}

}  // namespace

void ReadVerilatorReports(std::string_view stream, Observation& seen) {
  std::istringstream lines{std::string(stream)};
  std::string line;
  while (std::getline(lines, line)) {
    ReadLine(line, seen);
  }
}

auto ObserveWithVerilator(
    const VerilatorTool& tool, const ConformanceCase& test_case,
    const std::filesystem::path& directory) -> Observation {
  Observation seen;
  if (std::optional<HarnessFailed> missing = MissingInput(test_case)) {
    seen.end = std::move(*missing);
    return seen;
  }
  // Every file on Verilator's command line is one compilation unit, and it
  // has no use model in which each file is one of its own.
  if (test_case.compilation_units == CompilationUnits::kSeparate) {
    seen.end = ToolLacks{
        .use_model =
            "use model compiling each file as a separate compilation unit",
        .clause = "LRM 3.12.1"};
    return seen;
  }
  const std::filesystem::path objects = directory / "obj";
  std::vector<std::string> build{"--binary",       "--timing",   "-Wno-fatal",
                                 "-Wno-lint",      "-Wno-style", "--Mdir",
                                 objects.string(), "-o",         "program"};
  if (test_case.tops.size() == 1) {
    build.emplace_back("--top-module");
    build.push_back(test_case.tops.front());
  }
  if (test_case.library_map.has_value()) {
    build.emplace_back("-libmap");
    build.push_back(test_case.library_map->string());
  }
  for (const std::filesystem::path& supporting : test_case.supporting_sources) {
    build.push_back(supporting.string());
  }
  build.push_back(test_case.entry.string());
  // Verilator compiles every source it is given as C++, while the boundary is
  // one of C linkage (LRM 35.5.1.6), so a C source is compiled as C first and
  // Verilator is handed the object.
  for (const std::filesystem::path& native : test_case.link_sources) {
    if (native.extension() != ".c") {
      build.push_back(native.string());
      continue;
    }
    const std::filesystem::path object =
        directory / (native.filename().string() + ".o");
    const std::vector<std::string> compile{"-c", native.string(),
                                           "-I", tool.standard_headers.string(),
                                           "-o", object.string()};
    const ProcessOutcome compiled =
        RunChildProcess(tool.c_compiler, compile, kStuckAfter, directory);
    RecordCommand(seen, tool.c_compiler, compile, compiled);
    if (ExitCodeOf(compiled) != 0) {
      seen.end = HarnessFailed{std::format(
          "'{}' does not compile as C against the standard header",
          native.string())};
      return seen;
    }
    build.push_back(object.string());
  }

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
    seen.end = BuildEnd(built.stdout_text + built.stderr_text);
    return seen;
  }

  const std::filesystem::path program = objects / "program";
  const ProcessOutcome run =
      RunChildProcess(program, test_case.program_args, kStuckAfter, directory);
  RecordCommand(seen, program, test_case.program_args, run);
  seen.end = EndOf(run);
  ReadMarkers(run.stdout_text, seen);
  ReadVerilatorReports(run.stdout_text, seen);
  ReadVerilatorReports(run.stderr_text, seen);
  return seen;
}

}  // namespace lyra::test
