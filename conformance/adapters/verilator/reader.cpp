#include "conformance/adapters/verilator/reader.hpp"

#include <algorithm>
#include <array>
#include <chrono>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "conformance/framework/case.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/process.hpp"

namespace conformance::adapters::verilator {

using framework::Case;
using framework::CompilationUnits;
using framework::EndOf;
using framework::ExitCodeOf;
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
using framework::RunChildProcess;
using framework::RunEnd;
using framework::RunStep;
using framework::ToolFailed;
using framework::ToolIdentity;
using framework::ToolLacks;
using framework::VersionOf;
namespace {

// The C++ compiler Verilator's build file sets for every build, written
// `CXX = <name>` in `include/verilated.mk` under where Verilator is
// installed, and found on PATH as make finds it.
auto BuildCompilerOf(const std::filesystem::path& root)
    -> std::optional<std::filesystem::path> {
  std::ifstream build_file(root / "include" / "verilated.mk");
  constexpr std::string_view kSetting = "CXX = ";
  std::string line;
  while (std::getline(build_file, line)) {
    if (!line.starts_with(kSetting)) {
      continue;
    }
    const std::string name = line.substr(kSetting.size());
    if (std::filesystem::path(name).is_absolute()) {
      return name;
    }
    return FindOnPath(name);
  }
  return std::nullopt;
}

// What Verilator's guide documents it reads from the environment, among what a
// build and run of a case can reach: where it is installed and which binary it
// runs, the make it builds with and that make's flags, a wrapper put in front
// of the C++ compiler, the solver it randomizes with, and how its program
// places threads.
constexpr std::array<std::string_view, 7> kEnvironmentRead = {
    "VERILATOR_ROOT",
    "VERILATOR_BIN",
    "MAKE",
    "MAKEFLAGS",
    "OBJCACHE",
    "VERILATOR_SOLVER",
    "VERILATOR_NUMA_STRATEGY"};

// The line Verilator ends a refusal of the source with, which a failure to
// build what it wrote from an accepted source never carries, and the words it
// marks a refusal with that is not of the program but of its own: a construct
// it does not carry out, and a bug of its own.
constexpr std::string_view kRefusalEnding = "%Error: Exiting due to";
constexpr std::string_view kUnsupported = "%Error-UNSUPPORTED";
constexpr std::string_view kInternalError = "Internal Error";

// How Verilator, the host compiler its build runs, and a program it built say
// they ran out of memory.
constexpr std::array<std::string_view, 3> kOutOfMemory = {
    "std::bad_alloc", "virtual memory exhausted", "out of memory allocating"};

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

// A qualified statement's violation report names the statement in the tool's
// words alone (LRM 12.4.2.1, 12.5.3.1): an if's says it was violated, and a
// case's opens with the statement and goes on to what it found.
constexpr std::string_view kIfViolationEnding = "statement violated";
constexpr std::array<std::string_view, 3> kCaseViolationOpenings = {
    "unique case, but ", "unique0 case, but ", "priority case, but "};

auto IsViolation(std::string_view said) -> bool {
  return said.ends_with(kIfViolationEnding) ||
         std::ranges::any_of(
             kCaseViolationOpenings, [&](std::string_view opening) {
               return said.starts_with(opening);
             });
}

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
      // The program's message follows the scope; where no scope is written,
      // nothing marks where the tool's words end and the message begins.
      const std::size_t scope_end = scoped.find(": ");
      const std::string_view said = scope_end == std::string_view::npos
                                        ? scoped
                                        : scoped.substr(scope_end + 2);
      const bool violation = IsViolation(said);
      if (violation) {
        report.kind = ReportKind::kViolation;
      }
      if (scope_end != std::string_view::npos) {
        report.scope = std::string(scoped.substr(0, scope_end));
        if (!violation) {
          report.message = std::string(said);
        }
      }
    }
    seen.reports.push_back(std::move(report));
    return;
  }
}

}  // namespace

void ReadReports(std::string_view stream, Observation& seen) {
  std::istringstream lines{std::string(stream)};
  std::string line;
  while (std::getline(lines, line)) {
    ReadLine(line, seen);
  }
}

auto FindTool(
    std::optional<std::filesystem::path> exe,
    std::optional<std::filesystem::path> c_compiler,
    std::vector<std::string> build_options)
    -> std::expected<Tool, std::string> {
  if (!exe) {
    exe = FindOnPath("verilator");
  }
  if (!c_compiler) {
    c_compiler = FindOnPath("cc");
  }
  if (!exe || !c_compiler) {
    return std::unexpected("this needs verilator and cc, named or on PATH");
  }
  // Where Verilator keeps what it ships, as it reports it, so the standard
  // header a DPI-C source includes is the one it was built against.
  const std::vector<std::string> ask{"--getenv", "VERILATOR_ROOT"};
  const ProcessOutcome root =
      RunChildProcess(*exe, ask, framework::kQuestionAnsweredWithin);
  std::string_view root_text = root.stdout_text;
  while (root_text.ends_with('\n')) {
    root_text.remove_suffix(1);
  }
  if (ExitCodeOf(root) != 0 || root_text.empty()) {
    return std::unexpected(
        std::format("'{}' did not say where it is installed", exe->string()));
  }
  const std::filesystem::path installed(root_text);
  return Tool{
      .exe = *exe,
      .c_compiler = *c_compiler,
      .standard_headers = installed / "include" / "vltstd",
      .cxx = BuildCompilerOf(installed),
      .build_options = std::move(build_options)};
}

Reader::Reader(Tool tool) : tool_(std::move(tool)) {
}

auto Reader::Identity() const -> ToolIdentity {
  std::vector<Program> programs{
      Program{
          .role = "verilator",
          .path = tool_.exe,
          .version = VersionOf(tool_.exe)},
      Program{
          .role = "c compiler",
          .path = tool_.c_compiler,
          .version = VersionOf(tool_.c_compiler)}};
  if (tool_.cxx) {
    programs.push_back(
        Program{
            .role = "c++ compiler",
            .path = *tool_.cxx,
            .version = VersionOf(*tool_.cxx)});
  }
  return ToolIdentity{
      .name = "verilator",
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
  // A case is the unit a caller runs side by side with others, so its own
  // build compiles one file at a time rather than taking every core. Verilator
  // carries out the dump tasks (LRM 21.7) only in a build asked for them.
  std::vector<std::string> build{"--binary",       "--timing",   "--trace",
                                 "--build-jobs",   "1",          "-Wno-fatal",
                                 "-Wno-lint",      "-Wno-style", "--Mdir",
                                 objects.string(), "-o",         "program"};
  build.insert(
      build.end(), tool.build_options.begin(), tool.build_options.end());
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
        RunStep(seen, tool.c_compiler, compile, directory, deadline);
    // A compile stopped at a limit says nothing of the source, so it ends the
    // case the way any stopped command does; only one that ran and failed
    // says the source does not compile.
    const RunEnd compile_end = EndOf(compiled, kOutOfMemory);
    if (!std::holds_alternative<Ran>(compile_end)) {
      seen.end = compile_end;
      return seen;
    }
    if (ExitCodeOf(compiled) != 0) {
      seen.end = HarnessFailed{std::format(
          "'{}' does not compile as C against the standard header",
          native.string())};
      return seen;
    }
    build.push_back(object.string());
  }

  const ProcessOutcome built =
      RunStep(seen, tool.exe, build, directory, deadline);
  const RunEnd build_end = EndOf(built, kOutOfMemory);
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
      RunStep(seen, program, test_case.program_args, directory, deadline);
  seen.end = EndOf(run, kOutOfMemory);
  ReadOutputs(test_case, directory, seen);
  ReadMarkers(run.stdout_text, seen);
  ReadReports(run.stdout_text, seen);
  ReadReports(run.stderr_text, seen);
  return seen;
}

}  // namespace conformance::adapters::verilator
