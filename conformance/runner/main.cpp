// Holds one tool to the suite: every case named on the command line, or every
// case where none is, judged by the claims every tool is held to, with the
// verdict and the evidence for it printed per case, and the whole run written
// as JSON where the caller names a file for it. The verdict is the standard's
// alone; whether a tool is known to give it is the tool's own business, so
// nothing here is a gate and nothing here is recorded against the tool.

#include <algorithm>
#include <atomic>
#include <charconv>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <expected>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <memory>
#include <mutex>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <sys/resource.h>
#include <sys/utsname.h>
#include <system_error>
#include <thread>
#include <utility>
#include <variant>
#include <vector>

#include <fmt/core.h>

#include "conformance/adapters/lyra/reader.hpp"
#include "conformance/adapters/verilator/reader.hpp"
#include "conformance/framework/case.hpp"
#include "conformance/framework/execution.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/results.hpp"
#include "conformance/framework/verdict.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;

namespace {

namespace framework = conformance::framework;

constexpr std::string_view kUsage =
    "usage: {} --tool lyra --lyra <binary>\n"
    "       {} --tool verilator [--verilator <binary>] [--cc <compiler>]\n"
    "  common: [--jobs N] [--time-limit SECONDS] [--address-limit MiB]\n"
    "          [--build-option ARG]... [--output FILE]\n"
    "          [--suite-revision REV] [--tool-revision REV] [case id...]\n";

// How the run is to go, from the command line. `--tool` names the tool, and
// the options after it name where that tool's own binaries are. `--jobs N` (or
// `-j N`) is how many cases are judged at once, one by default, as make and
// lit take it; `--time-limit SECONDS` is how long one case may take, all its
// commands together, and `--address-limit MiB` how much address space each
// process the run starts may hold, neither limited by default. Those numbers
// are the caller's, since only the caller knows the machine; a limit is per
// case and per process, so what one case may take does not depend on how many
// run beside it. `--build-option ARG`, given once per argument, is an option
// every case's build is given, as a tool's own command line takes it -- which
// path Lyra takes, say -- so one tool held under two such sets is two
// configurations, each with its own record. `--output FILE` is where the whole
// run is written as JSON. The two revisions are what the caller knows of the
// suite's text and the tool's build, which neither this program nor the tool
// can say of itself; each is written as unknown where it is not given. The rest
// are the ids of the cases to judge, every case where none is named.
struct Request {
  std::string tool;
  std::optional<std::filesystem::path> lyra;
  std::optional<std::filesystem::path> verilator;
  std::optional<std::filesystem::path> c_compiler;
  int jobs = 1;
  std::optional<std::chrono::seconds> time_limit;
  std::optional<std::uint64_t> address_limit_mib;
  std::vector<std::string> build_options;
  std::optional<std::filesystem::path> output;
  std::optional<std::string> suite_revision;
  std::optional<std::string> tool_revision;
  std::vector<std::string> ids;
};

auto ReadCount(std::string_view text) -> std::optional<std::uint64_t> {
  std::uint64_t value = 0;
  const auto [end, failed] =
      std::from_chars(text.data(), text.data() + text.size(), value);
  if (failed != std::errc{} || end != text.data() + text.size() || value == 0) {
    return std::nullopt;
  }
  return value;
}

// A path the caller wrote, as the caller meant it. `bazel run` starts the
// program inside its runfiles and says where it was started from, so a
// relative path is taken from there rather than from where the program runs.
auto ResolveCallerPath(std::string_view written) -> std::filesystem::path {
  const std::filesystem::path path(written);
  if (path.is_absolute()) {
    return path.lexically_normal();
  }
  const char* started_in = std::getenv("BUILD_WORKING_DIRECTORY");
  if (started_in != nullptr && *started_in != '\0') {
    return (std::filesystem::path(started_in) / path).lexically_normal();
  }
  return std::filesystem::absolute(path).lexically_normal();
}

auto ReadRequest(std::span<char* const> args) -> std::optional<Request> {
  Request request;
  for (std::size_t i = 1; i < args.size(); ++i) {
    const std::string_view arg = args[i];
    const bool takes_value =
        arg == "--tool" || arg == "--lyra" || arg == "--verilator" ||
        arg == "--cc" || arg == "--jobs" || arg == "-j" ||
        arg == "--time-limit" || arg == "--address-limit" ||
        arg == "--build-option" || arg == "--output" ||
        arg == "--suite-revision" || arg == "--tool-revision";
    if (!takes_value) {
      request.ids.emplace_back(arg);
      continue;
    }
    if (i + 1 == args.size()) {
      return std::nullopt;
    }
    const std::string_view value = args[++i];
    if (arg == "--tool") {
      request.tool = value;
    } else if (arg == "--lyra") {
      request.lyra = ResolveCallerPath(value);
    } else if (arg == "--verilator") {
      request.verilator = ResolveCallerPath(value);
    } else if (arg == "--cc") {
      request.c_compiler = ResolveCallerPath(value);
    } else if (arg == "--build-option") {
      request.build_options.emplace_back(value);
    } else if (arg == "--output") {
      request.output = ResolveCallerPath(value);
    } else if (arg == "--suite-revision") {
      request.suite_revision = value;
    } else if (arg == "--tool-revision") {
      request.tool_revision = value;
    } else {
      const std::optional<std::uint64_t> count = ReadCount(value);
      if (!count) {
        return std::nullopt;
      }
      if (arg == "--address-limit") {
        request.address_limit_mib = count;
      } else if (arg == "--time-limit") {
        request.time_limit = std::chrono::seconds(*count);
      } else {
        request.jobs = static_cast<int>(*count);
      }
    }
  }
  if (request.tool.empty()) {
    return std::nullopt;
  }
  return request;
}

auto ChooseTool(const Request& request)
    -> std::expected<std::unique_ptr<framework::Reader>, std::string> {
  if (request.tool == "lyra") {
    if (!request.lyra) {
      return std::unexpected("--tool lyra needs --lyra <binary>");
    }
    return std::make_unique<conformance::adapters::lyra::Reader>(
        conformance::adapters::lyra::Tool{
            .exe = *request.lyra, .build_options = request.build_options});
  }
  if (request.tool == "verilator") {
    auto found = conformance::adapters::verilator::FindTool(
        request.verilator, request.c_compiler, request.build_options);
    if (!found) {
      return std::unexpected(found.error());
    }
    return std::make_unique<conformance::adapters::verilator::Reader>(
        *std::move(found));
  }
  return std::unexpected(
      fmt::format(
          "'{}' is not a tool this suite has a reader for", request.tool));
}

// The ceiling the caller put around the whole run, where the machine says: the
// memory limit of the control group this process runs in, "none" where that
// group has none, and nothing where the machine does not say.
auto MemoryCeilingOfTheRun() -> std::optional<std::string> {
  std::ifstream membership("/proc/self/cgroup");
  std::string line;
  while (std::getline(membership, line)) {
    if (!line.starts_with("0::")) {
      continue;
    }
    std::ifstream limit(
        std::filesystem::path("/sys/fs/cgroup") / line.substr(4) /
        "memory.max");
    std::string value;
    if (limit >> value) {
      return value == "max" ? std::string{"none"} : value + " bytes";
    }
  }
  return std::nullopt;
}

// The operating system and machine the run was made on, as the kernel names
// them.
auto Host() -> std::optional<std::string> {
  utsname name{};
  if (uname(&name) != 0) {
    return std::nullopt;
  }
  return fmt::format(
      "{} {} {}", static_cast<const char*>(name.sysname),
      static_cast<const char*>(name.release),
      static_cast<const char*>(name.machine));
}

// The tool, as the first line of the run names it.
auto DescribeTool(const framework::ToolIdentity& tool) -> std::string {
  std::string text = tool.name;
  for (const framework::Program& program : tool.programs) {
    text += fmt::format(
        "; {} {} ({})", program.role, program.path.string(),
        program.version.value_or("no version given"));
  }
  return text;
}

// Writes the run beside where it is to go and then moves it there, so a file
// at that name is always a run that finished.
auto WriteResults(
    const framework::Results& results, const std::filesystem::path& to)
    -> std::expected<void, std::string> {
  std::filesystem::path partial = to;
  partial += ".partial";
  {
    std::ofstream out(partial, std::ios::binary);
    out << framework::ToJson(results);
    if (!out) {
      return std::unexpected(
          fmt::format("could not write '{}'", partial.string()));
    }
  }
  std::error_code moved;
  std::filesystem::rename(partial, to, moved);
  if (moved) {
    return std::unexpected(
        fmt::format(
            "could not move '{}' to '{}': {}", partial.string(), to.string(),
            moved.message()));
  }
  return {};
}

}  // namespace

auto main(int argc, char** argv) -> int {
  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
  const std::optional<Request> request = ReadRequest(args);
  if (!request) {
    fmt::print(stderr, kUsage, args.front(), args.front());
    return 1;
  }
  const auto reader = ChooseTool(*request);
  if (!reader) {
    fmt::print(stderr, "{}\n", reader.error());
    return 1;
  }
  if (request->address_limit_mib) {
    const rlim_t bytes = *request->address_limit_mib << 20U;
    const rlimit limit{.rlim_cur = bytes, .rlim_max = bytes};
    if (setrlimit(RLIMIT_AS, &limit) != 0) {
      fmt::print(
          stderr, "could not limit the address space to {} MiB\n",
          *request->address_limit_mib);
      return 1;
    }
  }
  std::string error;
  std::unique_ptr<Runfiles> runfiles{Runfiles::Create(args.front(), &error)};
  if (!runfiles) {
    fmt::print(stderr, "failed to create runfiles: {}\n", error);
    return 1;
  }
  // Each case is built in a directory of its own, so the corpus is named from
  // the root rather than from wherever this was started. The suite's checks of
  // its own helpers are run beside the cases, since a tool failing one shows
  // the helper wrong rather than the tool.
  const std::filesystem::path suite =
      std::filesystem::absolute(runfiles->Rlocation("_main/conformance"))
          .lexically_normal();
  std::vector<framework::Case> corpus;
  for (const std::string_view root : {"cases", "selftest"}) {
    std::vector<framework::Case> found =
        framework::LoadCases(suite / root, suite / "harness");
    corpus.insert(
        corpus.end(), std::make_move_iterator(found.begin()),
        std::make_move_iterator(found.end()));
  }
  // A suite with no case is one whose cases were not found where they were
  // looked for, and a name that is no case would otherwise be passed over;
  // either way the run would stand as an account of what was asked for while
  // missing some of it.
  if (corpus.empty()) {
    fmt::print(stderr, "no case of the suite is in '{}'\n", suite.string());
    return 1;
  }
  for (const std::string& id : request->ids) {
    if (std::ranges::find(corpus, id, &framework::Case::id) == corpus.end()) {
      fmt::print(stderr, "'{}' is no case of the corpus\n", id);
      return 1;
    }
  }
  std::vector<const framework::Case*> chosen;
  for (const framework::Case& c : corpus) {
    if (request->ids.empty() ||
        std::ranges::find(request->ids, c.id) != request->ids.end()) {
      chosen.push_back(&c);
    }
  }

  framework::ToolIdentity tool = (*reader)->Identity();
  std::vector<framework::EnvironmentVariable> environment =
      framework::EnvironmentOf(tool);
  framework::Results results{
      .suite_revision = request->suite_revision,
      .suite_directory = suite,
      .tool_revision = request->tool_revision,
      .tool = std::move(tool),
      .environment = std::move(environment),
      .host = Host(),
      .jobs = request->jobs,
      .time_limit = request->time_limit,
      .address_limit_mib = request->address_limit_mib,
      .memory_ceiling = MemoryCeilingOfTheRun(),
      .selection = request->ids.empty()
                       ? std::nullopt
                       : std::optional<std::vector<std::string>>(request->ids),
      .started = std::chrono::system_clock::now(),
      .ended = {},
      .cases = {}};
  // What the verdicts below were reached under, so a verdict read later can be
  // told apart from one another tool, or another limit, would give.
  fmt::print(
      "# {}; {} jobs; time limit {}; address limit {}; memory ceiling of the "
      "whole run {}\n",
      DescribeTool(results.tool), request->jobs,
      request->time_limit
          ? fmt::format("{} s per case", request->time_limit->count())
          : std::string{"none"},
      request->address_limit_mib
          ? fmt::format("{} MiB per process", *request->address_limit_mib)
          : std::string{"none"},
      results.memory_ceiling.value_or("not known"));

  // Each worker takes the next case not yet taken, so every case chosen is
  // judged exactly once, and a case's verdict and evidence are written whole,
  // so the output reads case by case in the order the cases finished.
  std::atomic<std::size_t> next{0};
  std::mutex output;
  const auto work = [&] {
    for (std::size_t i = next++; i < chosen.size(); i = next++) {
      const framework::Case& c = *chosen[i];
      framework::CaseResult result =
          framework::RunOneCase(c, **reader, request->time_limit);
      std::string text =
          fmt::format("{}: {}\n", c.id, framework::Describe(result.verdict));
      if (!std::holds_alternative<framework::Conforms>(result.verdict)) {
        text += fmt::format("{}\n", framework::Evidence(result.seen));
      }
      const std::scoped_lock lock{output};
      results.cases.push_back(
          framework::JudgedCase{.id = c.id, .result = std::move(result)});
      fmt::print("{}", text);
      std::fflush(stdout);
    }
  };
  {
    std::vector<std::jthread> workers;
    for (int w = 0; w < request->jobs; ++w) {
      workers.emplace_back(work);
    }
  }
  results.ended = std::chrono::system_clock::now();
  if (request->output) {
    if (const auto written = WriteResults(results, *request->output);
        !written) {
      fmt::print(stderr, "{}\n", written.error());
      return 1;
    }
  }
  // A case the harness could not hand to the tool says nothing of the tool, so
  // the run did not do what it was asked to, whatever the verdicts were.
  const bool not_run =
      std::ranges::any_of(results.cases, [](const framework::JudgedCase& c) {
        return std::holds_alternative<framework::NotRun>(c.result.verdict);
      });
  return not_run ? 1 : 0;
}
