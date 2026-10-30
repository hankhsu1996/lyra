// Holds one tool to the suite: every case named on the command line, or every
// case where none is, judged by the claims every tool is held to, with the
// verdict and the evidence for it printed per case. The verdict is the
// standard's alone; whether a tool is known to give it is the tool's own
// business, so nothing here is a gate and nothing here is recorded.

#include <algorithm>
#include <atomic>
#include <charconv>
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
#include "conformance/framework/verdict.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;

namespace {

namespace framework = conformance::framework;

constexpr std::string_view kUsage =
    "usage: {} --tool lyra --lyra <binary>\n"
    "       {} --tool verilator [--verilator <binary>] [--cc <compiler>]\n"
    "  common: [--jobs N] [--address-limit MiB] [case id...]\n";

// How the run is to go, from the command line. `--tool` names the tool, and
// the options after it name where that tool's own binaries are. `--jobs N` (or
// `-j N`) is how many cases are judged at once, one by default, as make and
// lit take it; `--address-limit MiB` is how much address space each process
// the run starts may hold, none by default; the rest are the ids of the cases
// to judge, every case where none is named. Both numbers are the caller's,
// since only the caller knows the machine. A limit is per process, so what one
// case may hold does not depend on how many run beside it.
struct Request {
  std::string tool;
  std::optional<std::filesystem::path> lyra;
  std::optional<std::filesystem::path> verilator;
  std::optional<std::filesystem::path> c_compiler;
  int jobs = 1;
  std::optional<std::uint64_t> address_limit_mib;
  std::vector<std::string_view> ids;
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
    const bool takes_value = arg == "--tool" || arg == "--lyra" ||
                             arg == "--verilator" || arg == "--cc" ||
                             arg == "--jobs" || arg == "-j" ||
                             arg == "--address-limit";
    if (!takes_value) {
      request.ids.push_back(arg);
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
    } else {
      const std::optional<std::uint64_t> count = ReadCount(value);
      if (!count) {
        return std::nullopt;
      }
      if (arg == "--address-limit") {
        request.address_limit_mib = count;
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

// The tool the run holds to the suite, as its reader, and what the run says of
// the tool beside the verdicts.
struct ChosenTool {
  std::unique_ptr<framework::Reader> reader;
  std::string identity;
};

auto ChooseTool(const Request& request)
    -> std::expected<ChosenTool, std::string> {
  if (request.tool == "lyra") {
    if (!request.lyra) {
      return std::unexpected("--tool lyra needs --lyra <binary>");
    }
    return ChosenTool{
        .reader = std::make_unique<conformance::adapters::lyra::Reader>(
            conformance::adapters::lyra::Tool{
                .exe = *request.lyra, .build_options = {}}),
        .identity = fmt::format("lyra at {}", request.lyra->string())};
  }
  if (request.tool == "verilator") {
    auto found = conformance::adapters::verilator::FindTool(
        request.verilator, request.c_compiler);
    if (!found) {
      return std::unexpected(found.error());
    }
    std::string identity = conformance::adapters::verilator::VersionOf(*found);
    return ChosenTool{
        .reader = std::make_unique<conformance::adapters::verilator::Reader>(
            *std::move(found)),
        .identity = std::move(identity)};
  }
  return std::unexpected(
      fmt::format(
          "'{}' is not a tool this suite has a reader for", request.tool));
}

// The ceiling the caller put around the whole run, where the machine says: the
// memory limit of the control group this process runs in. It is read only to
// be stated beside the verdicts, so a machine with no such thing says so.
auto MemoryCeilingOfTheRun() -> std::string {
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
  return "not known";
}

}  // namespace

auto main(int argc, char** argv) -> int {
  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
  const std::optional<Request> request = ReadRequest(args);
  if (!request) {
    fmt::print(stderr, kUsage, args.front(), args.front());
    return 1;
  }
  const auto tool = ChooseTool(*request);
  if (!tool) {
    fmt::print(stderr, "{}\n", tool.error());
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
  // What the verdicts below were reached under, so a verdict read later can be
  // told apart from one another tool, or another limit, would give.
  fmt::print(
      "# {}; {} jobs; address limit {}; memory ceiling of the whole run {}\n",
      tool->identity, request->jobs,
      request->address_limit_mib
          ? fmt::format("{} MiB per process", *request->address_limit_mib)
          : std::string{"none"},
      MemoryCeilingOfTheRun());
  // Each case is built in a directory of its own, so the corpus is named from
  // the root rather than from wherever this was started. The suite's checks of
  // its own helpers are run beside the cases, since a tool failing one shows
  // the helper wrong rather than the tool.
  const std::filesystem::path harness = std::filesystem::absolute(
      runfiles->Rlocation("_main/conformance/harness"));
  std::vector<framework::Case> corpus;
  for (const std::string_view root :
       {"_main/conformance/cases", "_main/conformance/selftest"}) {
    std::vector<framework::Case> found = framework::LoadCases(
        std::filesystem::absolute(runfiles->Rlocation(std::string(root))),
        harness);
    corpus.insert(
        corpus.end(), std::make_move_iterator(found.begin()),
        std::make_move_iterator(found.end()));
  }
  std::vector<const framework::Case*> chosen;
  for (const framework::Case& c : corpus) {
    if (request->ids.empty() ||
        std::ranges::find(request->ids, c.id) != request->ids.end()) {
      chosen.push_back(&c);
    }
  }
  if (chosen.empty()) {
    fmt::print(stderr, "no case of the corpus was named\n");
    return 1;
  }

  // Each worker takes the next case not yet taken, and a case's verdict and
  // evidence are written whole, so the output reads case by case in the order
  // the cases finished.
  std::atomic<std::size_t> next{0};
  std::atomic<bool> harness_failed{false};
  std::mutex output;
  const auto work = [&] {
    for (std::size_t i = next++; i < chosen.size(); i = next++) {
      const framework::Case& c = *chosen[i];
      const auto result = framework::RunOneCase(c, *tool->reader);
      if (!result) {
        const std::scoped_lock lock{output};
        fmt::print(stderr, "{}: {}\n", c.id, result.error());
        harness_failed = true;
        continue;
      }
      std::string text =
          fmt::format("{}: {}\n", c.id, framework::Describe(result->verdict));
      if (!std::holds_alternative<framework::Conforms>(result->verdict)) {
        text += fmt::format("{}\n", result->seen.evidence);
      }
      const std::scoped_lock lock{output};
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
  return harness_failed ? 1 : 0;
}
