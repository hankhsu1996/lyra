// Holds Verilator to the corpus: every case named on the command line, or the
// whole corpus where none is, judged by the same claims Lyra is held to, with
// the verdict and the evidence for it printed per case. Nothing here is a gate.
// Verilator is a second tool the corpus is run against to learn whether a case
// says only what the standard requires, and what it answers is read case by
// case rather than recorded.

#include <algorithm>
#include <atomic>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <memory>
#include <mutex>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <sys/resource.h>
#include <system_error>
#include <thread>
#include <variant>
#include <vector>

#include <fmt/core.h>

#include "tests/framework/cli_fixture.hpp"
#include "tests/framework/conformance_case.hpp"
#include "tests/framework/conformance_observation.hpp"
#include "tests/framework/conformance_verdict.hpp"
#include "tests/framework/process.hpp"
#include "tests/framework/verilator_adapter.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;

namespace {

// A program as the shell that started this would find it. Verilator and the C
// compiler are the machine's own installs, never build inputs, which is why
// this runs only where asked.
auto FindOnPath(std::string_view name) -> std::filesystem::path {
  const char* path = std::getenv("PATH");
  std::string_view rest = path == nullptr ? "" : path;
  while (!rest.empty()) {
    const std::size_t colon = rest.find(':');
    const std::filesystem::path candidate =
        std::filesystem::path(rest.substr(0, colon)) / name;
    if (std::filesystem::exists(candidate)) {
      return candidate;
    }
    rest.remove_prefix(
        colon == std::string_view::npos ? rest.size() : colon + 1);
  }
  return {};
}

// How the run is to go, from the command line: `--jobs N` (or `-j N`), how
// many cases are judged at once, one by default, as make and lit take it;
// `--address-limit MiB`, how much address space each process the run starts
// may hold, none by default; and the ids of the cases to judge, every case
// where none is named. Both numbers are the caller's, since only the caller
// knows the machine. A limit is per process, so what one case may hold does not
// depend on how many run beside it.
struct Request {
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

auto ReadRequest(std::span<char* const> args) -> std::optional<Request> {
  Request request;
  for (std::size_t i = 1; i < args.size(); ++i) {
    const std::string_view arg = args[i];
    const bool jobs = arg == "--jobs" || arg == "-j";
    if (!jobs && arg != "--address-limit") {
      request.ids.push_back(arg);
      continue;
    }
    if (i + 1 == args.size()) {
      return std::nullopt;
    }
    const std::optional<std::uint64_t> value = ReadCount(args[++i]);
    if (!value) {
      return std::nullopt;
    }
    if (jobs) {
      request.jobs = static_cast<int>(*value);
    } else {
      request.address_limit_mib = value;
    }
  }
  return request;
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
    fmt::print(
        stderr, "usage: {} [--jobs N] [--address-limit MiB] [case id...]\n",
        args.front());
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
  const std::filesystem::path verilator = FindOnPath("verilator");
  const std::filesystem::path c_compiler = FindOnPath("cc");
  if (verilator.empty() || c_compiler.empty()) {
    fmt::print(stderr, "this needs verilator and cc on PATH\n");
    return 1;
  }
  // Where Verilator keeps what it ships, as it reports it, so the standard
  // header a DPI-C source includes is the one it was built against.
  const std::vector<std::string> ask{"--getenv", "VERILATOR_ROOT"};
  const lyra::test::ProcessOutcome root =
      lyra::test::RunChildProcess(verilator, ask);
  std::string_view root_text = root.stdout_text;
  while (root_text.ends_with('\n')) {
    root_text.remove_suffix(1);
  }
  if (lyra::test::ExitCodeOf(root) != 0 || root_text.empty()) {
    fmt::print(stderr, "verilator did not say where it is installed\n");
    return 1;
  }
  const lyra::test::VerilatorTool tool{
      .exe = verilator,
      .c_compiler = c_compiler,
      .standard_headers =
          std::filesystem::path(root_text) / "include" / "vltstd"};
  // What the verdicts below were reached under, so a verdict read later can be
  // told apart from one another tool, or another limit, would give.
  const std::vector<std::string> version_ask{"--version"};
  const lyra::test::ProcessOutcome version =
      lyra::test::RunChildProcess(verilator, version_ask);
  std::string_view version_text = version.stdout_text;
  while (version_text.ends_with('\n')) {
    version_text.remove_suffix(1);
  }
  fmt::print(
      "# {}; {} jobs; address limit {}; memory ceiling of the whole run {}\n",
      version_text, request->jobs,
      request->address_limit_mib
          ? fmt::format("{} MiB per process", *request->address_limit_mib)
          : std::string{"none"},
      MemoryCeilingOfTheRun());
  // Each case is built in a directory of its own, so the corpus is named from
  // the root rather than from wherever this was started.
  const std::vector<lyra::test::ConformanceCase> corpus =
      lyra::test::LoadConformanceCases(
          std::filesystem::absolute(
              runfiles->Rlocation("_main/tests/conformance")));
  std::vector<const lyra::test::ConformanceCase*> chosen;
  for (const lyra::test::ConformanceCase& c : corpus) {
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
      const lyra::test::ConformanceCase& c = *chosen[i];
      const auto here = lyra::test::MakeScratchDir();
      if (!here) {
        const std::scoped_lock lock{output};
        fmt::print(stderr, "{}: {}\n", c.id, here.error());
        harness_failed = true;
        continue;
      }
      const lyra::test::Observation seen =
          lyra::test::ObserveWithVerilator(tool, c, *here);
      const lyra::test::Verdict verdict = lyra::test::JudgeCase(c, seen);
      std::string text =
          fmt::format("{}: {}\n", c.id, lyra::test::Describe(verdict));
      if (!std::holds_alternative<lyra::test::Conforms>(verdict)) {
        text += fmt::format("{}\n", seen.evidence);
      }
      std::filesystem::remove_all(*here);
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
