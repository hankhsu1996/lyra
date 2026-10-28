// Holds Verilator to the corpus: every case named on the command line, or the
// whole corpus where none is, judged by the same claims Lyra is held to, with
// the verdict and the evidence for it printed per case. Nothing here is a gate.
// Verilator is a second tool the corpus is run against to learn whether a case
// says only what the standard requires, and what it answers is read case by
// case rather than recorded.

#include <algorithm>
#include <cstddef>
#include <cstdlib>
#include <filesystem>
#include <memory>
#include <span>
#include <string>
#include <string_view>
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

}  // namespace

auto main(int argc, char** argv) -> int {
  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
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
  // Each case is built in a directory of its own, so the corpus is named from
  // the root rather than from wherever this was started.
  const std::vector<lyra::test::ConformanceCase> corpus =
      lyra::test::LoadConformanceCases(
          std::filesystem::absolute(
              runfiles->Rlocation("_main/tests/conformance")));
  std::vector<std::string_view> asked(args.begin() + 1, args.end());

  int judged = 0;
  for (const lyra::test::ConformanceCase& c : corpus) {
    if (!asked.empty() && std::ranges::find(asked, c.id) == asked.end()) {
      continue;
    }
    ++judged;
    const auto here = lyra::test::MakeScratchDir();
    if (!here) {
      fmt::print(stderr, "{}: {}\n", c.id, here.error());
      return 1;
    }
    const lyra::test::Observation seen =
        lyra::test::ObserveWithVerilator(tool, c, *here);
    const lyra::test::Verdict verdict = lyra::test::Judge(c.claim, seen);
    fmt::print("{}: {}\n", c.id, lyra::test::Describe(verdict));
    if (!std::holds_alternative<lyra::test::Conforms>(verdict)) {
      fmt::print("{}\n", seen.evidence);
    }
    std::filesystem::remove_all(*here);
  }
  if (judged == 0) {
    fmt::print(stderr, "no case of the corpus was named\n");
    return 1;
  }
  return 0;
}
