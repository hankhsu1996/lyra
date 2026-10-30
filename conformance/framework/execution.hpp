#pragma once

#include <chrono>
#include <filesystem>
#include <optional>
#include <string>
#include <vector>

#include "conformance/framework/case.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/verdict.hpp"

namespace conformance::framework {

// A program a tool's reader runs, where it was found, and the version it says
// it is, or nothing where it says none.
struct Program {
  std::string role;
  std::filesystem::path path;
  std::optional<std::string> version;
};

// What a tool is, as its reader resolved it: the programs it runs -- the tool
// itself first, then any it leans on, such as a host C compiler -- the options
// every case's build is given, and the environment variables the tool is known
// to read, by name. A verdict means something only beside these, since another
// version, option or setting may answer otherwise.
struct ToolIdentity {
  std::string name;
  std::vector<Program> programs;
  std::vector<std::string> options;
  std::vector<std::string> environment_read;
};

// A tool a case is handed to: how it builds and runs the case's program in a
// directory, and how what it wrote is read. Each tool's reader is one, and a
// caller that checks more of a tool than the standard asks wraps one.
class Reader {
 public:
  Reader() = default;
  Reader(const Reader&) = delete;
  Reader(Reader&&) = delete;
  auto operator=(const Reader&) -> Reader& = delete;
  auto operator=(Reader&&) -> Reader& = delete;
  virtual ~Reader() = default;

  // What the tool this reader runs is, as a run states it beside every
  // verdict.
  [[nodiscard]] virtual auto Identity() const -> ToolIdentity = 0;

  // Builds and runs `test_case` in `directory`, which is the reader's alone
  // for this case, and reads what the tool did. Every command it starts is
  // stopped once `deadline` has passed, where one is given, so the case as a
  // whole ends then however many commands it takes, give or take the moment a
  // stopped command is allowed to end in.
  [[nodiscard]] virtual auto Observe(
      const Case& test_case, const std::filesystem::path& directory,
      std::optional<std::chrono::steady_clock::time_point> deadline) const
      -> Observation = 0;
};

// What one case's run came to: what the tool was seen to do, the verdict the
// standard gives it, and how long it took on the machine that ran it.
struct CaseResult {
  Observation seen;
  Verdict verdict;
  std::chrono::duration<double> took;
};

// Runs one case on one tool and judges it. A program opens files where it
// starts, under names another case may use too, and a caller may run several
// cases at once, so each case runs in a directory of its own, removed once the
// run has been read. A directory that could not be made is the harness failing
// to hand the case to the tool, as a missing source is, so the case is not run.
// The case is given `time_limit` in all, where one is given.
auto RunOneCase(
    const Case& test_case, const Reader& reader,
    std::optional<std::chrono::seconds> time_limit) -> CaseResult;

}  // namespace conformance::framework
