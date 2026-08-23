#pragma once

#include <filesystem>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

#include "matcher.hpp"
#include "process.hpp"
#include "sv_literal.hpp"

namespace lyra::test {

struct CaseInput {
  std::vector<std::string> command;
  bool no_project = true;
  std::optional<std::string> top;
  std::vector<std::string> files;
  std::vector<std::string> extra_args;
  // Arguments for the simulated program rather than for Lyra. Passed after the
  // `--` that ends Lyra's own command line, which is where LRM 21.6 plusargs
  // reach a design.
  std::vector<std::string> program_args;
  // Native sources (.c / .cpp) providing DPI-C foreign symbols, passed to the
  // run via `--dpi-link` and linked into the built program (LRM 35).
  std::vector<std::string> link_sources;
  // Which backend runs the simulation, taken from the suite that selected this
  // case. Unset runs the default.
  std::optional<std::string> backend;
};

struct ExpectedVariable {
  std::string name;
  ExpectedValue value;
};

struct ExpectedFile {
  std::string relative_path;
  std::string content;
};

struct CaseExpect {
  std::optional<int> exit_code;
  ExpectedOutput stdout_spec;
  ExpectedOutput stderr_spec;
  std::vector<ExpectedVariable> variables;
  std::vector<ExpectedFile> files;
};

struct TestCase {
  std::string id;
  std::vector<std::string> tags;
  std::filesystem::path case_dir;
  std::filesystem::path case_yaml_path;
  CaseInput input;
  CaseExpect expect;
};

// A named selection of cases plus the backend they run on. The backend is the
// suite's because it is a property of the run, not of the source: the same case
// is written once and every backend that claims it -- by the tag the suite
// includes -- must produce what the case says.
struct Suite {
  std::string name;
  std::vector<std::string> include_tags;
  std::unordered_set<std::string> excluded_ids;
  std::optional<std::string> backend;
};

struct RunResult {
  ProcessOutcome proc;
  std::vector<std::string> argv;
  std::optional<std::string> mismatch;
};

auto LoadCases(const std::filesystem::path& cases_root)
    -> std::vector<TestCase>;

auto LoadSuite(const std::filesystem::path& suites_yaml, std::string_view name)
    -> Suite;

auto FilterCases(const std::vector<TestCase>& cases, const Suite& suite)
    -> std::vector<TestCase>;

auto RunCase(const std::filesystem::path& lyra_exe, const TestCase& c)
    -> RunResult;

auto FormatCaseFailure(
    std::string_view case_id, std::span<const std::string> argv,
    const ProcessOutcome& outcome) -> std::string;

}  // namespace lyra::test
