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

namespace lyra::test {

struct CaseInput {
  std::vector<std::string> command;
  bool no_project = true;
  std::optional<std::string> top;
  std::vector<std::string> files;
  std::vector<std::string> extra_args;
};

struct CaseExpect {
  std::optional<int> exit_code;
  ExpectedOutput stdout_spec;
  ExpectedOutput stderr_spec;
};

struct TestCase {
  std::string id;
  std::vector<std::string> tags;
  std::filesystem::path case_dir;
  std::filesystem::path case_yaml_path;
  CaseInput input;
  CaseExpect expect;
};

struct Suite {
  std::string name;
  std::vector<std::string> include_tags;
  std::unordered_set<std::string> excluded_ids;
};

struct RunResult {
  ProcessOutcome proc;
  std::vector<std::string> argv;
  std::optional<std::string> mismatch;
};

// Paths needed to build and run an emitted C++ artifact. Used by the
// `[run, cpp]` command path in RunCase.
struct CppRunPaths {
  std::filesystem::path include_root;
  std::vector<std::filesystem::path> runtime_src_dirs;
};

auto LoadCases(const std::filesystem::path& cases_root)
    -> std::vector<TestCase>;

auto LoadSuite(const std::filesystem::path& suites_yaml, std::string_view name)
    -> Suite;

auto FilterCases(const std::vector<TestCase>& cases, const Suite& suite)
    -> std::vector<TestCase>;

auto RunCase(
    const std::filesystem::path& lyra_exe, const TestCase& c,
    const CppRunPaths& cpp_paths) -> RunResult;

auto FormatCaseFailure(
    std::string_view case_id, std::span<const std::string> argv,
    const ProcessOutcome& outcome) -> std::string;

}  // namespace lyra::test
