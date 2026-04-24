#include "runner.hpp"

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <filesystem>
#include <format>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "matcher.hpp"
#include "process.hpp"

namespace lyra::test {
namespace {

auto ParseExpected(const YAML::Node& node) -> ExpectedOutput {
  ExpectedOutput out;
  if (!node || !node.IsMap()) {
    return out;
  }
  if (node["exact"]) {
    out.exact = node["exact"].as<std::string>();
  }
  if (node["contains"]) {
    for (const auto& item : node["contains"]) {
      out.contains.push_back(item.as<std::string>());
    }
  }
  if (node["not_contains"]) {
    for (const auto& item : node["not_contains"]) {
      out.not_contains.push_back(item.as<std::string>());
    }
  }
  return out;
}

auto ParseCase(
    const std::filesystem::path& case_dir,
    const std::filesystem::path& yaml_path) -> TestCase {
  YAML::Node root = YAML::LoadFile(yaml_path.string());

  TestCase c;
  c.case_dir = case_dir;

  if (!root["id"]) {
    throw std::runtime_error(
        std::format("{}: missing 'id'", yaml_path.string()));
  }
  c.id = root["id"].as<std::string>();

  if (root["tags"]) {
    for (const auto& t : root["tags"]) {
      c.tags.push_back(t.as<std::string>());
    }
  }

  const auto& in = root["input"];
  if (!in || !in.IsMap()) {
    throw std::runtime_error(
        std::format("{}: missing 'input' map", yaml_path.string()));
  }

  if (!in["command"] || !in["command"].IsSequence() ||
      in["command"].size() == 0) {
    throw std::runtime_error(
        std::format(
            "{}: 'input.command' must be a non-empty list",
            yaml_path.string()));
  }
  for (const auto& tok : in["command"]) {
    c.input.command.push_back(tok.as<std::string>());
  }

  if (!in["project"]) {
    throw std::runtime_error(
        std::format(
            "{}: 'input.project' is required (true/false)",
            yaml_path.string()));
  }
  c.input.no_project = !in["project"].as<bool>();

  if (in["top"]) {
    c.input.top = in["top"].as<std::string>();
  }
  if (in["files"]) {
    for (const auto& f : in["files"]) {
      const auto rel = f.as<std::string>();
      const auto abs = case_dir / rel;
      if (!std::filesystem::exists(abs)) {
        throw std::runtime_error(
            std::format(
                "{}: referenced file '{}' does not exist at '{}'",
                yaml_path.string(), rel, abs.string()));
      }
      c.input.files.push_back(rel);
    }
  }
  if (in["args"]) {
    for (const auto& a : in["args"]) {
      c.input.extra_args.push_back(a.as<std::string>());
    }
  }

  const auto& ex = root["expect"];
  if (ex && ex.IsMap()) {
    if (ex["exit"]) {
      c.expect.exit_code = ex["exit"].as<int>();
    }
    c.expect.stdout_spec = ParseExpected(ex["stdout"]);
    c.expect.stderr_spec = ParseExpected(ex["stderr"]);
  }

  return c;
}

auto TerminationName(TerminationKind k) -> std::string_view {
  switch (k) {
    case TerminationKind::kExitedNormally:
      return "ExitedNormally";
    case TerminationKind::kExitedNonZero:
      return "ExitedNonZero";
    case TerminationKind::kSignaled:
      return "Signaled";
    case TerminationKind::kTimedOut:
      return "TimedOut";
    case TerminationKind::kSpawnFailed:
      return "SpawnFailed";
    case TerminationKind::kWaitFailed:
      return "WaitFailed";
  }
  return "Unknown";
}

auto CaseHasTag(const TestCase& c, std::string_view tag) -> bool {
  return std::ranges::find(c.tags, tag) != c.tags.end();
}

auto MatchesIncludeTags(const TestCase& c, const Suite& suite) -> bool {
  if (suite.include_tags.empty()) {
    return true;
  }
  return std::ranges::any_of(
      suite.include_tags, [&](const auto& tag) { return CaseHasTag(c, tag); });
}

}  // namespace

auto LoadCases(const std::filesystem::path& cases_root)
    -> std::vector<TestCase> {
  std::vector<TestCase> out;
  if (!std::filesystem::exists(cases_root)) {
    return out;
  }
  for (const auto& entry :
       std::filesystem::recursive_directory_iterator(cases_root)) {
    if (!entry.is_regular_file()) {
      continue;
    }
    if (entry.path().filename() != "case.yaml") {
      continue;
    }
    out.push_back(ParseCase(entry.path().parent_path(), entry.path()));
  }
  std::ranges::sort(out, {}, &TestCase::id);

  for (std::size_t i = 1; i < out.size(); ++i) {
    if (out[i].id == out[i - 1].id) {
      throw std::runtime_error(
          std::format(
              "duplicate case id '{}' (found in '{}' and '{}')", out[i].id,
              out[i - 1].case_dir.string(), out[i].case_dir.string()));
    }
  }
  return out;
}

auto LoadSuite(const std::filesystem::path& suites_yaml, std::string_view name)
    -> Suite {
  YAML::Node root = YAML::LoadFile(suites_yaml.string());
  const auto& suites = root["suites"];
  if (!suites || !suites.IsMap()) {
    throw std::runtime_error(
        std::format("{}: missing 'suites' map", suites_yaml.string()));
  }
  const auto& node = suites[std::string(name)];
  if (!node || !node.IsMap()) {
    throw std::runtime_error(
        std::format("{}: suite '{}' not found", suites_yaml.string(), name));
  }

  Suite s;
  s.name = std::string(name);
  if (node["include_tags"]) {
    for (const auto& t : node["include_tags"]) {
      s.include_tags.push_back(t.as<std::string>());
    }
  }
  if (node["exclude"]) {
    for (const auto& item : node["exclude"]) {
      if (item.IsMap() && item["id"]) {
        s.excluded_ids.insert(item["id"].as<std::string>());
      }
    }
  }
  return s;
}

auto FilterCases(const std::vector<TestCase>& cases, const Suite& suite)
    -> std::vector<TestCase> {
  std::unordered_set<std::string> known_ids;
  known_ids.reserve(cases.size());
  for (const auto& c : cases) {
    known_ids.insert(c.id);
  }

  std::unordered_set<std::string> seen_excluded;
  for (const auto& id : suite.excluded_ids) {
    if (!known_ids.contains(id)) {
      throw std::runtime_error(
          std::format(
              "suite '{}': excluded id '{}' does not match any loaded case",
              suite.name, id));
    }
  }

  std::vector<TestCase> out;
  for (const auto& c : cases) {
    if (!MatchesIncludeTags(c, suite)) {
      continue;
    }
    if (suite.excluded_ids.contains(c.id)) {
      seen_excluded.insert(c.id);
      continue;
    }
    out.push_back(c);
  }

  for (const auto& id : suite.excluded_ids) {
    if (!seen_excluded.contains(id)) {
      throw std::runtime_error(
          std::format(
              "suite '{}': excluded id '{}' is never selected by include_tags "
              "(the exclude is redundant)",
              suite.name, id));
    }
  }
  return out;
}

auto RunCase(const std::filesystem::path& lyra_exe, const TestCase& c)
    -> RunResult {
  RunResult result;

  std::vector<std::string>& argv = result.argv;
  for (const auto& t : c.input.command) {
    argv.push_back(t);
  }
  if (c.input.no_project) {
    argv.emplace_back("--no-project");
  }
  if (c.input.top.has_value()) {
    argv.emplace_back("--top");
    argv.push_back(*c.input.top);
  }
  for (const auto& a : c.input.extra_args) {
    argv.push_back(a);
  }
  for (const auto& f : c.input.files) {
    argv.push_back((c.case_dir / f).string());
  }

  result.proc = RunChildProcess(lyra_exe, argv, std::chrono::seconds{30});

  auto record = [&](std::string header, std::string body) {
    std::string prefix =
        FormatCaseFailure(c.id, argv, result.proc) + "\n" + header + ": ";
    result.mismatch = prefix + body;
  };

  if (c.expect.exit_code.has_value()) {
    int actual_exit = result.proc.exit_code;
    if (result.proc.termination == TerminationKind::kSignaled) {
      actual_exit = 128 + result.proc.signal_number;
    }
    if (actual_exit != *c.expect.exit_code) {
      record(
          "exit mismatch",
          std::format("expected {}, got {}", *c.expect.exit_code, actual_exit));
      return result;
    }
  }

  if (auto mm = CheckOutput(
          result.proc.stdout_text, c.expect.stdout_spec, "stdout")) {
    record("stdout mismatch", *mm);
    return result;
  }
  if (auto mm = CheckOutput(
          result.proc.stderr_text, c.expect.stderr_spec, "stderr")) {
    record("stderr mismatch", *mm);
    return result;
  }
  return result;
}

auto FormatCaseFailure(
    std::string_view case_id, std::span<const std::string> argv,
    const ProcessOutcome& outcome) -> std::string {
  std::string rendered_argv;
  for (std::size_t i = 0; i < argv.size(); ++i) {
    if (i != 0) {
      rendered_argv += ' ';
    }
    rendered_argv += argv[i];
  }
  std::string term = std::format(
      "{} (exit={}", TerminationName(outcome.termination), outcome.exit_code);
  if (outcome.termination == TerminationKind::kSignaled) {
    term += std::format(", signal={}", outcome.signal_number);
  }
  term += ')';
  return std::format(
      "case: {}\nargv: {}\ntermination: {}\nstdout:\n{}\nstderr:\n{}", case_id,
      rendered_argv, term, outcome.stdout_text, outcome.stderr_text);
}

}  // namespace lyra::test
