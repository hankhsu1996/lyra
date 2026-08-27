#include "conformance_run.hpp"

#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "conformance_case.hpp"
#include "process.hpp"

namespace lyra::test {
namespace {

// How lyra reports the violation of an invariant it established itself. That
// is a fact about the command line, like the exit status and the two streams,
// so the harness reads it from the output rather than linking a compiler
// library to learn it.
constexpr std::string_view kInternalErrorReport = "lyra: internal error:";

auto TerminationName(TerminationKind kind) -> std::string_view {
  switch (kind) {
    case TerminationKind::kExitedNormally:
      return "exited 0";
    case TerminationKind::kExitedNonZero:
      return "exited non-zero";
    case TerminationKind::kSignaled:
      return "killed by a signal";
    case TerminationKind::kTimedOut:
      return "timed out";
    case TerminationKind::kSpawnFailed:
      return "could not be spawned";
    case TerminationKind::kWaitFailed:
      return "could not be waited on";
  }
  return "terminated in an unknown way";
}

auto Render(
    std::string_view what, const ConformanceCase& test_case,
    std::string_view path_name, const std::vector<std::string>& argv,
    const ProcessOutcome& outcome) -> std::string {
  std::string rendered;
  for (const std::string& arg : argv) {
    if (!rendered.empty()) {
      rendered += ' ';
    }
    rendered += arg;
  }
  return std::format(
      "{}\n\ncase: {}\npath: {}\nargv: {}\ntermination: {} ({})\n"
      "stdout:\n{}\nstderr:\n{}",
      what, test_case.id, path_name, rendered,
      TerminationName(outcome.termination), outcome.exit_code,
      outcome.stdout_text, outcome.stderr_text);
}

auto BuildArgv(const ConformancePath& path, const ConformanceCase& test_case)
    -> std::vector<std::string> {
  std::vector<std::string> argv;
  argv.emplace_back("run");
  for (const std::string& arg : path.selector) {
    argv.push_back(arg);
  }
  argv.emplace_back("--no-project");
  // A refusal is matched against recorded text, and read by whoever the match
  // failed for. Both want the diagnostic as it was written rather than wrapped
  // in the escape sequences that colour it for a terminal.
  argv.emplace_back("--no-color");
  if (path.caches_prelude) {
    if (const char* scratch = std::getenv("TEST_TMPDIR");
        scratch != nullptr && *scratch != '\0') {
      // Landing the prelude beside the shard's other scratch keeps one build of
      // it serving every case the shard runs, without reaching into a cache
      // shared with whatever else is running on the machine.
      argv.emplace_back("--pch-cache-dir");
      argv.push_back(std::string(scratch) + "/lyra-pch");
    }
  }
  for (const std::string& top : test_case.tops) {
    argv.emplace_back("--top");
    argv.push_back(top);
  }
  for (const std::string& arg : test_case.front_end_args) {
    argv.push_back(arg);
  }
  for (const std::filesystem::path& native : test_case.link_sources) {
    argv.emplace_back("--dpi-link");
    argv.push_back(native.string());
  }
  // A supporting source is what the case is written against, and a reference
  // reaches only the part of a compilation-unit scope declared before it
  // (LRM 3.12.1), so the sources beside the entry one are compiled ahead of it.
  // Otherwise the file holding the checks could not be the file that reads what
  // the others declare.
  for (const std::filesystem::path& supporting : test_case.supporting_sources) {
    argv.push_back(supporting.string());
  }
  argv.push_back(test_case.entry.string());
  if (!test_case.program_args.empty()) {
    argv.emplace_back("--");
    for (const std::string& arg : test_case.program_args) {
      argv.push_back(arg);
    }
  }
  return argv;
}

}  // namespace

auto FindConformancePath(std::string_view name)
    -> std::optional<ConformancePath> {
  if (name == "cpp") {
    return ConformancePath{
        .name = "cpp", .selector = {}, .caches_prelude = true};
  }
  if (name == "llvm") {
    return ConformancePath{
        .name = "llvm",
        .selector = {"--backend", "jit"},
        .caches_prelude = false};
  }
  return std::nullopt;
}

auto CaseRecord::Load(const std::filesystem::path& yaml) -> CaseRecord {
  CaseRecord record;
  if (!std::filesystem::exists(yaml)) {
    return record;
  }
  const YAML::Node root = YAML::LoadFile(yaml.string());
  // A path with nothing to record keeps a file holding only its own
  // explanation, which parses to nothing at all.
  if (root.IsNull()) {
    return record;
  }
  if (!root.IsMap()) {
    throw std::runtime_error(
        std::format(
            "{}: a record is a map of case id to text the run that case "
            "currently produces contains",
            yaml.string()));
  }
  for (const auto& entry : root) {
    record.entries_.emplace(
        entry.first.as<std::string>(), entry.second.as<std::string>());
  }
  return record;
}

auto CaseRecord::Find(std::string_view case_id) const
    -> std::optional<std::string> {
  const auto found = entries_.find(std::string(case_id));
  if (found == entries_.end()) {
    return std::nullopt;
  }
  return found->second;
}

auto CaseRecord::UnmatchedIds(const std::vector<ConformanceCase>& cases) const
    -> std::vector<std::string> {
  std::unordered_set<std::string> known;
  known.reserve(cases.size());
  for (const ConformanceCase& c : cases) {
    known.insert(c.id);
  }
  std::vector<std::string> unmatched;
  for (const auto& [id, text] : entries_) {
    if (!known.contains(id)) {
      unmatched.push_back(id);
    }
  }
  return unmatched;
}

auto CaseRecord::SharedIds(const CaseRecord& other) const
    -> std::vector<std::string> {
  std::vector<std::string> shared;
  for (const auto& [id, text] : entries_) {
    if (other.entries_.contains(id)) {
      shared.push_back(id);
    }
  }
  return shared;
}

auto CheckParkedCase(
    const std::filesystem::path& lyra_exe, const ConformanceCase& test_case)
    -> std::optional<std::string> {
  std::vector<std::string> argv{"check", "--no-project", "--no-color"};
  for (const std::string& top : test_case.tops) {
    argv.emplace_back("--top");
    argv.push_back(top);
  }
  for (const std::string& arg : test_case.front_end_args) {
    argv.push_back(arg);
  }
  for (const std::filesystem::path& supporting : test_case.supporting_sources) {
    argv.push_back(supporting.string());
  }
  argv.push_back(test_case.entry.string());

  const ProcessOutcome outcome =
      RunChildProcess(lyra_exe, argv, std::chrono::seconds{60});
  const auto report = [&](std::string_view what) {
    return Render(what, test_case, "elaboration only", argv, outcome);
  };

  // A case the standard requires a tool to reject is parked the same way any
  // other is, and elaborating one is supposed to fail.
  if (test_case.required_error.has_value()) {
    if (outcome.exit_code == 0) {
      return report(
          "the standard requires this program to be rejected, and it "
          "elaborated");
    }
    return std::nullopt;
  }
  if (outcome.termination != TerminationKind::kExitedNormally) {
    return report("this parked case no longer elaborates");
  }
  return std::nullopt;
}

auto RunConformanceCase(
    const std::filesystem::path& lyra_exe, const ConformancePath& path,
    const ConformanceCase& test_case, const PathRecords& records)
    -> std::optional<std::string> {
  const std::vector<std::string> argv = BuildArgv(path, test_case);
  const ProcessOutcome outcome =
      RunChildProcess(lyra_exe, argv, std::chrono::seconds{60});
  const auto report = [&](std::string_view what) {
    return Render(what, test_case, path.name, argv, outcome);
  };

  if (outcome.termination != TerminationKind::kExitedNormally &&
      outcome.termination != TerminationKind::kExitedNonZero) {
    return report("the run did not reach an exit status of its own");
  }

  const bool succeeded = outcome.exit_code == 0;
  const bool checked =
      outcome.stdout_text.find(kAllChecksPassed) != std::string::npos;
  const auto mentions = [&](std::string_view text) {
    return outcome.stderr_text.find(text) != std::string::npos;
  };

  // A compiler bug is not a limitation, so it is neither a refusal a path may
  // be recorded as producing nor the rejection a case requires. Were it either,
  // "this is not implemented yet" and "this is implemented wrongly" would be
  // recorded the same way, and the record would stop being a coverage report.
  if (mentions(kInternalErrorReport)) {
    return report("the run reported a compiler bug");
  }

  // A program IEEE 1800 requires a tool to reject makes no checks, so what it
  // is held to is the rejection and the reason given for it.
  if (test_case.required_error.has_value()) {
    if (succeeded) {
      return report(
          "the standard requires this program to be rejected, and it was "
          "accepted");
    }
    if (!mentions(*test_case.required_error)) {
      return report(
          std::format(
              "the rejection does not mention '{}'",
              *test_case.required_error));
    }
    return std::nullopt;
  }

  if (const std::optional<std::string> refusal =
          records.refusals.Find(test_case.id)) {
    if (succeeded && checked) {
      return report(
          "this path runs the case now; drop its entry from the path's refusal "
          "record so the coverage it just gained is held from here on");
    }
    if (succeeded) {
      return report(
          "the path neither refused the case nor ran its checks to the end");
    }
    if (!mentions(*refusal)) {
      return report(
          std::format(
              "the path refuses this case for a reason other than the recorded "
              "'{}'",
              *refusal));
    }
    return std::nullopt;
  }

  // A recorded defect is a wrong answer someone diagnosed and left standing, so
  // what the run is held to is producing that same wrong answer. The case keeps
  // every check it makes, which is what lets the record notice the day the
  // answer becomes right -- a check commented out instead would leave nothing
  // to notice it.
  if (const std::optional<std::string> defect =
          records.defects.Find(test_case.id)) {
    if (succeeded && checked) {
      return report(
          "this path answers this case correctly now; drop its entry from the "
          "path's defect record so the behaviour is held from here on");
    }
    const bool says_so =
        outcome.stdout_text.find(*defect) != std::string::npos ||
        mentions(*defect);
    if (!says_so) {
      return report(
          std::format(
              "this case fails for a reason other than the recorded defect "
              "'{}'",
              *defect));
    }
    return std::nullopt;
  }

  if (!succeeded) {
    return report("a check in this case failed, or the path could not run it");
  }
  if (!checked) {
    return report(
        std::format(
            "the program ran to completion without printing '{}', so its "
            "checks did not all run",
            kAllChecksPassed));
  }
  return std::nullopt;
}

}  // namespace lyra::test
