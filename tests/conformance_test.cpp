#include <algorithm>
#include <atomic>
#include <cctype>
#include <charconv>
#include <cstddef>
#include <filesystem>
#include <future>
#include <gtest/gtest.h>
#include <memory>
#include <optional>
#include <span>
#include <stop_token>
#include <string>
#include <string_view>
#include <thread>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "tests/framework/conformance_case.hpp"
#include "tests/framework/conformance_run.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;
using lyra::test::CaseRecord;
using lyra::test::CheckParkedCase;
using lyra::test::ConformanceCase;
using lyra::test::ConformancePath;
using lyra::test::FindConformancePath;
using lyra::test::LoadConformanceCases;
using lyra::test::LoadParkedCases;
using lyra::test::PathRecords;
using lyra::test::RunConformanceCase;

namespace {

// The host compiler this ran under. A case can pass under one and fail under
// another -- a path that builds C++ hands the design to whichever compiler is
// installed -- so a log that does not say which one ran leaves the reader
// guessing at the difference between two machines.
auto HostCompiler() -> std::string {
#if defined(__clang_version__)
  return std::string("clang ") + __clang_version__;
#elif defined(__VERSION__)
  return std::string("gcc ") + __VERSION__;
#else
  return "an unknown compiler";
#endif
}

struct Corpus {
  std::filesystem::path lyra_exe;
  std::filesystem::path cases_root;
  std::filesystem::path paths_root;
};

// The two names a test filter takes, which a case id already is: the clause it
// tests, and the subject within that clause. A target runs one path, so the
// path is the target's to name and no part of these.
struct FilterName {
  std::string group;
  std::string name;
};

// A case id reads as a file path, which a test filter cannot take: `/` has no
// spelling in one. Substituting the separator is the whole difference between
// the two, and the corpus admits no other character that would need changing,
// so a name and a path convert to each other by inspection.
auto SplitCaseId(std::string_view id) -> FilterName {
  const std::size_t clause_end = id.find('/');
  std::string name(id.substr(clause_end + 1));
  std::ranges::replace(name, '/', '.');
  return FilterName{
      .group = std::string(id.substr(0, clause_end)), .name = std::move(name)};
}

// The cases this run reports, run by several workers at once ahead of the test
// that reports each. A case is a program built and run in a process of its own
// and judged by what that process left, so nothing a test body does can change
// its verdict, and starting it early only takes the wait off whatever the
// machine is not already doing. gtest has chosen the run's cases -- by filter
// and by shard -- before an environment is set up, so these are exactly the
// cases it will report, taken in the order gtest lists them.
class CaseRuns : public testing::Environment {
 public:
  CaseRuns(
      std::size_t workers, const std::filesystem::path* lyra_exe,
      const ConformancePath* path, const PathRecords* records)
      : workers_(workers), lyra_exe_(lyra_exe), path_(path), records_(records) {
  }

  // Names the case a registered test reports, so the run can find the cases
  // gtest chose by the tests it chose.
  void Register(std::string test_name, const ConformanceCase& c) {
    by_test_name_.emplace(std::move(test_name), &c);
  }

  // A repetition of the run runs every case again, so each starts over.
  void SetUp() override {
    chosen_.clear();
    verdicts_.clear();
    verdict_of_.clear();
    next_ = 0;
    const testing::UnitTest& unit = *testing::UnitTest::GetInstance();
    for (int i = 0; i < unit.total_test_suite_count(); ++i) {
      const testing::TestSuite& suite = *unit.GetTestSuite(i);
      for (int j = 0; j < suite.total_test_count(); ++j) {
        const testing::TestInfo& test = *suite.GetTestInfo(j);
        const auto found = by_test_name_.find(
            std::string(test.test_suite_name()) + "." + test.name());
        if (test.should_run() && found != by_test_name_.end()) {
          chosen_.push_back(found->second);
        }
      }
    }
    verdicts_.resize(chosen_.size());
    for (std::size_t i = 0; i < chosen_.size(); ++i) {
      verdict_of_.emplace(chosen_[i], verdicts_[i].get_future().share());
    }
    for (std::size_t w = 0; w < workers_; ++w) {
      running_.emplace_back(
          [this](const std::stop_token& stop) { Work(stop); });
    }
  }

  // A run that stops at its first failure reports nothing after it, so the
  // workers start no case beyond those already running.
  void TearDown() override {
    for (std::jthread& worker : running_) {
      worker.request_stop();
    }
    running_.clear();
  }

  // What the case's run is judged to have done, once it has ended.
  auto Verdict(const ConformanceCase& c) const -> std::optional<std::string> {
    return verdict_of_.at(&c).get();
  }

 private:
  void Work(const std::stop_token& stop) {
    while (!stop.stop_requested()) {
      const std::size_t i = next_.fetch_add(1);
      if (i >= chosen_.size()) {
        return;
      }
      verdicts_[i].set_value(
          RunConformanceCase(*lyra_exe_, *path_, *chosen_[i], *records_));
    }
  }

  std::size_t workers_;
  const std::filesystem::path* lyra_exe_;
  const ConformancePath* path_;
  const PathRecords* records_;
  std::unordered_map<std::string, const ConformanceCase*> by_test_name_;
  std::vector<const ConformanceCase*> chosen_;
  std::vector<std::promise<std::optional<std::string>>> verdicts_;
  std::unordered_map<
      const ConformanceCase*, std::shared_future<std::optional<std::string>>>
      verdict_of_;
  std::atomic<std::size_t> next_{0};
  std::vector<std::jthread> running_;
};

class ConformanceTest : public testing::Test {
 public:
  ConformanceTest(const ConformanceCase& c, const CaseRuns* runs)
      : case_(&c), runs_(runs) {
  }

 protected:
  void TestBody() override {
    if (auto failure = runs_->Verdict(*case_)) {
      ADD_FAILURE() << *failure;
    }
  }

 private:
  const ConformanceCase* case_;
  const CaseRuns* runs_;
};

class ParkedCaseTest : public testing::Test {
 public:
  ParkedCaseTest(
      const ConformanceCase& c, const std::filesystem::path* lyra_exe)
      : case_(&c), lyra_exe_(lyra_exe) {
  }

 protected:
  void TestBody() override {
    if (auto failure = CheckParkedCase(*lyra_exe_, *case_)) {
      ADD_FAILURE() << *failure;
    }
  }

 private:
  const ConformanceCase* case_;
  const std::filesystem::path* lyra_exe_;
};

}  // namespace

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);

  std::string error;
  std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&error)};
  if (!runfiles) {
    fmt::print(stderr, "failed to create runfiles: {}\n", error);
    return 1;
  }
  static const Corpus kCorpus{
      .lyra_exe = runfiles->Rlocation("_main/lyra"),
      .cases_root = runfiles->Rlocation("_main/tests/conformance"),
      .paths_root = runfiles->Rlocation("_main/tests/paths")};

  // The path to run, and what each program it builds is checked for while it
  // runs: a build of the same path, so the path's records still hold, and a
  // report of a read of ended storage fails the case that made it.
  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
  std::string requested;
  std::size_t workers = 1;
  std::optional<std::string> sanitizer;
  for (std::size_t i = 1; i + 1 < args.size(); ++i) {
    const std::string_view arg = args[i];
    const std::string_view value = args[i + 1];
    if (arg == "--path") {
      requested = value;
    }
    if (arg == "--workers") {
      const auto [end, error] =
          std::from_chars(value.data(), value.data() + value.size(), workers);
      if (error != std::errc{} || end != value.data() + value.size() ||
          workers == 0) {
        fmt::print(stderr, "--workers takes a count of one or more\n");
        return 1;
      }
    }
    if (arg == "--sanitize") {
      sanitizer = value;
    }
  }
  if (requested.empty()) {
    fmt::print(stderr, "no path selected: pass --path <name>\n");
    return 1;
  }
  const auto found = FindConformancePath(requested);
  if (!found) {
    fmt::print(stderr, "'{}' is not a path this corpus runs on\n", requested);
    return 1;
  }
  static const ConformancePath kPath = [&] {
    ConformancePath path = *found;
    if (sanitizer) {
      path.selector.insert(path.selector.end(), {"--sanitize", *sanitizer});
    }
    return path;
  }();

  static const std::vector<ConformanceCase> kCases =
      LoadConformanceCases(kCorpus.cases_root);
  if (kCases.empty()) {
    fmt::print(
        stderr,
        "the corpus holds no cases, so passing would report coverage that was "
        "never measured\n");
    return 1;
  }

  const CaseRecord refusals =
      CaseRecord::Load(kCorpus.paths_root / (kPath.name + ".yaml"));
  const CaseRecord defects =
      CaseRecord::Load(kCorpus.paths_root / (kPath.name + ".defects.yaml"));
  // What the sanitizer finds that the path's own run cannot see, which is
  // recorded for the checked run alone: the same case passes unchecked, and
  // that record would fail it.
  const CaseRecord found_by_sanitizer =
      sanitizer ? CaseRecord::Load(
                      kCorpus.paths_root /
                      fmt::format("{}.{}.defects.yaml", kPath.name, *sanitizer))
                : CaseRecord{};

  bool records_hold = true;
  for (const auto& [record, kind] :
       {std::pair{&refusals, "refusal"}, std::pair{&defects, "defect"},
        std::pair{&found_by_sanitizer, "sanitizer finding"}}) {
    for (const std::string& id : record->UnmatchedIds(kCases)) {
      fmt::print(
          stderr, "the '{}' path records a {} for '{}', which is not a case\n",
          kPath.name, kind, id);
      records_hold = false;
    }
  }
  // A case is held to one outcome; recorded twice, there would be no saying
  // which one the run is held to.
  for (const auto& [first, second, kinds] :
       {std::tuple{&refusals, &defects, "a refusal and a defect"},
        std::tuple{
            &refusals, &found_by_sanitizer,
            "a refusal and a sanitizer finding"},
        std::tuple{
            &defects, &found_by_sanitizer,
            "a defect and a sanitizer finding"}}) {
    for (const std::string& id : first->SharedIds(*second)) {
      fmt::print(
          stderr, "the '{}' path records '{}' as both {}\n", kPath.name, id,
          kinds);
      records_hold = false;
    }
  }
  if (!records_hold) {
    return 1;
  }
  static const PathRecords kRecords{
      .refusals = refusals, .defects = defects.Joined(found_by_sanitizer)};

  fmt::print(
      "{} cases on the {} path, built with {}\n", kCases.size(), kPath.name,
      HostCompiler());

  // gtest owns the environment and every test it is handed, and keeps them to
  // the end of the run.
  auto owned_runs =
      std::make_unique<CaseRuns>(workers, &kCorpus.lyra_exe, &kPath, &kRecords);
  CaseRuns* const runs = owned_runs.get();
  testing::AddGlobalTestEnvironment(owned_runs.release());

  for (const ConformanceCase& c : kCases) {
    const FilterName filter = SplitCaseId(c.id);
    runs->Register(filter.group + "." + filter.name, c);
    testing::RegisterTest(
        filter.group.c_str(), filter.name.c_str(), nullptr, nullptr, __FILE__,
        __LINE__, [&c, runs]() -> testing::Test* {
          return std::make_unique<ConformanceTest>(c, runs).release();
        });
  }

  // Parked cases are held only to elaborating, which no path decides, so every
  // target holds them to the same thing rather than one being picked to.
  static const std::vector<ConformanceCase> kParked =
      LoadParkedCases(kCorpus.cases_root);
  // A parked case answers to a group of its own, since a target holds both
  // kinds and only the name says which of the two a result came from.
  for (const ConformanceCase& c : kParked) {
    const FilterName filter = SplitCaseId(c.id);
    const std::string name = filter.group + "." + filter.name;
    testing::RegisterTest(
        "Parked", name.c_str(), nullptr, nullptr, __FILE__, __LINE__,
        [&c]() -> testing::Test* {
          return std::make_unique<ParkedCaseTest>(c, &kCorpus.lyra_exe)
              .release();
        });
  }

  // A repetition is asked for to run every case again, and the environment is
  // what runs them, so it is set up again for each.
  GTEST_FLAG_SET(recreate_environments_when_repeating, true);
  return RUN_ALL_TESTS();
}
