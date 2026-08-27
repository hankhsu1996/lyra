#include <algorithm>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <gtest/gtest.h>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
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

// Which of the corpus a target runs. The corpus divides here on what a case
// needs to run rather than on what it claims, because a case carrying foreign
// sources builds them with the host C compiler whatever path it is on -- the
// design's own translation is what a path changes, not how a foreign symbol is
// produced. Splitting on that keeps a handful of such cases from holding the
// whole corpus to a machine that has a C compiler.
enum class Foreign : std::uint8_t { kEither, kOnly, kExclude };

auto Selects(Foreign selection, const ConformanceCase& c) -> bool {
  switch (selection) {
    case Foreign::kEither:
      return true;
    case Foreign::kOnly:
      return !c.link_sources.empty();
    case Foreign::kExclude:
      return c.link_sources.empty();
  }
  return true;
}

auto FindForeign(std::string_view name) -> std::optional<Foreign> {
  if (name == "only") {
    return Foreign::kOnly;
  }
  if (name == "exclude") {
    return Foreign::kExclude;
  }
  return std::nullopt;
}

// A case id reads as a file path, which a test filter cannot take: `/` has no
// spelling in one. Substituting the separator is the whole difference between
// the two, and the corpus admits no other character that would need changing,
// so a name and a path convert to each other by inspection.
auto TestName(std::string_view id) -> std::string {
  std::string name(id);
  std::ranges::replace(name, '/', '.');
  return name;
}

auto GroupName(std::string_view path_name) -> std::string {
  std::string group(path_name);
  group[0] =
      static_cast<char>(std::toupper(static_cast<unsigned char>(group[0])));
  return group;
}

class ConformanceTest : public testing::Test {
 public:
  ConformanceTest(
      const ConformanceCase& c, const std::filesystem::path* lyra_exe,
      const ConformancePath* path, const PathRecords* records)
      : case_(&c), lyra_exe_(lyra_exe), path_(path), records_(records) {
  }

 protected:
  void TestBody() override {
    if (auto failure =
            RunConformanceCase(*lyra_exe_, *path_, *case_, *records_)) {
      ADD_FAILURE() << *failure;
    }
  }

 private:
  const ConformanceCase* case_;
  const std::filesystem::path* lyra_exe_;
  const ConformancePath* path_;
  const PathRecords* records_;
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

  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
  std::string requested;
  Foreign foreign = Foreign::kEither;
  for (std::size_t i = 1; i + 1 < args.size(); ++i) {
    const std::string_view flag(args[i]);
    if (flag == "--path") {
      requested = args[i + 1];
    }
    if (flag == "--foreign") {
      const auto selection = FindForeign(args[i + 1]);
      if (!selection) {
        fmt::print(
            stderr, "--foreign takes 'only' or 'exclude', not '{}'\n",
            args[i + 1]);
        return 1;
      }
      foreign = *selection;
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
  static const ConformancePath kPath = *found;

  static const std::vector<ConformanceCase> kCases =
      LoadConformanceCases(kCorpus.cases_root);
  if (kCases.empty()) {
    fmt::print(
        stderr,
        "the corpus holds no cases, so passing would report coverage that was "
        "never measured\n");
    return 1;
  }

  static const PathRecords kRecords{
      .refusals = CaseRecord::Load(kCorpus.paths_root / (kPath.name + ".yaml")),
      .defects = CaseRecord::Load(
          kCorpus.paths_root / (kPath.name + ".defects.yaml"))};

  bool records_hold = true;
  for (const auto& [record, kind] :
       {std::pair{&kRecords.refusals, "refusal"},
        std::pair{&kRecords.defects, "defect"}}) {
    for (const std::string& id : record->UnmatchedIds(kCases)) {
      fmt::print(
          stderr, "the '{}' path records a {} for '{}', which is not a case\n",
          kPath.name, kind, id);
      records_hold = false;
    }
  }
  // A case is refused or answered wrongly, never both; recorded as both, there
  // would be no saying which outcome the run is held to.
  for (const std::string& id : kRecords.refusals.SharedIds(kRecords.defects)) {
    fmt::print(
        stderr, "the '{}' path records '{}' as both a refusal and a defect\n",
        kPath.name, id);
    records_hold = false;
  }
  if (!records_hold) {
    return 1;
  }

  // The record is checked against the whole corpus rather than against the
  // selection, so a refusal recorded for a case this target does not run is
  // still held to naming a case that exists.
  std::vector<const ConformanceCase*> selected;
  for (const ConformanceCase& c : kCases) {
    if (Selects(foreign, c)) {
      selected.push_back(&c);
    }
  }
  if (selected.empty()) {
    fmt::print(
        stderr,
        "the selection holds no cases, so passing would report coverage that "
        "was never measured\n");
    return 1;
  }

  fmt::print(
      "{} of {} cases on the {} path, built with {}\n", selected.size(),
      kCases.size(), kPath.name, HostCompiler());

  const std::string group = GroupName(kPath.name);
  // NOLINTBEGIN(cppcoreguidelines-owning-memory)
  for (const ConformanceCase* selected_case : selected) {
    const ConformanceCase& c = *selected_case;
    const std::string name = TestName(c.id);
    testing::RegisterTest(
        group.c_str(), name.c_str(), nullptr, nullptr, __FILE__, __LINE__,
        [&c]() -> testing::Test* {
          return new ConformanceTest(c, &kCorpus.lyra_exe, &kPath, &kRecords);
        });
  }

  // Parked cases are held only to elaborating, which no path decides, so every
  // target holds them to the same thing rather than one being picked to.
  static const std::vector<ConformanceCase> kParked =
      LoadParkedCases(kCorpus.cases_root);
  for (const ConformanceCase& c : kParked) {
    const std::string name = TestName(c.id);
    testing::RegisterTest(
        "Parked", name.c_str(), nullptr, nullptr, __FILE__, __LINE__,
        [&c]() -> testing::Test* {
          return new ParkedCaseTest(c, &kCorpus.lyra_exe);
        });
  }
  // NOLINTEND(cppcoreguidelines-owning-memory)

  return RUN_ALL_TESTS();
}
