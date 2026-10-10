// A build after an edit makes again the units that now mean something else,
// and no others. No program a design simulates can observe which units a build
// made, and no single build shows it. So each case here is one design twice,
// before an edit and after it, stating which units the edit gives another
// meaning; the design is built as it was, then as it is, and the units the
// second build made are held to that statement.
//
// A case states one kind of edit, so what its record names is that kind. An
// edit that is not about where text sits keeps every line where it was, since
// a unit that follows its position would otherwise answer for every case.

#include <cstdint>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <iterator>
#include <map>
#include <memory>
#include <nlohmann/json.hpp>
#include <optional>
#include <ostream>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "tests/framework/cli_fixture.hpp"
#include "tests/framework/held_record.hpp"
#include "tests/framework/process.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;
using lyra::test::HoldToRecord;
using lyra::test::LoadByName;
using lyra::test::MakeScratchDir;
using lyra::test::ReadJson;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;
using namespace std::chrono_literals;

namespace {

// The line of a case's first file that names the units its edit gives another
// meaning, by the names a build reports them under.
constexpr std::string_view kRemakes = "// @remakes:";

enum class Backend : std::uint8_t {
  kCpp,
  kLlvm,
};

auto NameOf(Backend backend) -> std::string_view {
  switch (backend) {
    case Backend::kCpp:
      return "cpp";
    case Backend::kLlvm:
      return "llvm";
  }
  std::unreachable();
}

struct IncrementalCase {
  std::string name;
  std::filesystem::path before;
  std::filesystem::path after;
  std::set<std::string> remakes;
  // How this case is recorded as departing from that, for every backend.
  std::map<Backend, std::set<std::string>> recorded;
};

auto ReadWholeFile(const std::filesystem::path& path) -> std::string {
  std::ifstream in(path, std::ios::binary);
  return {std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
}

auto Run(
    const std::filesystem::path& lyra, const std::vector<std::string>& command)
    -> std::expected<void, std::string> {
  const auto ran = RunChildProcess(lyra, command, 300s);
  if (ran.termination != TerminationKind::kExitedNormally) {
    std::string said = "lyra";
    for (const std::string& word : command) {
      said += ' ';
      said += word;
    }
    return std::unexpected(
        std::format(
            "{} did not compile:\n{}{}", said, ran.stdout_text,
            ran.stderr_text));
  }
  return {};
}

// Every line of text one translation unit is handed: its own, and that of every
// file it reaches through an include of the project's own. What a compile of it
// depends on is this, so this is what decides whether it is compiled again.
auto ReadCompileInput(
    const std::filesystem::path& dir, const std::filesystem::path& entry,
    std::set<std::filesystem::path>& seen) -> std::string {
  if (!seen.insert(entry).second) {
    return {};
  }
  const std::string own = ReadWholeFile(dir / entry);
  std::string out = own;
  const std::regex include{"#include \"([^\"/]+)\""};
  for (auto it = std::sregex_iterator(own.begin(), own.end(), include);
       it != std::sregex_iterator(); ++it) {
    const std::filesystem::path named = (*it)[1].str();
    if (std::filesystem::exists(dir / named)) {
      out += ReadCompileInput(dir, named, seen);
    }
  }
  return out;
}

// What each unit of an emitted project hands its compiler, by the unit's name.
auto EmittedCompileInputs(
    const std::filesystem::path& lyra, const std::filesystem::path& source,
    const std::filesystem::path& out)
    -> std::expected<std::map<std::string, std::string>, std::string> {
  const std::filesystem::path stats = out.string() + ".stats.json";
  const auto emitted =
      Run(lyra, {"emit", "cpp", "--top", "Top", "-o", out.string(),
                 "--stats-file", stats.string(), source.string()});
  if (!emitted.has_value()) {
    return std::unexpected(emitted.error());
  }
  const nlohmann::json report = ReadJson(stats);
  if (report.is_discarded()) {
    return std::unexpected("the emit wrote no report of itself");
  }
  std::map<std::string, std::string> inputs;
  for (const nlohmann::json& unit : report.at("units")) {
    std::string& input = inputs[unit.at("name").get<std::string>()];
    for (const nlohmann::json& artifact : unit.at("artifacts")) {
      if (artifact.at("kind").get<std::string>() == "C++ source") {
        std::set<std::filesystem::path> seen;
        input +=
            ReadCompileInput(out, artifact.at("name").get<std::string>(), seen);
      }
    }
  }
  return inputs;
}

// The units the build of a design after its edit made again, the build before
// the edit having left what it made.
//
// The C++ backend keeps no object, so there a unit is made again where what its
// translation units are handed differs. The execution backend keeps each
// unit's object, and its own report says which it made.
auto UnitsMadeAgain(
    const std::filesystem::path& lyra, const IncrementalCase& incremental,
    Backend backend, const std::filesystem::path& scratch)
    -> std::expected<std::set<std::string>, std::string> {
  // One path for both, so the file's own name is the same in each build.
  const std::filesystem::path source = scratch / "design.sv";
  std::set<std::string> made_again;
  switch (backend) {
    case Backend::kCpp: {
      std::ofstream(source) << ReadWholeFile(incremental.before);
      const auto before = EmittedCompileInputs(lyra, source, scratch / "was");
      if (!before.has_value()) {
        return std::unexpected(before.error());
      }
      std::ofstream(source) << ReadWholeFile(incremental.after);
      const auto after = EmittedCompileInputs(lyra, source, scratch / "is");
      if (!after.has_value()) {
        return std::unexpected(after.error());
      }
      for (const auto& [unit, input] : *after) {
        const auto was = before->find(unit);
        if (was == before->end() || was->second != input) {
          made_again.insert(unit);
        }
      }
      return made_again;
    }
    case Backend::kLlvm: {
      const std::filesystem::path stats = scratch / "stats.json";
      const std::vector<std::string> build = {
          "build",
          "--backend",
          "llvm",
          "--top",
          "Top",
          "-o",
          (scratch / "program").string(),
          "--cache-dir",
          (scratch / "cache").string(),
          "--stats-file",
          stats.string(),
          source.string()};
      std::ofstream(source) << ReadWholeFile(incremental.before);
      if (const auto built = Run(lyra, build); !built.has_value()) {
        return std::unexpected(built.error());
      }
      std::ofstream(source) << ReadWholeFile(incremental.after);
      if (const auto built = Run(lyra, build); !built.has_value()) {
        return std::unexpected(built.error());
      }
      const nlohmann::json report = ReadJson(stats);
      if (report.is_discarded()) {
        return std::unexpected("the build wrote no report of itself");
      }
      for (const nlohmann::json& unit : report.at("units")) {
        for (const nlohmann::json& artifact : unit.at("artifacts")) {
          if (artifact.at("made").get<bool>()) {
            made_again.insert(unit.at("name").get<std::string>());
          }
        }
      }
      return made_again;
    }
  }
  std::unreachable();
}

// Holds one case on one backend to what is recorded for it. Nothing where it
// holds; otherwise what differs.
auto CheckIncremental(
    const std::filesystem::path& lyra, const IncrementalCase& incremental,
    Backend backend) -> std::optional<std::string> {
  auto scratch = MakeScratchDir();
  if (!scratch.has_value()) {
    return scratch.error();
  }
  const auto made_again = UnitsMadeAgain(lyra, incremental, backend, *scratch);
  if (!made_again.has_value()) {
    return made_again.error();
  }

  // How the build departs from what the case states, in the words a record
  // names it by.
  std::set<std::string> departs;
  for (const std::string& unit : *made_again) {
    if (!incremental.remakes.contains(unit)) {
      departs.insert(std::format("made {}", unit));
    }
  }
  for (const std::string& unit : incremental.remakes) {
    if (!made_again->contains(unit)) {
      departs.insert(std::format("kept {}", unit));
    }
  }

  const lyra::test::Departures departures =
      HoldToRecord(incremental.recorded.at(backend), departs);
  if (departures.Empty()) {
    return std::nullopt;
  }
  std::string differs;
  for (const std::string& entry : departures.not_recorded) {
    differs += std::format(
        "  its edit gave the unit no other meaning, or gave it one: {}\n",
        entry);
  }
  for (const std::string& entry : departures.no_longer_found) {
    differs += std::format(
        "  recorded and no longer so, so its line goes: {}\n", entry);
  }
  return std::format(
      "'{}' on the {} backend, built before its edit and after:\n{}",
      incremental.name, NameOf(backend), differs);
}

// The units a case's first file says its edit gives another meaning, or
// nothing where the file does not say.
auto RemakesStatedBy(const std::filesystem::path& before)
    -> std::optional<std::set<std::string>> {
  std::ifstream in(before);
  std::string line;
  if (!std::getline(in, line) || !line.starts_with(kRemakes)) {
    return std::nullopt;
  }
  std::istringstream units{line.substr(kRemakes.size())};
  return std::set<std::string>{
      std::istream_iterator<std::string>{units},
      std::istream_iterator<std::string>{}};
}

struct Corpus {
  std::vector<IncrementalCase> cases;
  std::vector<std::string> problems;
};

// Every case under the incremental directory, with what each path's record
// says of it. A case missing a file or its statement, and a record naming no
// case, are reported.
auto LoadCorpus() -> Corpus {
  Corpus corpus;
  std::string error;
  const std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&error)};
  if (!runfiles) {
    corpus.problems.push_back(
        std::format("failed to create runfiles: {}", error));
    return corpus;
  }
  const std::filesystem::path root =
      runfiles->Rlocation("_main/tests/incremental");
  const std::filesystem::path paths = runfiles->Rlocation("_main/tests/paths");

  std::map<std::string, IncrementalCase> cases;
  for (const auto& entry : std::filesystem::directory_iterator(root)) {
    if (!entry.is_directory()) {
      continue;
    }
    const std::string name = entry.path().filename().string();
    const std::filesystem::path before = entry.path() / "before.sv";
    const std::filesystem::path after = entry.path() / "after.sv";
    const auto remakes = RemakesStatedBy(before);
    if (!std::filesystem::exists(after) || !remakes.has_value()) {
      corpus.problems.push_back(
          std::format(
              "case '{}' needs a before.sv opening with '{}' and an after.sv",
              name, kRemakes));
      continue;
    }
    cases[name] = IncrementalCase{
        .name = name,
        .before = before,
        .after = after,
        .remakes = *remakes,
        .recorded = {{Backend::kCpp, {}}, {Backend::kLlvm, {}}}};
  }
  for (const Backend backend : {Backend::kCpp, Backend::kLlvm}) {
    const std::string file =
        std::format("{}.incremental.yaml", NameOf(backend));
    for (const auto& [name, departs] :
         LoadByName<std::vector<std::string>>(paths / file)) {
      if (const auto found = cases.find(name); found != cases.end()) {
        found->second.recorded.at(backend).insert(
            departs.begin(), departs.end());
      } else {
        corpus.problems.push_back(
            std::format("{} records '{}', which is not a case", file, name));
      }
    }
  }
  for (auto& [name, incremental] : cases) {
    corpus.cases.push_back(std::move(incremental));
  }
  return corpus;
}

}  // namespace

namespace lyra::test {

// One case on one backend. The test framework finds how to print a parameter
// by the parameter's own namespace, which is why this one has a name.
struct IncrementalSubject {
  IncrementalCase incremental;
  Backend backend;

  friend auto PrintTo(const IncrementalSubject& subject, std::ostream* out)
      -> void {
    *out << subject.incremental.name << " on " << NameOf(subject.backend);
  }
};

}  // namespace lyra::test

namespace {

using lyra::test::IncrementalSubject;

auto Subjects() -> std::vector<IncrementalSubject> {
  std::vector<IncrementalSubject> subjects;
  for (const IncrementalCase& incremental : LoadCorpus().cases) {
    for (const Backend backend : {Backend::kCpp, Backend::kLlvm}) {
      subjects.push_back(
          IncrementalSubject{.incremental = incremental, .backend = backend});
    }
  }
  return subjects;
}

// A case that states nothing, or a record naming one that is not there, would
// leave it unheld and every other test here still passing.
TEST(IncrementalRecord, NamesEveryCaseAndNothingElse) {
  const Corpus corpus = LoadCorpus();
  for (const std::string& problem : corpus.problems) {
    ADD_FAILURE() << problem;
  }
  EXPECT_FALSE(corpus.cases.empty());
}

class Incremental : public testing::TestWithParam<IncrementalSubject> {};

TEST_P(Incremental, MakesAgainOnlyWhatMeansSomethingElse) {
  const auto lyra = lyra::test::ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  if (auto failure =
          CheckIncremental(lyra, GetParam().incremental, GetParam().backend)) {
    ADD_FAILURE() << *failure;
  }
}

INSTANTIATE_TEST_SUITE_P(
    Cases, Incremental, testing::ValuesIn(Subjects()),
    [](const testing::TestParamInfo<IncrementalSubject>& subject) {
      return std::format(
          "{}_{}", NameOf(subject.param.backend),
          subject.param.incremental.name);
    });

}  // namespace
