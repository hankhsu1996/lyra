// Whether Lyra builds and runs the designs other simulators are measured on.
// The conformance corpus cannot say: each of its cases is one small unit, and
// what stops a real design is a hierarchy, a size, or a way of writing that no
// clause-by-clause case thinks to write.
//
// The designs are those of Verilator's benchmark suite, read as that suite
// publishes them. Each states its sources, what reading them needs, its tests
// and the script that checks a run, once, for any simulator; what is built from
// that here is a declaration of the design in Lyra's own form, so a case is
// reproduced by building in the directory that declaration is written to.
//
// A case is the one the suite marks as a design's quick check. What it is
// recorded as stopping on is held exactly: a case that stops somewhere else,
// says something else, or stops no longer fails until the record says so, which
// makes the record the account of how far each design gets.

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <gtest/gtest.h>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "lyra/driver/subprocess.hpp"
#include "tests/framework/cli_fixture.hpp"
#include "tests/framework/process.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;
using lyra::test::MakeScratchDir;
using lyra::test::ProcessOutcome;
using lyra::test::RunChildProcess;
using lyra::test::TerminationKind;
using namespace std::chrono_literals;

namespace {

// The tag the suite gives the case that says whether a design works at all.
constexpr std::string_view kQuickCheckTag = "sanity";

// What a step may address, in kibibytes. A build's memory is the compiler's
// own and the host compiler's it starts, and nothing else bounds either: one
// design that asks for more than the machine has takes the machine with it,
// and every case after it. Past this a design stops at the step that asked,
// which is the finding. It is set under what the smallest machine these run on
// holds, so a case ends the same way wherever it is run.
constexpr std::uint64_t kStepAddressLimitKib = 12ULL << 20;

// How much of what a step said is shown when it is not what was recorded. The
// first lines name the cause; a design that stops can say thousands more.
constexpr std::size_t kLinesShown = 40;

enum class Step : std::uint8_t {
  kBuild,
  kRun,
  kCheck,
};

auto NameOf(Step step) -> std::string_view {
  switch (step) {
    case Step::kBuild:
      return "build";
    case Step::kRun:
      return "run";
    case Step::kCheck:
      return "check";
  }
  std::unreachable();
}

// How long a step may take; a design past it stops at that step, which is the
// finding, and how long each case took is said beside it.
//
// A build is bounded well past the slowest one measured, so that which side of
// the limit a design falls on does not turn on the machine: what it catches is
// a build that will not end. A run is bounded by what it should cost. Each
// case is the suite's quick check, which Verilator's own published figures put
// under a second for most designs and at about thirty for the largest, so ten
// times the largest is the limit. A run past it is either stuck or slower than
// that, and waiting longer does not say which.
auto TimeLimitOf(Step step) -> std::chrono::seconds {
  switch (step) {
    case Step::kBuild:
      return 1800s;
    case Step::kRun:
    case Step::kCheck:
      return 300s;
  }
  std::unreachable();
}

auto StepNamed(std::string_view name) -> std::optional<Step> {
  for (const Step step : {Step::kBuild, Step::kRun, Step::kCheck}) {
    if (NameOf(step) == name) {
      return step;
    }
  }
  return std::nullopt;
}

// A step a case stops at and what that step says. Of a case that was run it is
// everything the step said; of the record it is words the step says there.
struct Stop {
  Step at;
  std::string said;
};

struct DesignCase {
  // The suite's own name for it: design, configuration and test.
  std::string name;
  // The design as Lyra is told of it.
  std::string declaration;
  // What the test reads from the directory it runs in.
  std::vector<std::filesystem::path> files;
  std::vector<std::string> arguments;
  // The suite's check of a finished run, where the design has one.
  std::optional<std::filesystem::path> check;
};

// The suite states a part of a design in up to four places -- for the design,
// for one test of it, and for either within one configuration -- and reads them
// in that order: a list is every place's entries one after another, a map takes
// a later place's value for a key, and a single value is the last one given.
using Sections = std::vector<YAML::Node>;

auto Section(const YAML::Node& node, const std::string& key) -> YAML::Node {
  if (!node.IsMap()) {
    return {};
  }
  const YAML::Node found = node[key];
  return found.IsDefined() ? found : YAML::Node{};
}

auto ListOf(const Sections& sections, const std::string& key)
    -> std::vector<std::string> {
  std::vector<std::string> all;
  for (const YAML::Node& section : sections) {
    const YAML::Node entries = Section(section, key);
    if (!entries.IsSequence()) continue;
    for (const YAML::Node& entry : entries) {
      all.push_back(entry.as<std::string>());
    }
  }
  return all;
}

auto MapOf(const Sections& sections, const std::string& key)
    -> std::map<std::string, std::string> {
  std::map<std::string, std::string> merged;
  for (const YAML::Node& section : sections) {
    const YAML::Node entries = Section(section, key);
    if (!entries.IsMap()) continue;
    for (const auto& entry : entries) {
      merged[entry.first.as<std::string>()] = entry.second.as<std::string>();
    }
  }
  return merged;
}

auto LastOf(const Sections& sections, const std::string& key)
    -> std::optional<std::string> {
  std::optional<std::string> last;
  for (const YAML::Node& section : sections) {
    const YAML::Node value = Section(section, key);
    if (value.IsScalar()) {
      last = value.as<std::string>();
    }
  }
  return last;
}

auto TomlString(std::string_view text) -> std::string {
  std::string out = "\"";
  for (const char c : text) {
    if (c == '"' || c == '\\') {
      out += '\\';
    }
    out += c;
  }
  out += '"';
  return out;
}

auto TomlArray(const std::vector<std::string>& entries) -> std::string {
  std::string out = "[\n";
  for (const std::string& entry : entries) {
    out += std::format("    {},\n", TomlString(entry));
  }
  out += "]";
  return out;
}

// A library is named by an identifier, which a design's directory need not be.
auto LibraryNameOf(std::string_view design) -> std::string {
  std::string name;
  for (const char c : design) {
    const auto u = static_cast<unsigned char>(c);
    name += std::isalnum(u) != 0 ? static_cast<char>(std::tolower(u)) : '_';
  }
  return name;
}

// Every time scale a `timescale directive in these files gives, spelled with no
// space in it.
auto TimeScalesStatedIn(const std::vector<std::string>& files)
    -> std::set<std::string> {
  constexpr std::string_view kDirective = "`timescale";
  std::set<std::string> stated;
  for (const std::string& file : files) {
    std::ifstream in(file);
    for (std::string line; std::getline(in, line);) {
      const std::size_t at = line.find(kDirective);
      if (at == std::string::npos) continue;
      std::string scale;
      for (const char c :
           std::string_view(line).substr(at + kDirective.size())) {
        if (std::isalnum(static_cast<unsigned char>(c)) != 0 || c == '/') {
          scale += c;
        } else if (
            !scale.empty() && scale.contains('/') && scale.back() != '/') {
          break;
        }
      }
      stated.insert(scale);
    }
  }
  return stated;
}

// The design as its descriptor's compile sections state it, in the form Lyra
// reads. The suite's own module, which counts the clock the descriptor names,
// is one more source, as it is for every simulator the suite runs.
//
// Two things are said of every design because they are how the suite's
// reference simulator reads one: every file is one compilation unit, so a
// macro an early file defines reaches a later one, and assertions are not
// evaluated.
auto DeclarationOf(
    std::string_view design, const std::filesystem::path& design_dir,
    const std::filesystem::path& suite, const Sections& compile)
    -> std::string {
  std::vector<std::string> files;
  for (const std::string& file : ListOf(compile, "verilogSourceFiles")) {
    files.push_back((design_dir / file).string());
  }
  files.push_back((suite / "rtl" / "__rtlmeter_utils.sv").string());

  std::vector<std::string> incdir;
  const auto searched = [&](const std::filesystem::path& dir) {
    if (std::ranges::find(incdir, dir.string()) == incdir.end()) {
      incdir.push_back(dir.string());
    }
  };
  for (const std::string& file : ListOf(compile, "verilogIncludeFiles")) {
    searched((design_dir / file).parent_path());
  }
  searched(suite / "rtl");

  // The suite's sources were prepared for its reference simulator and many
  // select on the macros that simulator predefines -- one naming it, one saying
  // it carries out delays -- some of them with no branch for any other tool.
  // The macros are therefore part of the text every design here is, and what is
  // built is the design that simulator builds.
  std::vector<std::string> defines = {"VERILATOR=1", "VERILATOR_TIMING=1"};
  for (const auto& [macro, value] : MapOf(compile, "verilogDefines")) {
    defines.push_back(std::format("{}={}", macro, value));
  }
  defines.push_back(
      std::format(
          "__RTLMETER_MAIN_CLOCK={}",
          LastOf(compile, "mainClock").value_or("")));

  std::vector<std::string> native;
  for (const std::string& file : ListOf(compile, "cppSourceFiles")) {
    native.push_back((design_dir / file).string());
  }

  // A time scale is a fact about the design that the descriptor has no place
  // for of its own, so it is written among the reference simulator's options.
  // Where it is not, and the sources state exactly one, that one is it: the
  // reference simulator reads an element stating none at the time scale the
  // rest of the design states, where the standard has a design that states
  // one for some elements and not others be an error (LRM 3.14.2.3).
  std::string time_scale;
  const std::vector<std::string> options = ListOf(compile, "verilatorArgs");
  if (const auto flag = std::ranges::find(options, "--timescale");
      flag != options.end() && std::next(flag) != options.end()) {
    time_scale = std::format("timescale = {}\n", TomlString(*std::next(flag)));
  } else if (const std::set<std::string> stated = TimeScalesStatedIn(files);
             stated.size() == 1) {
    time_scale = std::format("timescale = {}\n", TomlString(*stated.begin()));
  }

  return std::format(
      "[library]\nname = {}\nfiles = {}\nincdir = {}\ndefines = {}\ndpi = {}\n"
      "\n[design]\ntop = [{}]\n"
      "\n[compile]\n{}single_unit = true\nassertions = \"skip\"\n",
      TomlString(LibraryNameOf(design)), TomlArray(files), TomlArray(incdir),
      TomlArray(defines), TomlArray(native),
      TomlString(LastOf(compile, "topModule").value_or("")), time_scale);
}

// Every case of one design that the suite marks as its quick check.
auto QuickChecksOf(
    const std::string& design, const std::filesystem::path& design_dir,
    const std::filesystem::path& suite) -> std::vector<DesignCase> {
  const YAML::Node descriptor =
      YAML::LoadFile((design_dir / "descriptor.yaml").string());
  const YAML::Node execute = Section(descriptor, "execute");
  // A design that names no configuration has one, which the suite calls this.
  std::map<std::string, YAML::Node> configurations;
  if (const YAML::Node stated = Section(descriptor, "configurations");
      stated.IsMap()) {
    for (const auto& entry : stated) {
      configurations.emplace(entry.first.as<std::string>(), entry.second);
    }
  } else {
    configurations.emplace("default", YAML::Node{});
  }

  std::vector<DesignCase> cases;
  for (const auto& [configuration, stated] : configurations) {
    const YAML::Node configured = Section(stated, "execute");
    std::set<std::string> tests;
    for (const YAML::Node& from :
         {Section(execute, "tests"), Section(configured, "tests")}) {
      if (!from.IsMap()) continue;
      for (const auto& entry : from) {
        tests.insert(entry.first.as<std::string>());
      }
    }
    for (const std::string& test : tests) {
      const Sections run = {
          Section(execute, "common"), Section(Section(execute, "tests"), test),
          Section(configured, "common"),
          Section(Section(configured, "tests"), test)};
      const std::vector<std::string> tags = ListOf(run, "tags");
      if (std::ranges::find(tags, kQuickCheckTag) == tags.end()) continue;

      DesignCase quick{
          .name = std::format("{}:{}:{}", design, configuration, test),
          .declaration = DeclarationOf(
              design, design_dir, suite,
              {Section(descriptor, "compile"), Section(stated, "compile")}),
          .files = {},
          .arguments = ListOf(run, "args"),
          .check = std::nullopt};
      for (const std::string& file : ListOf(run, "files")) {
        quick.files.push_back(design_dir / file);
      }
      if (const auto hook = LastOf(run, "postHook")) {
        quick.check = design_dir / *hook;
      }
      cases.push_back(std::move(quick));
    }
  }
  return cases;
}

auto ShellQuoted(std::string_view word) -> std::string {
  std::string out = "'";
  for (const char c : word) {
    if (c == '\'') {
      out += "'\\''";
    } else {
      out += c;
    }
  }
  out += "'";
  return out;
}

// Runs a shell command from `dir`, since where a step runs is part of what it
// is: a declaration is found from there, and a test reads and writes there.
auto RunFrom(
    Step step, const std::filesystem::path& dir, const std::string& command)
    -> ProcessOutcome {
  const auto sh = lyra::driver::FindOnPath("sh");
  if (!sh.has_value()) {
    return {};
  }
  const std::vector<std::string> argv = {
      "-c", std::format(
                "ulimit -v {} && cd {} && {}", kStepAddressLimitKib,
                ShellQuoted(dir.string()), command)};
  return RunChildProcess(*sh, argv, TimeLimitOf(step));
}

auto Said(Step step, const ProcessOutcome& outcome) -> std::string {
  if (outcome.termination == TerminationKind::kTimedOut) {
    return std::format(
        "did not end within {} s\n{}{}", TimeLimitOf(step).count(),
        outcome.stdout_text, outcome.stderr_text);
  }
  return outcome.stdout_text + outcome.stderr_text;
}

auto ReadWhole(const std::filesystem::path& path) -> std::string {
  const std::ifstream in(path);
  std::ostringstream text;
  text << in.rdbuf();
  return text.str();
}

// Builds the case in `work` and runs it there as the suite would, ending at
// the first step that fails. The run's output is kept where the suite's check
// reads it.
auto Carry(
    const std::filesystem::path& lyra, const DesignCase& design,
    const std::filesystem::path& work) -> std::optional<Stop> {
  std::ofstream(work / "lyra.toml") << design.declaration;
  // A design of this size draws warnings by the thousand, and what stops it is
  // an error, so only errors are asked for.
  const auto built = RunFrom(
      Step::kBuild, work,
      std::format(
          "{} build --backend llvm --progress=none -Wnone --cache-dir "
          "cache -o sim",
          ShellQuoted(lyra.string())));
  if (built.termination != TerminationKind::kExitedNormally) {
    return Stop{.at = Step::kBuild, .said = Said(Step::kBuild, built)};
  }

  const std::filesystem::path run = work / "run";
  std::filesystem::create_directories(run / "_execute");
  for (const std::filesystem::path& file : design.files) {
    std::filesystem::create_symlink(file, run / file.filename());
  }
  std::string command = "../sim";
  for (const std::string& argument : design.arguments) {
    command += ' ';
    command += ShellQuoted(argument);
  }
  const auto ran =
      RunFrom(Step::kRun, run, command + " > _execute/stdout.log 2>&1");
  if (ran.termination != TerminationKind::kExitedNormally) {
    return Stop{
        .at = Step::kRun,
        .said =
            Said(Step::kRun, ran) + ReadWhole(run / "_execute" / "stdout.log")};
  }

  if (design.check) {
    // The suite runs a check as a program of its own, so each is written in
    // whatever its first line names.
    const auto checked =
        RunFrom(Step::kCheck, run, ShellQuoted(design.check->string()));
    if (checked.termination != TerminationKind::kExitedNormally) {
      return Stop{.at = Step::kCheck, .said = Said(Step::kCheck, checked)};
    }
  }
  return std::nullopt;
}

auto FirstLines(std::string_view text) -> std::string {
  std::size_t end = 0;
  for (std::size_t line = 0; line < kLinesShown && end < text.size(); ++line) {
    const std::size_t next = text.find('\n', end);
    end = next == std::string_view::npos ? text.size() : next + 1;
  }
  return std::string(text.substr(0, end));
}

// Holds what a case did against what is recorded of it. Nothing where they
// agree; otherwise what differs and what the record would have to say.
auto Differs(
    const std::optional<Stop>& stop, const std::optional<Stop>& recorded)
    -> std::optional<std::string> {
  if (!stop) {
    if (!recorded) return std::nullopt;
    return std::format(
        "is recorded as stopping at its {} and no longer stops, so its entry "
        "goes",
        NameOf(recorded->at));
  }
  if (recorded && recorded->at == stop->at &&
      stop->said.find(recorded->said) != std::string::npos) {
    return std::nullopt;
  }
  const std::string was =
      recorded ? std::format(
                     "recorded as stopping at its {} saying '{}'",
                     NameOf(recorded->at), recorded->said)
               : std::string("recorded as stopping nowhere");
  return std::format(
      "stops at its {}, and is {}. What the step said:\n{}", NameOf(stop->at),
      was, FirstLines(stop->said));
}

// The cases and their record, found where the build put them.
struct Corpus {
  std::vector<DesignCase> cases;
  std::map<std::string, Stop> recorded;
  std::vector<std::string> problems;
};

auto LoadCorpus() -> Corpus {
  Corpus corpus;
  std::string error;
  const std::unique_ptr<Runfiles> runfiles{
      Runfiles::CreateForTest(BAZEL_CURRENT_REPOSITORY, &error)};
  if (!runfiles) {
    corpus.problems.push_back(
        std::format("failed to create runfiles: {}", error));
    return corpus;
  }
  // Resolved to where the suite really is, so a path a diagnostic or a
  // declaration names is one that outlives this run.
  std::error_code missing;
  const std::filesystem::path module = std::filesystem::canonical(
      runfiles->Rlocation("rtlmeter/rtl/__rtlmeter_utils.sv"), missing);
  if (missing) {
    corpus.problems.emplace_back("the suite is not among this run's inputs");
    return corpus;
  }
  const std::filesystem::path suite = module.parent_path().parent_path();

  std::set<std::filesystem::path> design_dirs;
  for (const auto& entry :
       std::filesystem::directory_iterator(suite / "designs")) {
    if (std::filesystem::exists(entry.path() / "descriptor.yaml")) {
      design_dirs.insert(entry.path());
    }
  }
  for (const std::filesystem::path& dir : design_dirs) {
    auto quick = QuickChecksOf(dir.filename().string(), dir, suite);
    std::ranges::move(quick, std::back_inserter(corpus.cases));
  }

  const YAML::Node record = YAML::LoadFile(
      runfiles->Rlocation("_main/tests/paths/llvm.external_designs.yaml"));
  for (const auto& entry : record) {
    const auto name = entry.first.as<std::string>();
    const auto at = StepNamed(entry.second["stops"].as<std::string>(""));
    if (!at) {
      corpus.problems.push_back(
          std::format("the record of '{}' names no step it stops at", name));
      continue;
    }
    const auto is_named = [&](const DesignCase& design) {
      return design.name == name;
    };
    if (std::ranges::none_of(corpus.cases, is_named)) {
      corpus.problems.push_back(
          std::format("the record names '{}', which is not a case", name));
      continue;
    }
    corpus.recorded.emplace(
        name,
        Stop{.at = *at, .said = entry.second["says"].as<std::string>("")});
  }
  return corpus;
}

}  // namespace

namespace lyra::test {

// One case with what is recorded of it. The test framework finds how to print
// a parameter by the parameter's own namespace, which is why this one has a
// name.
struct DesignSubject {
  DesignCase design;
  std::optional<Stop> recorded;

  friend auto PrintTo(const DesignSubject& subject, std::ostream* out) -> void {
    *out << subject.design.name;
  }
};

}  // namespace lyra::test

namespace {

using lyra::test::DesignSubject;

auto Subjects() -> std::vector<DesignSubject> {
  const Corpus corpus = LoadCorpus();
  std::vector<DesignSubject> subjects;
  for (const DesignCase& design : corpus.cases) {
    const auto recorded = corpus.recorded.find(design.name);
    subjects.push_back(
        DesignSubject{
            .design = design,
            .recorded = recorded == corpus.recorded.end()
                            ? std::nullopt
                            : std::optional{recorded->second}});
  }
  return subjects;
}

// A record naming a case the suite does not have would hold nothing, and every
// other test here would still pass.
TEST(DesignRecord, NamesOnlyCasesTheSuiteHas) {
  const Corpus corpus = LoadCorpus();
  for (const std::string& problem : corpus.problems) {
    ADD_FAILURE() << problem;
  }
  EXPECT_FALSE(corpus.cases.empty());
}

class Design : public testing::TestWithParam<DesignSubject> {};

TEST_P(Design, GetsAsFarAsIsRecordedOfIt) {
  const auto lyra = lyra::test::ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  auto work = MakeScratchDir();
  ASSERT_TRUE(work.has_value()) << work.error();

  const DesignSubject& subject = GetParam();
  const auto began = std::chrono::steady_clock::now();
  const std::optional<Stop> stop = Carry(lyra, subject.design, *work);
  const auto took = std::chrono::duration_cast<std::chrono::seconds>(
      std::chrono::steady_clock::now() - began);
  // Written as each case ends, so a run that is cut short has still said how
  // far the cases before it got, and with where it was built, since a case
  // that stops as recorded is looked into from there as much as one that does
  // not.
  std::cout << std::format(
                   "{}: {} after {} s, built in {}", subject.design.name,
                   stop ? std::format("stops at its {}", NameOf(stop->at))
                        : std::string("builds, runs and passes its check"),
                   took.count(), work->string())
            << std::endl;

  if (const auto differs = Differs(stop, subject.recorded)) {
    ADD_FAILURE() << subject.design.name << " " << *differs
                  << "\n`lyra build --backend llvm` from the directory above "
                     "is the step that builds it.";
  }
}

INSTANTIATE_TEST_SUITE_P(
    Suite, Design, testing::ValuesIn(Subjects()),
    [](const testing::TestParamInfo<DesignSubject>& subject) {
      std::string name;
      for (const char c : subject.param.design.name) {
        name += std::isalnum(static_cast<unsigned char>(c)) != 0 ? c : '_';
      }
      return name;
    });

}  // namespace
