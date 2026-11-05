#include "tests/framework/external_design.hpp"

#include <algorithm>
#include <cctype>
#include <cstddef>
#include <filesystem>
#include <format>
#include <fstream>
#include <iterator>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "tools/cpp/runfiles/runfiles.h"

namespace lyra::test {

namespace {

// The tag the suite gives the case that says whether a design works at all.
constexpr std::string_view kQuickCheckTag = "sanity";

auto StepNamed(std::string_view name) -> std::optional<DesignStep> {
  for (const DesignStep step :
       {DesignStep::kBuild, DesignStep::kRun, DesignStep::kCheck}) {
    if (NameOf(step) == name) {
      return step;
    }
  }
  return std::nullopt;
}

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

  // A time scale and a value for a parameter of the top are facts about the
  // design that the descriptor has no place for of their own, so they are
  // written among the reference simulator's options. A design built without
  // its values is another design: a clock whose period is left at a default
  // of zero never lets time advance.
  const std::vector<std::string> options = ListOf(compile, "verilatorArgs");
  std::vector<std::string> values;
  for (const std::string& option : options) {
    if (option.starts_with("-G")) {
      values.push_back(option.substr(2));
    }
  }

  // Where no time scale is among them, and the sources state exactly one, that
  // one is it: the reference simulator reads an element stating none at the
  // time scale the rest of the design states, where the standard has a design
  // that states one for some elements and not others be an error (LRM
  // 3.14.2.3).
  std::string time_scale;
  if (const auto flag = std::ranges::find(options, "--timescale");
      flag != options.end() && std::next(flag) != options.end()) {
    time_scale = std::format("timescale = {}\n", TomlString(*std::next(flag)));
  } else if (const std::set<std::string> stated = TimeScalesStatedIn(files);
             stated.size() == 1) {
    time_scale = std::format("timescale = {}\n", TomlString(*stated.begin()));
  }

  return std::format(
      "[library]\nname = {}\nfiles = {}\nincdir = {}\ndefines = {}\ndpi = {}\n"
      "\n[design]\ntop = [{}]\nparams = {}\n"
      "\n[compile]\n{}single_unit = true\nassertions = \"skip\"\n",
      TomlString(LibraryNameOf(design)), TomlArray(files), TomlArray(incdir),
      TomlArray(defines), TomlArray(native),
      TomlString(LastOf(compile, "topModule").value_or("")), TomlArray(values),
      time_scale);
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
          .design_name = design,
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
        quick.check = std::vector{(design_dir / *hook).string()};
      }
      cases.push_back(std::move(quick));
    }
  }
  return cases;
}

// Where `relative`, a path under the inputs `root` names, really is, so a
// path a diagnostic or a declaration names is one that outlives this run; and
// with it the directory `root` names, which is that path with `relative` taken
// off its end.
struct Located {
  std::filesystem::path path;
  std::filesystem::path root;
};

auto Locate(
    const bazel::tools::cpp::runfiles::Runfiles& runfiles,
    const std::string& root, const std::filesystem::path& relative)
    -> std::optional<Located> {
  // A case may name a file beside its own directory, so `..` is taken out
  // before what is left is counted off the real path.
  const std::filesystem::path within = relative.lexically_normal();
  std::error_code missing;
  const std::filesystem::path path = std::filesystem::canonical(
      runfiles.Rlocation(std::format("{}/{}", root, within.string())), missing);
  if (missing) return std::nullopt;
  std::filesystem::path base = path;
  for (auto part = within.begin(); part != within.end(); ++part) {
    base = base.parent_path();
  }
  return Located{.path = path, .root = base};
}

// Every quick check of every design the benchmark suite holds.
auto BenchmarkSuiteCases(
    const bazel::tools::cpp::runfiles::Runfiles& runfiles,
    std::vector<std::string>& problems) -> std::vector<DesignCase> {
  const auto located = Locate(runfiles, "rtlmeter", "rtl/__rtlmeter_utils.sv");
  if (!located) {
    problems.emplace_back("the benchmark suite is not among this run's inputs");
    return {};
  }
  const std::filesystem::path& suite = located->root;
  std::set<std::filesystem::path> design_dirs;
  for (const auto& entry :
       std::filesystem::directory_iterator(suite / "designs")) {
    if (std::filesystem::exists(entry.path() / "descriptor.yaml")) {
      design_dirs.insert(entry.path());
    }
  }
  std::vector<DesignCase> cases;
  for (const std::filesystem::path& dir : design_dirs) {
    auto quick = QuickChecksOf(dir.filename().string(), dir, suite);
    std::ranges::move(quick, std::back_inserter(cases));
  }
  return cases;
}

// The cases of one suite `tests/external_design_suites.yaml` describes, as
// Lyra is told of them. Where the description names an input this run does
// not have, or cannot be read as it says, a problem says so and the cases it
// reaches are left out.
auto DescribedSuiteCases(
    const bazel::tools::cpp::runfiles::Runfiles& runfiles,
    const std::string& suite, const YAML::Node& described,
    std::vector<std::string>& problems) -> std::vector<DesignCase> {
  const std::string sources_in = LastOf({described}, "sources_in").value_or("");
  const std::string cases_in = LastOf({described}, "cases_in").value_or("");
  std::vector<std::string> sources;
  std::optional<std::filesystem::path> sources_root;
  for (const std::string& source : ListOf({described}, "sources")) {
    const auto located = Locate(runfiles, sources_in, source);
    if (!located) {
      problems.push_back(
          std::format(
              "'{}' names the source '{}/{}', which is not among this run's "
              "inputs",
              suite, sources_in, source));
      return {};
    }
    sources.push_back(located->path.string());
    sources_root = located->root;
  }
  // A suite's include directories are under its sources, which are what says
  // where those are.
  const std::vector<std::string> suite_dirs = ListOf({described}, "incdir");
  if (!suite_dirs.empty() && !sources_root) {
    problems.push_back(
        std::format(
            "'{}' names include directories under '{}' and no source there",
            suite, sources_in));
    return {};
  }
  std::vector<std::string> suite_incdir;
  for (const std::string& dir : suite_dirs) {
    suite_incdir.push_back((*sources_root / dir).lexically_normal().string());
  }
  std::string compile;
  if (const auto scale = LastOf({described}, "timescale")) {
    compile += std::format("timescale = {}\n", TomlString(*scale));
  }
  if (const YAML::Node single = Section(described, "single_unit");
      single.IsDefined() && single.as<bool>()) {
    compile += "single_unit = true\n";
  }

  std::vector<DesignCase> cases;
  for (const YAML::Node& entry : Section(described, "cases")) {
    const Sections both = {described, entry};
    const std::string dir_name = LastOf({entry}, "dir").value_or("");
    const std::vector<std::string> own = ListOf({entry}, "files");
    const auto first = own.empty()
                           ? std::nullopt
                           : Locate(
                                 runfiles, cases_in,
                                 std::filesystem::path(dir_name) / own.front());
    if (!first) {
      problems.push_back(
          std::format(
              "'{}' names the case '{}', whose first file is not among this "
              "run's inputs",
              suite, dir_name));
      continue;
    }
    const std::filesystem::path dir = first->root / dir_name;
    std::vector<std::string> files = sources;
    for (const std::string& file : own) {
      files.push_back((dir / file).lexically_normal().string());
    }
    std::vector<std::string> incdir = suite_incdir;
    incdir.push_back(dir.string());
    for (const std::string& path : ListOf({entry}, "incdir")) {
      incdir.push_back((dir / path).lexically_normal().string());
    }
    const std::string declaration = std::format(
        "[library]\nname = {}\nfiles = {}\nincdir = {}\ndefines = {}\n"
        "\n[compile]\n{}",
        TomlString(LibraryNameOf(std::format("{}_{}", suite, dir_name))),
        TomlArray(files), TomlArray(incdir), TomlArray(ListOf(both, "defines")),
        compile);

    // A case's own check replaces the suite's; the program it names is an
    // input of this run.
    const YAML::Node check_node = Section(entry, "check").IsSequence()
                                      ? Section(entry, "check")
                                      : Section(described, "check");
    std::optional<std::vector<std::string>> check;
    if (check_node.IsSequence()) {
      std::vector<std::string> command;
      for (const YAML::Node& word : check_node) {
        command.push_back(word.as<std::string>());
      }
      const std::string program = command.empty() ? "" : command.front();
      const std::size_t slash = program.find('/');
      const auto located = slash == std::string::npos
                               ? std::nullopt
                               : Locate(
                                     runfiles, program.substr(0, slash),
                                     program.substr(slash + 1));
      if (!located) {
        problems.push_back(
            std::format(
                "'{}' checks a run with '{}', which is not among this run's "
                "inputs",
                suite, program));
        continue;
      }
      command.front() = located->path.string();
      check = std::move(command);
    }

    // A case that names no runs makes one, handed nothing of its own.
    std::map<std::string, std::vector<std::string>> runs;
    if (const YAML::Node named = Section(entry, "runs"); named.IsMap()) {
      for (const auto& run : named) {
        std::vector<std::string>& handed = runs[run.first.as<std::string>()];
        for (const YAML::Node& argument : run.second) {
          handed.push_back(argument.as<std::string>());
        }
      }
    } else {
      runs.emplace("default", std::vector<std::string>{});
    }
    for (const auto& [run, handed] : runs) {
      std::vector<std::string> arguments = ListOf({described}, "arguments");
      std::ranges::copy(handed, std::back_inserter(arguments));
      cases.push_back(
          DesignCase{
              .name = std::format("{}:{}:{}", suite, dir_name, run),
              .design_name = suite,
              .declaration = declaration,
              .files = {},
              .arguments = std::move(arguments),
              .check = check});
    }
  }
  return cases;
}

}  // namespace

auto NameOf(DesignStep step) -> std::string_view {
  switch (step) {
    case DesignStep::kBuild:
      return "build";
    case DesignStep::kRun:
      return "run";
    case DesignStep::kCheck:
      return "check";
  }
  std::unreachable();
}

auto LoadDesignCorpus(const bazel::tools::cpp::runfiles::Runfiles& runfiles)
    -> DesignCorpus {
  DesignCorpus corpus;
  corpus.cases = BenchmarkSuiteCases(runfiles, corpus.problems);
  const YAML::Node suites = YAML::LoadFile(
      runfiles.Rlocation("_main/tests/external_design_suites.yaml"));
  for (const auto& suite : suites) {
    std::ranges::move(
        DescribedSuiteCases(
            runfiles, suite.first.as<std::string>(), suite.second,
            corpus.problems),
        std::back_inserter(corpus.cases));
  }

  const YAML::Node record = YAML::LoadFile(
      runfiles.Rlocation("_main/tests/paths/llvm.external_designs.yaml"));
  for (const auto& entry : record) {
    const auto name = entry.first.as<std::string>();
    const auto at = StepNamed(LastOf({entry.second}, "stops").value_or(""));
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
        DesignStop{
            .at = *at, .said = LastOf({entry.second}, "says").value_or("")});
  }

  const YAML::Node options = YAML::LoadFile(
      runfiles.Rlocation("_main/tests/external_design_options.yaml"));
  for (const auto& entry : options) {
    const auto design = entry.first.as<std::string>();
    const auto is_of = [&](const DesignCase& one) {
      return one.design_name == design;
    };
    if (std::ranges::none_of(corpus.cases, is_of)) {
      corpus.problems.push_back(
          std::format(
              "options are listed for '{}', which is not a design", design));
      continue;
    }
    std::vector<std::string> listed;
    for (const YAML::Node& option : entry.second) {
      listed.push_back(option.as<std::string>());
    }
    corpus.options.emplace(design, std::move(listed));
  }
  return corpus;
}

auto LayOutDesign(const DesignCase& design, const std::filesystem::path& work)
    -> std::filesystem::path {
  std::ofstream(work / "lyra.toml") << design.declaration;
  const std::filesystem::path run = work / "run";
  std::filesystem::create_directories(run / "_execute");
  for (const std::filesystem::path& file : design.files) {
    std::filesystem::create_symlink(file, run / file.filename());
  }
  return run;
}

}  // namespace lyra::test
