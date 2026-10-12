// What a design costs to compile as it gets larger is a property no program it
// simulates can observe, and one no single compile shows: a compiler that
// writes a file per repetition, or declares a loop in the square of its count,
// is correct at every size and fast at a small one. So each design here states
// one thing and takes N steps of it, and is compiled at N and at twice N, both
// runs asked to report on themselves. A step that repeats what is stated once
// adds nothing to what a compile leaves behind; a step unlike the others adds
// what its own statements are worth, which the design says kind by kind. Either
// way a step adds at most proportionally to what a compile spends.
//
// What is held of what a compile leaves is every kind of thing the run reports
// holding, whichever kinds those are: a kind the design does not name must not
// follow its size, and one it names must follow it as named. So a kind of thing
// the compiler starts to make per step is held from the day it is reported,
// without this file naming it.
//
// The two runs are made back to back on one machine, which is what lets time
// be held at all: a duration says how fast the machine is, the ratio of two
// taken together says how the cost grows.

#include <algorithm>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <gtest/gtest.h>
#include <map>
#include <memory>
#include <nlohmann/json.hpp>
#include <optional>
#include <ostream>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
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

// A file may carry the size it was compiled at -- a bound, an index -- so the
// bytes a run leaves are held to a fraction of themselves rather than to
// equality. Bytes that follow the repetition double.
constexpr std::uint64_t kBytesAllowedNumerator = 5;
constexpr std::uint64_t kBytesAllowedDenominator = 4;

// Twice the repetition may cost twice the work, and a cost in its square costs
// four times. The bound sits between the two.
constexpr std::uint64_t kProportionalBound = 3;

// A stage shorter than this at the larger size is mostly the machine's noise,
// and a ratio of two such readings says nothing.
constexpr std::uint64_t kShortestTimedStageMicroseconds = 50'000;

// A peak moves by the allocator's own granularity between two runs of one
// thing, so a small stage's is held to an absolute allowance as well.
constexpr std::uint64_t kPeakAllowanceBytes = 16ULL << 20;

// How many times a pair of runs is made before a time or a peak is said to
// have outgrown its bound. A loaded machine can stretch one run of a pair; it
// does not stretch the larger one every time.
constexpr int kAttempts = 3;

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

// A step adds this many of a kind, which the source shows: one procedure
// written is one callable.
struct ThisMany {
  std::uint64_t count = 0;
};

// A step adds to a kind by an amount nobody can read off the source, as the
// bytes a unit leaves follow how much its bodies say.
struct SomeAmount {};

using Added = std::variant<ThisMany, SomeAmount>;

// What one more step of a design's size adds, by kind.
using Adds = std::map<std::string, Added>;

struct GrowthCase {
  std::string name;
  std::filesystem::path design;
  std::uint64_t size = 0;
  // What the design's own file says a step adds. A kind it does not name is
  // one a step adds none of.
  Adds each_adds;
  // How the larger run is recorded as departing from that today, for every
  // backend.
  std::map<Backend, std::set<std::string>> recorded;
};

// The kinds a run is read for beyond the ones it reports holding.
constexpr std::string_view kUnits = "units";
constexpr std::string_view kFiles = "files";
constexpr std::string_view kBytes = "bytes";
// A unit's own body and every one lowered only to be held against a unit.
constexpr std::string_view kBodiesLowered = "bodies lowered";

// What one compile left behind and spent, as the run itself recorded it.
struct Reading {
  // How many of each kind of thing its units left and hold, taken together. A
  // unit's name carries the size it was specialized on, so two runs name their
  // units differently and only the whole of each can be held to the other. A
  // kind a unit holds in one of its forms is named `form.kind`.
  std::map<std::string, std::uint64_t> left;
  // The stages that ran alone, each with the peak it reached where the
  // platform offers one.
  std::map<std::string, std::optional<std::uint64_t>> stage_peak_bytes;
  // The time under every span name, summed over the run.
  std::map<std::string, std::uint64_t> span_microseconds;
};

auto CommandFor(
    Backend backend, const std::filesystem::path& design, std::uint64_t size,
    const std::filesystem::path& dir) -> std::vector<std::string> {
  std::vector<std::string> command;
  switch (backend) {
    case Backend::kCpp:
      command = {"emit", "cpp", "-o", (dir / "project").string()};
      break;
    case Backend::kLlvm:
      command = {
          "build",
          "--backend",
          "llvm",
          "-o",
          (dir / "program").string(),
          "--cache-dir",
          (dir / "cache").string()};
      break;
  }
  const std::vector<std::string> common = {
      "--top",
      "Top",
      std::format("-GN={}", size),
      "--time-trace",
      (dir / "trace.json").string(),
      "--time-trace-granularity",
      "0",
      "--stats-file",
      (dir / "stats.json").string(),
      design.string()};
  command.insert(command.end(), common.begin(), common.end());
  return command;
}

auto Joined(const std::vector<std::string>& command) -> std::string {
  std::string out = "lyra";
  for (const std::string& word : command) {
    out += ' ';
    out += word;
  }
  return out;
}

auto Compile(
    const std::filesystem::path& lyra, const std::vector<std::string>& command,
    const std::filesystem::path& dir) -> std::expected<Reading, std::string> {
  std::filesystem::remove_all(dir);
  std::filesystem::create_directories(dir);
  const auto ran = RunChildProcess(lyra, command, 600s);
  if (ran.termination != TerminationKind::kExitedNormally) {
    return std::unexpected(
        std::format(
            "{} did not compile:\n{}{}", Joined(command), ran.stdout_text,
            ran.stderr_text));
  }
  const nlohmann::json stats = ReadJson(dir / "stats.json");
  const nlohmann::json trace = ReadJson(dir / "trace.json");
  if (stats.is_discarded() || trace.is_discarded()) {
    return std::unexpected(
        std::format("{} wrote no report of itself", Joined(command)));
  }

  Reading reading;
  for (const nlohmann::json& unit : stats.at("units")) {
    reading.left[std::string(kUnits)] += 1;
    for (const nlohmann::json& artifact : unit.at("artifacts")) {
      reading.left[std::string(kFiles)] += 1;
      reading.left[std::string(kBytes)] +=
          artifact.at("bytes").get<std::uint64_t>();
    }
    for (const auto& [form, held] : unit.at("counts").items()) {
      for (const auto& [kind, count] : held.items()) {
        reading.left[std::format("{}.{}", form, kind)] +=
            count.get<std::uint64_t>();
      }
    }
  }
  for (const nlohmann::json& stage : stats.at("stages")) {
    reading.stage_peak_bytes[stage.at("name").get<std::string>()] =
        stage.contains("peak_rss_bytes")
            ? std::optional{stage.at("peak_rss_bytes").get<std::uint64_t>()}
            : std::nullopt;
  }
  // The trace's writer adds one event per span name holding that name's time
  // summed over the run, under the name with this prefix.
  constexpr std::string_view kTotal = "Total ";
  // The span a body's lowering runs under, one event each.
  constexpr std::string_view kBodyLowered = "lower to HIR";
  for (const nlohmann::json& event : trace.at("traceEvents")) {
    const std::string name = event.value("name", "");
    if (name.starts_with(kTotal)) {
      reading.span_microseconds[name.substr(kTotal.size())] =
          event.at("dur").get<std::uint64_t>();
    } else if (name == kBodyLowered) {
      reading.left[std::string(kBodiesLowered)] += 1;
    }
  }
  return reading;
}

auto Milliseconds(std::uint64_t microseconds) -> std::string {
  return std::format("{:.1f} ms", static_cast<double>(microseconds) / 1e3);
}

auto Mebibytes(std::uint64_t bytes) -> std::string {
  return std::format(
      "{:.1f} MiB", static_cast<double>(bytes) / static_cast<double>(1 << 20));
}

auto TimeOf(const Reading& reading, const std::string& span) -> std::uint64_t {
  const auto found = reading.span_microseconds.find(span);
  return found == reading.span_microseconds.end() ? 0 : found->second;
}

auto OutgrewProportion(std::uint64_t small, std::uint64_t large) -> bool {
  return large >= kProportionalBound * small;
}

// What departed, in the words a record names it by, each with the numbers that
// show it.
using Departed = std::map<std::string, std::string>;

// The form a kind is held in, which is what stands before the dot of its name,
// and nothing for a kind of the run as a whole.
auto FormOf(std::string_view kind) -> std::string_view {
  const std::size_t dot = kind.find('.');
  return dot == std::string_view::npos ? std::string_view{}
                                       : kind.substr(0, dot);
}

auto LeftOf(const Reading& reading, const std::string& kind) -> std::uint64_t {
  const auto found = reading.left.find(kind);
  return found == reading.left.end() ? 0 : found->second;
}

// Where what the larger run left departs from what the design says its extra
// steps add. These are counts and sizes, the same on every run of one
// compiler, and the difference of two runs holds nothing that does not follow
// the size.
//
// A kind is held both ways. One the design names and that did not follow the
// size as named is a departure as much as one it does not name that did: a
// statement nothing answers to would otherwise stand after the compiler
// stopped making, or stopped reporting, what it speaks of.
auto DepartedInWhatWasLeft(
    const GrowthCase& growth, const Reading& small, const Reading& large)
    -> Departed {
  std::set<std::string> kinds;
  for (const auto& [kind, count] : small.left) {
    kinds.insert(kind);
  }
  for (const auto& [kind, count] : large.left) {
    kinds.insert(kind);
  }
  // A design speaks of every form a unit may pass through, and a path takes a
  // unit through some of them, so what is said of a form this run never
  // reported is said of another path.
  std::set<std::string_view> forms;
  for (const std::string& kind : kinds) {
    forms.insert(FormOf(kind));
  }
  for (const auto& [kind, added] : growth.each_adds) {
    if (forms.contains(FormOf(kind))) {
      kinds.insert(kind);
    }
  }

  Departed departed;
  for (const std::string& kind : kinds) {
    const std::uint64_t before = LeftOf(small, kind);
    const std::uint64_t after = LeftOf(large, kind);
    const auto said = growth.each_adds.find(kind);
    if (said == growth.each_adds.end()) {
      const bool followed = kind == kBytes ? after * kBytesAllowedDenominator >
                                                 before * kBytesAllowedNumerator
                                           : after > before;
      if (followed) {
        departed[kind] = std::format("{} -> {}", before, after);
      }
      continue;
    }
    std::visit(
        lyra::Overloaded{
            [&](const ThisMany& many) {
              const std::uint64_t expected =
                  before + (many.count * growth.size);
              if (after != expected) {
                departed[kind] = std::format(
                    "{} -> {}, where {} a step makes it {}", before, after,
                    many.count, expected);
              }
            },
            [&](const SomeAmount&) {
              if (after <= before) {
                departed[kind] = std::format(
                    "{} -> {}, where a step is said to add", before, after);
              }
            }},
        said->second);
  }
  return departed;
}

// How a stage's time and its peak are named in a record.
constexpr std::string_view kTimeOf = "time ";
constexpr std::string_view kPeakOf = "peak ";

// Whether `name` is something a run spent rather than something it left.
auto IsSpent(std::string_view name) -> bool {
  return name.starts_with(kTimeOf) || name.starts_with(kPeakOf);
}

// What the larger run spent beyond its proportion, per stage that ran alone.
// These move with the machine.
auto DepartedInWhatWasSpent(const Reading& small, const Reading& large)
    -> Departed {
  Departed outgrew;
  for (const auto& [stage, peak] : large.stage_peak_bytes) {
    const std::uint64_t took = TimeOf(large, stage);
    const std::uint64_t took_before = TimeOf(small, stage);
    if (took >= kShortestTimedStageMicroseconds &&
        OutgrewProportion(took_before, took)) {
      outgrew[std::format("{}{}", kTimeOf, stage)] = std::format(
          "{} -> {}", Milliseconds(took_before), Milliseconds(took));
    }
    const auto before = small.stage_peak_bytes.find(stage);
    if (peak.has_value() && before != small.stage_peak_bytes.end() &&
        before->second.has_value() &&
        *peak > *before->second + kPeakAllowanceBytes &&
        OutgrewProportion(*before->second, *peak)) {
      outgrew[std::format("{}{}", kPeakOf, stage)] =
          std::format("{} -> {}", Mebibytes(*before->second), Mebibytes(*peak));
    }
  }
  return outgrew;
}

// The spans whose time outgrew its proportion, longest first: a stage that
// outgrew is located by the step inside it that did.
auto SpansThatOutgrew(const Reading& small, const Reading& large)
    -> std::string {
  std::vector<std::pair<std::uint64_t, std::string>> spans;
  for (const auto& [span, took] : large.span_microseconds) {
    if (took >= kShortestTimedStageMicroseconds &&
        OutgrewProportion(TimeOf(small, span), took)) {
      spans.emplace_back(took, span);
    }
  }
  std::ranges::sort(spans, std::greater{});
  std::string out;
  for (const auto& [took, span] : spans) {
    out += std::format(
        "    {}: {} -> {}\n", span, Milliseconds(TimeOf(small, span)),
        Milliseconds(took));
  }
  return out;
}

// One design compiled at its size and at twice that, one run after the other.
struct Pair {
  Reading small;
  Reading large;
};

// Holds one design on one backend to what is recorded for it. Nothing where
// it holds; otherwise what differs, with the numbers and the two commands
// that show it.
auto CheckGrowth(
    const std::filesystem::path& lyra, const GrowthCase& growth,
    Backend backend) -> std::optional<std::string> {
  auto scratch = MakeScratchDir();
  if (!scratch.has_value()) {
    return scratch.error();
  }
  const std::vector<std::string> small_command =
      CommandFor(backend, growth.design, growth.size, *scratch / "small");
  const std::vector<std::string> large_command =
      CommandFor(backend, growth.design, 2 * growth.size, *scratch / "large");

  const auto compile_pair = [&]() -> std::expected<Pair, std::string> {
    auto small = Compile(lyra, small_command, *scratch / "small");
    if (!small.has_value()) {
      return std::unexpected(small.error());
    }
    auto large = Compile(lyra, large_command, *scratch / "large");
    if (!large.has_value()) {
      return std::unexpected(large.error());
    }
    return Pair{.small = std::move(*small), .large = std::move(*large)};
  };

  const std::set<std::string>& recorded = growth.recorded.at(backend);
  auto pair = compile_pair();
  if (!pair.has_value()) {
    return pair.error();
  }
  // A time or a peak outgrew only if it does on every attempt, so the pair is
  // compiled again while something not recorded is still standing.
  Departed spent = DepartedInWhatWasSpent(pair->small, pair->large);
  const auto all_recorded = [&] {
    return std::ranges::all_of(spent, [&](const auto& entry) {
      return recorded.contains(entry.first);
    });
  };
  for (int attempt = 1; attempt < kAttempts && !all_recorded(); ++attempt) {
    pair = compile_pair();
    if (!pair.has_value()) {
      return pair.error();
    }
    const Departed again = DepartedInWhatWasSpent(pair->small, pair->large);
    std::erase_if(
        spent, [&](const auto& entry) { return !again.contains(entry.first); });
  }

  Departed departed = DepartedInWhatWasLeft(growth, pair->small, pair->large);
  departed.insert(spent.begin(), spent.end());
  const std::string spans = SpansThatOutgrew(pair->small, pair->large);

  std::set<std::string> found;
  for (const auto& [name, numbers] : departed) {
    found.insert(name);
  }
  const lyra::test::Departures against_record = HoldToRecord(recorded, found);
  std::string differs;
  for (const std::string& name : against_record.not_recorded) {
    differs += std::format(
        "  does not follow its size as the design says: {} ({})\n", name,
        departed.at(name));
  }
  for (const std::string& name : against_record.no_longer_found) {
    // A time or a peak recorded as owed is still owed on a run that stayed
    // under the bound: whether one cost crosses it depends on the machine as
    // much as on the cost, so it is the fix that takes its line out.
    if (IsSpent(name)) {
      continue;
    }
    differs += std::format(
        "  recorded as departing and no longer does, so its line goes: {}\n",
        name);
  }
  if (differs.empty()) {
    return std::nullopt;
  }
  return std::format(
      "'{}' on the {} backend, compiled at N={} and N={}:\n{}"
      "  spans that outgrew their proportion:\n{}"
      "  the two runs:\n    {}\n    {}\n",
      growth.name, NameOf(backend), growth.size, 2 * growth.size, differs,
      spans.empty() ? "    none\n" : spans, Joined(small_command),
      Joined(large_command));
}

auto Trimmed(std::string_view text) -> std::string_view {
  constexpr std::string_view kBlank = " \t\r";
  const std::size_t first = text.find_first_not_of(kBlank);
  if (first == std::string_view::npos) {
    return {};
  }
  return text.substr(first, text.find_last_not_of(kBlank) - first + 1);
}

// What a design's file says one more step of its size adds: the kinds on its
// `// @each-adds:` lines, separated by commas, each followed by how many where
// the source shows how many. A design with no such line says a step adds
// nothing. A line that names nothing is reported into `problems`, since it
// would read as that.
auto EachAddsOf(
    const std::filesystem::path& design, std::vector<std::string>& problems)
    -> Adds {
  constexpr std::string_view kKey = "// @each-adds:";
  Adds adds;
  std::ifstream source(design);
  std::string line;
  while (std::getline(source, line)) {
    if (!line.starts_with(kKey)) {
      continue;
    }
    std::string_view rest = std::string_view(line).substr(kKey.size());
    bool named = false;
    while (true) {
      const std::size_t comma = rest.find(',');
      const std::string_view item = Trimmed(rest.substr(0, comma));
      const std::size_t last_word = item.find_last_of(' ');
      std::uint64_t count = 0;
      const std::string_view number = last_word == std::string_view::npos
                                          ? std::string_view{}
                                          : item.substr(last_word + 1);
      const bool counted =
          !number.empty() &&
          std::from_chars(number.data(), number.data() + number.size(), count)
                  .ptr == number.data() + number.size();
      const std::string_view kind =
          counted ? Trimmed(item.substr(0, last_word)) : item;
      if (!kind.empty()) {
        adds[std::string(kind)] =
            counted ? Added{ThisMany{.count = count}} : Added{SomeAmount{}};
        named = true;
      }
      if (comma == std::string_view::npos) {
        break;
      }
      rest = rest.substr(comma + 1);
    }
    if (!named) {
      problems.push_back(
          std::format(
              "{} names no kind in '{}'", design.filename().string(), line));
    }
  }
  return adds;
}

// Every design under `designs_root`, at the size the file beside them gives
// it, with what its own file says a step adds and how each path's record under
// `paths_root` says it departs from that. A design given no size, and a size
// or a record naming no design, are reported into `problems`.
auto LoadGrowthCases(
    const std::filesystem::path& designs_root,
    const std::filesystem::path& paths_root, std::vector<std::string>& problems)
    -> std::vector<GrowthCase> {
  std::map<std::string, GrowthCase> cases;
  for (const auto& entry : std::filesystem::directory_iterator(designs_root)) {
    if (entry.path().extension() == ".sv") {
      const std::string name = entry.path().stem().string();
      cases[name] = GrowthCase{
          .name = name,
          .design = entry.path(),
          .size = 0,
          .each_adds = EachAddsOf(entry.path(), problems),
          .recorded = {{Backend::kCpp, {}}, {Backend::kLlvm, {}}}};
    }
  }

  const auto sizes = LoadByName<std::uint64_t>(designs_root / "sizes.yaml");
  for (const auto& [name, size] : sizes) {
    if (const auto found = cases.find(name); found != cases.end()) {
      found->second.size = size;
    } else {
      problems.push_back(
          std::format("'{}' is given a size and is not a design", name));
    }
  }
  for (const Backend backend : {Backend::kCpp, Backend::kLlvm}) {
    const std::string file = std::format("{}.growth.yaml", NameOf(backend));
    for (const auto& [name, outgrows] :
         LoadByName<std::vector<std::string>>(paths_root / file)) {
      if (const auto found = cases.find(name); found != cases.end()) {
        found->second.recorded.at(backend).insert(
            outgrows.begin(), outgrows.end());
      } else {
        problems.push_back(
            std::format("{} records '{}', which is not a design", file, name));
      }
    }
  }

  std::vector<GrowthCase> loaded;
  for (auto& [name, growth] : cases) {
    if (growth.size == 0) {
      problems.push_back(std::format("design '{}' is given no size", name));
    } else {
      loaded.push_back(std::move(growth));
    }
  }
  return loaded;
}

// The designs and their record, found where the build put them.
struct Corpus {
  std::vector<GrowthCase> cases;
  std::vector<std::string> problems;
};

auto LoadCorpus() -> Corpus {
  Corpus corpus;
  std::string error;
  const std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&error)};
  if (!runfiles) {
    corpus.problems.push_back(
        std::format("failed to create runfiles: {}", error));
    return corpus;
  }
  corpus.cases = LoadGrowthCases(
      runfiles->Rlocation("_main/tests/growth"),
      runfiles->Rlocation("_main/tests/paths"), corpus.problems);
  return corpus;
}

}  // namespace

namespace lyra::test {

// One design on one backend. The test framework finds how to print a parameter
// by the parameter's own namespace, which is why this one has a name.
struct GrowthSubject {
  GrowthCase growth;
  Backend backend;

  friend auto PrintTo(const GrowthSubject& subject, std::ostream* out) -> void {
    *out << subject.growth.name << " on " << NameOf(subject.backend);
  }
};

}  // namespace lyra::test

namespace {

using lyra::test::GrowthSubject;

auto Subjects() -> std::vector<GrowthSubject> {
  std::vector<GrowthSubject> subjects;
  for (const GrowthCase& growth : LoadCorpus().cases) {
    for (const Backend backend : {Backend::kCpp, Backend::kLlvm}) {
      subjects.push_back(GrowthSubject{.growth = growth, .backend = backend});
    }
  }
  return subjects;
}

// A design given no size, or a record naming one that is not there, would
// leave the bound unmeasured for it and every other test here still passing.
TEST(GrowthRecord, NamesEveryDesignAndNothingElse) {
  const Corpus corpus = LoadCorpus();
  for (const std::string& problem : corpus.problems) {
    ADD_FAILURE() << problem;
  }
  EXPECT_FALSE(corpus.cases.empty());
}

class Growth : public testing::TestWithParam<GrowthSubject> {};

TEST_P(Growth, IsHeldToItsSize) {
  const auto lyra = lyra::test::ResolveLyra();
  ASSERT_TRUE(std::filesystem::exists(lyra)) << lyra.string();
  if (auto failure = CheckGrowth(lyra, GetParam().growth, GetParam().backend)) {
    ADD_FAILURE() << *failure;
  }
}

INSTANTIATE_TEST_SUITE_P(
    Designs, Growth, testing::ValuesIn(Subjects()),
    [](const testing::TestParamInfo<GrowthSubject>& subject) {
      return std::format(
          "{}_{}", NameOf(subject.param.backend), subject.param.growth.name);
    });

}  // namespace
