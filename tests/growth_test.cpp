// What a design costs to compile as it gets larger is a property no program it
// simulates can observe, and one no single compile shows: a compiler that
// writes a file per repetition, or declares a loop in the square of its count,
// is correct at every size and fast at a small one. So each design here states
// one thing and repeats it N times, and is compiled at N and at twice N, both
// runs asked to report on themselves. Repeating what is stated once adds
// nothing to what a compile leaves behind, and at most proportionally to what
// it spends.
//
// The two runs are made back to back on one machine, which is what lets time
// be held at all: a duration says how fast the machine is, the ratio of two
// taken together says how the cost grows.

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <format>
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

struct GrowthCase {
  std::string name;
  std::filesystem::path design;
  std::uint64_t size = 0;
  // What the larger run is recorded as exceeding today, for every backend.
  std::map<Backend, std::set<std::string>> recorded;
};

// What one compile left behind and spent, as the run itself recorded it.
struct Reading {
  // What its units left, taken together. A unit's name carries the size it
  // was specialized on, so two runs name their units differently and only the
  // whole of each can be held to the other.
  std::size_t units = 0;
  std::size_t files = 0;
  std::uint64_t bytes = 0;
  // How many bodies the run lowered, a unit's own and every one lowered only
  // to be held against a unit.
  std::size_t bodies_lowered = 0;
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
    reading.units += 1;
    for (const nlohmann::json& artifact : unit.at("artifacts")) {
      reading.files += 1;
      reading.bytes += artifact.at("bytes").get<std::uint64_t>();
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
      reading.bodies_lowered += 1;
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

// What was exceeded, in the words a record names it by, each with the two
// numbers that show it.
using Outgrowths = std::map<std::string, std::string>;

// What the larger run left behind or lowered that the smaller did not. These
// are counts and sizes, the same on every run of one compiler.
auto LeftBehindOutgrowths(const Reading& small, const Reading& large)
    -> Outgrowths {
  Outgrowths outgrew;
  if (large.units > small.units) {
    outgrew["units"] = std::format("{} -> {}", small.units, large.units);
  }
  if (large.files > small.files) {
    outgrew["files"] = std::format("{} -> {}", small.files, large.files);
  }
  if (large.bytes * kBytesAllowedDenominator >
      small.bytes * kBytesAllowedNumerator) {
    outgrew["bytes"] = std::format("{} -> {}", small.bytes, large.bytes);
  }
  if (large.bodies_lowered > small.bodies_lowered) {
    outgrew["bodies lowered"] =
        std::format("{} -> {}", small.bodies_lowered, large.bodies_lowered);
  }
  return outgrew;
}

// What the larger run spent beyond its proportion, per stage that ran alone.
// These move with the machine.
auto SpentOutgrowths(const Reading& small, const Reading& large) -> Outgrowths {
  Outgrowths outgrew;
  for (const auto& [stage, peak] : large.stage_peak_bytes) {
    const std::uint64_t took = TimeOf(large, stage);
    const std::uint64_t took_before = TimeOf(small, stage);
    if (took >= kShortestTimedStageMicroseconds &&
        OutgrewProportion(took_before, took)) {
      outgrew[std::format("time {}", stage)] = std::format(
          "{} -> {}", Milliseconds(took_before), Milliseconds(took));
    }
    const auto before = small.stage_peak_bytes.find(stage);
    if (peak.has_value() && before != small.stage_peak_bytes.end() &&
        before->second.has_value() &&
        *peak > *before->second + kPeakAllowanceBytes &&
        OutgrewProportion(*before->second, *peak)) {
      outgrew[std::format("peak {}", stage)] =
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
  Outgrowths spent = SpentOutgrowths(pair->small, pair->large);
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
    const Outgrowths again = SpentOutgrowths(pair->small, pair->large);
    std::erase_if(
        spent, [&](const auto& entry) { return !again.contains(entry.first); });
  }

  Outgrowths outgrew = LeftBehindOutgrowths(pair->small, pair->large);
  outgrew.insert(spent.begin(), spent.end());
  const std::string spans = SpansThatOutgrew(pair->small, pair->large);

  std::set<std::string> found;
  for (const auto& [name, numbers] : outgrew) {
    found.insert(name);
  }
  const lyra::test::Departures departures = HoldToRecord(recorded, found);
  std::string differs;
  for (const std::string& name : departures.not_recorded) {
    differs +=
        std::format("  outgrew its size: {} ({})\n", name, outgrew.at(name));
  }
  for (const std::string& name : departures.no_longer_found) {
    differs += std::format(
        "  recorded as outgrowing and no longer does, so its line goes: {}\n",
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

// Every design under `designs_root`, at the size the file beside them gives
// it, with what each path's record under `paths_root` says it exceeds. A
// design given no size, and a size or a record naming no design, are reported
// into `problems`.
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
