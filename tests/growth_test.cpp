// What a design costs to compile as it gets larger is a property no program it
// simulates can observe, and one no single compile shows: a compiler that
// writes a file per repetition, or declares a loop in the square of its count,
// is correct at every size and fast at a small one. So each design here states
// one thing and takes N steps of it, and is compiled at N and at twice N, both
// runs asked to report on themselves. A step that repeats what is stated once
// adds nothing to what a compile leaves behind; a step unlike the others adds
// what its own statements are worth, which the design says kind by kind.
//
// What is held of what a compile leaves is every kind of thing the run reports
// holding, and of what it did every kind of work it reports counting,
// whichever kinds those are: a kind the design does not name must not follow
// its size, and one it names must follow it as named. So a kind of thing the
// compiler starts to make per step is held from the day it is reported,
// without this file naming it. How deep the text a run writes nests is held the
// same way, read off the text, since a compiler reading it bounds its depth and
// no design is bounded by one, and so is how deep a unit's code nests, which
// the run reports and every consumer walking that code pays for.
//
// Everything is held by what the larger run left beyond the smaller, so what
// both leave alike -- a library every program compiles, however large --
// cancels.
//
// Everything held here is a count the run reports or a property of the text it
// wrote, so one tree gives one answer. What a run spent -- a stage's time, its
// peak -- is read off the machine running it, and a busy one moves it from one
// run of a tree to the next, so it is no part of this test.

#include <algorithm>
#include <array>
#include <charconv>
#include <cstddef>
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
#include <set>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "conformance/framework/process.hpp"
#include "lyra/base/overloaded.hpp"
#include "tests/framework/cli_fixture.hpp"
#include "tests/framework/emitted_nesting.hpp"
#include "tests/framework/held_record.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;
using conformance::framework::ExitCodeOf;
using conformance::framework::MakeScratchDir;
using conformance::framework::RunChildProcess;
using lyra::test::HoldToRecord;
using lyra::test::LoadByName;
using lyra::test::ReadJson;

namespace {

// How a design is compiled: by the C++ backend as far as its emitted project,
// or by the execution backend.
enum class Path : std::uint8_t {
  kCpp,
  kLlvm,
};

constexpr std::array<Path, 2> kPaths = {Path::kCpp, Path::kLlvm};

auto NameOf(Path path) -> std::string_view {
  switch (path) {
    case Path::kCpp:
      return "cpp";
    case Path::kLlvm:
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

// What a design's size counts: one more of what it repeats, or one more bit of
// a width. The least a step can leave in a file is a byte of the first and a
// bit of the second, and a file may also carry the size it was compiled at --
// a bound, an index -- in a few bytes whatever the size is. So the bytes a run
// leaves beyond the smaller run are held under one such least amount a step,
// which a file carrying its size stays under and anything made per step does
// not.
enum class Counted : std::uint8_t {
  kRepetitions,
  kBits,
};

struct GrowthCase {
  std::string name;
  std::filesystem::path design;
  std::uint64_t size = 0;
  Counted counted = Counted::kRepetitions;
  // What the design's own file says a step adds. A kind it does not name is
  // one a step adds none of.
  Adds each_adds;
  // How the larger run is recorded as departing from that today, along every
  // path.
  std::map<Path, std::set<std::string>> recorded;
};

// The kinds a run is read for beyond the ones it reports holding.
constexpr std::string_view kUnits = "units";
constexpr std::string_view kFiles = "files";
constexpr std::string_view kBytes = "bytes";
// How deep the text a run writes nests at the deepest, over every file, which
// a compiler reading the text bounds and a design does not.
constexpr std::string_view kDeepestNesting = "deepest nesting";
// How deep one form of a unit nests at the deepest, over every unit.
constexpr std::string_view kNestingDepth = "nesting depth";

// What one compile left behind and did, as the run itself recorded it: how
// many of each kind of thing its units left and hold, taken together, and how
// often each kind of work ran. A unit's name carries the size it was
// specialized on, so two runs name their units differently and only the whole
// of each can be held to the other. A kind a unit holds in one of its forms is
// named `form.kind`.
using Reading = std::map<std::string, std::uint64_t>;

auto CommandFor(
    Path path, const std::filesystem::path& design, std::uint64_t size,
    const std::filesystem::path& dir) -> std::vector<std::string> {
  std::vector<std::string> command;
  switch (path) {
    case Path::kCpp:
      command = {"emit", "cpp", "-o", (dir / "project").string()};
      break;
    case Path::kLlvm:
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
      "--stats-file",
      (dir / "stats.json").string(),
      design.string()};
  command.insert(command.end(), common.begin(), common.end());
  return command;
}

// A design's text at `size`. A count the source writes out -- the members of a
// structure, the operands of a chain, the arguments of a call -- is no
// parameter the language can generate from, so the harness writes it: each
// `[[piece|separator]]` is the piece written `size` times, joined by the
// separator, which is what follows the last `|` and may be empty, with `{i}` in
// the piece each step's index from zero; and `{last}` anywhere is the last
// step's index.
auto TextAt(std::string_view text, std::uint64_t size) -> std::string {
  std::string out;
  while (true) {
    const std::size_t open = text.find("[[");
    out += text.substr(0, open);
    if (open == std::string_view::npos) {
      break;
    }
    const std::size_t close = text.find("]]", open);
    const std::string_view inner = text.substr(open + 2, close - open - 2);
    const std::size_t bar = inner.rfind('|');
    const std::string_view piece = inner.substr(0, bar);
    const std::string_view separator = bar == std::string_view::npos
                                           ? std::string_view{}
                                           : inner.substr(bar + 1);
    for (std::uint64_t step = 0; step < size; ++step) {
      if (step > 0) {
        out += separator;
      }
      std::string_view rest = piece;
      for (std::size_t at = rest.find("{i}"); at != std::string_view::npos;
           at = rest.find("{i}")) {
        out += rest.substr(0, at);
        out += std::to_string(step);
        rest = rest.substr(at + 3);
      }
      out += rest;
    }
    text = text.substr(close + 2);
  }
  const std::string last = std::to_string(size - 1);
  for (std::size_t at = out.find("{last}"); at != std::string::npos;
       at = out.find("{last}", at + last.size())) {
    out.replace(at, 6, last);
  }
  return out;
}

// `design` written at `size` to `path`, which a run then compiles.
auto WrittenAt(
    const std::filesystem::path& design, std::uint64_t size,
    const std::filesystem::path& path) -> std::filesystem::path {
  std::ifstream source(design);
  const std::string text{
      std::istreambuf_iterator<char>(source), std::istreambuf_iterator<char>()};
  std::ofstream(path) << TextAt(text, size);
  return path;
}

auto Joined(const std::vector<std::string>& command) -> std::string {
  std::string out = "lyra";
  for (const std::string& word : command) {
    out += ' ';
    out += word;
  }
  return out;
}

// What one compile left behind: its reading, and the bytes of each file it
// wrote, by the file's name, which a report shows to say where bytes went.
struct Left {
  Reading reading;
  std::map<std::string, std::uint64_t> file_bytes;
};

auto Compile(
    const std::filesystem::path& lyra, const std::vector<std::string>& command,
    const std::filesystem::path& dir) -> std::expected<Left, std::string> {
  std::filesystem::remove_all(dir);
  std::filesystem::create_directories(dir);
  const auto ran = RunChildProcess(lyra, command);
  if (ExitCodeOf(ran) != 0) {
    return std::unexpected(
        std::format(
            "{} did not compile:\n{}{}", Joined(command), ran.stdout_text,
            ran.stderr_text));
  }
  const nlohmann::json stats = ReadJson(dir / "stats.json");
  if (stats.is_discarded()) {
    return std::unexpected(
        std::format("{} wrote no report of itself", Joined(command)));
  }

  Left left;
  Reading& reading = left.reading;
  for (const nlohmann::json& unit : stats.at("units")) {
    reading[std::string(kUnits)] += 1;
    for (const nlohmann::json& artifact : unit.at("artifacts")) {
      const auto bytes = artifact.at("bytes").get<std::uint64_t>();
      reading[std::string(kFiles)] += 1;
      reading[std::string(kBytes)] += bytes;
      left.file_bytes[artifact.at("name").get<std::string>()] = bytes;
      // What the unit wrote as text lies where the run wrote its project; a
      // path that writes none leaves nothing to read there.
      std::ifstream text_file(
          dir / "project" / artifact.at("name").get<std::string>(),
          std::ios::binary);
      if (!text_file) {
        continue;
      }
      const std::string text{
          std::istreambuf_iterator<char>(text_file),
          std::istreambuf_iterator<char>()};
      std::uint64_t& deepest = reading[std::string(kDeepestNesting)];
      deepest = std::max<std::uint64_t>(
          deepest, lyra::test::DeepestNesting(text).depth);
    }
    for (const auto& [form, held] : unit.at("counts").items()) {
      for (const auto& [kind, count] : held.items()) {
        reading[std::format("{}.{}", form, kind)] += count.get<std::uint64_t>();
      }
    }
    // How deep a unit nests is no count, so what the run left is the deepest
    // over every unit, as for the text it wrote.
    for (const auto& [form, depth] : unit.at("nesting_depth").items()) {
      std::uint64_t& deepest =
          reading[std::format("{}.{}", form, kNestingDepth)];
      deepest = std::max<std::uint64_t>(deepest, depth.get<std::uint64_t>());
    }
  }
  for (const auto& [kind, count] : stats.at("work").items()) {
    reading[kind] += count.get<std::uint64_t>();
  }
  return left;
}

// The files of `left` that `other` has none of at that name and size, with
// their bytes. A unit's name carries the size, so a unit specialized on it
// differs by name whether or not its bytes moved.
auto FilesNotAlike(
    const std::map<std::string, std::uint64_t>& left,
    const std::map<std::string, std::uint64_t>& other) -> std::string {
  std::string out;
  for (const auto& [name, bytes] : left) {
    const auto found = other.find(name);
    if (found == other.end() || found->second != bytes) {
      out += std::format(" {} {}", name, bytes);
    }
  }
  return out;
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

// The least one step can leave in a file: its name, and how many steps each
// leaving it leave one byte between them.
struct Least {
  std::string_view name;
  std::uint64_t steps_per_byte = 1;
};

auto LeastAStepLeaves(Counted counted) -> Least {
  switch (counted) {
    case Counted::kRepetitions:
      return Least{.name = "byte", .steps_per_byte = 1};
    case Counted::kBits:
      return Least{.name = "bit", .steps_per_byte = 8};
  }
  std::unreachable();
}

auto LeftOf(const Reading& reading, const std::string& kind) -> std::uint64_t {
  const auto found = reading.find(kind);
  return found == reading.end() ? 0 : found->second;
}

// Where what the larger run left departs from what the design says its extra
// steps add. The difference of two runs holds nothing that does not follow the
// size.
//
// A kind is held both ways. One the design names and that did not follow the
// size as named is a departure as much as one it does not name that did: a
// statement nothing answers to would otherwise stand after the compiler
// stopped making, or stopped reporting, what it speaks of.
auto DepartedInWhatWasLeft(
    const GrowthCase& growth, const Reading& small, const Reading& large)
    -> Departed {
  std::set<std::string> kinds;
  for (const auto& [kind, count] : small) {
    kinds.insert(kind);
  }
  for (const auto& [kind, count] : large) {
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
      const bool followed =
          kind == kBytes
              ? after > before &&
                    (after - before) *
                            LeastAStepLeaves(growth.counted).steps_per_byte >=
                        growth.size
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

// Holds one design along one path to what is recorded for it. Nothing where
// it holds; otherwise what differs, with the numbers and the two commands
// that show it.
auto CheckGrowth(
    const std::filesystem::path& lyra, const GrowthCase& growth, Path path)
    -> std::optional<std::string> {
  auto scratch = MakeScratchDir();
  if (!scratch.has_value()) {
    return scratch.error();
  }
  const std::vector<std::string> small_command = CommandFor(
      path, WrittenAt(growth.design, growth.size, *scratch / "small.sv"),
      growth.size, *scratch / "small");
  const std::vector<std::string> large_command = CommandFor(
      path, WrittenAt(growth.design, 2 * growth.size, *scratch / "large.sv"),
      2 * growth.size, *scratch / "large");

  auto small = Compile(lyra, small_command, *scratch / "small");
  if (!small.has_value()) {
    return small.error();
  }
  auto large = Compile(lyra, large_command, *scratch / "large");
  if (!large.has_value()) {
    return large.error();
  }
  const Departed departed =
      DepartedInWhatWasLeft(growth, small->reading, large->reading);

  // The numbers behind a kind, so whoever edits the record reads what moved
  // rather than a verdict; for bytes, how much a step against the least a step
  // can leave, and the files that differ.
  const auto numbers = [&](const std::string& kind,
                           std::string out) -> std::string {
    if (kind == kBytes) {
      const std::uint64_t before = LeftOf(small->reading, kind);
      const std::uint64_t after = LeftOf(large->reading, kind);
      out += std::format(
          ", {:.2f} bytes a step against less than one {} a step; files that "
          "differ by name or size, at N:{}; at 2N:{}",
          (static_cast<double>(after) - static_cast<double>(before)) /
              static_cast<double>(growth.size),
          LeastAStepLeaves(growth.counted).name,
          FilesNotAlike(small->file_bytes, large->file_bytes),
          FilesNotAlike(large->file_bytes, small->file_bytes));
    }
    return out;
  };

  std::set<std::string> found;
  for (const auto& [name, shown] : departed) {
    found.insert(name);
  }
  const lyra::test::Departures against_record =
      HoldToRecord(growth.recorded.at(path), found);
  std::string differs;
  for (const std::string& name : against_record.not_recorded) {
    differs += std::format(
        "  does not follow its size as the design says: {} ({})\n", name,
        numbers(name, departed.at(name)));
  }
  // A kind neither run reports was not measured, which says nothing of
  // whether it still departs.
  for (const std::string& name : against_record.no_longer_found) {
    differs +=
        !small->reading.contains(name) && !large->reading.contains(name)
            ? std::format(
                  "  recorded as departing and neither run reports it: {}\n",
                  name)
            : std::format(
                  "  recorded as departing and no longer does, so its line "
                  "goes: {} ({})\n",
                  name,
                  numbers(
                      name, std::format(
                                "{} -> {}", LeftOf(small->reading, name),
                                LeftOf(large->reading, name))));
  }
  if (differs.empty()) {
    return std::nullopt;
  }
  return std::format(
      "'{}' along the {} path, compiled at N={} and N={}:\n{}"
      "  the two runs:\n    {}\n    {}\n",
      growth.name, NameOf(path), growth.size, 2 * growth.size, differs,
      Joined(small_command), Joined(large_command));
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
    std::span<const std::string> lines, std::string_view design,
    std::vector<std::string>& problems) -> Adds {
  constexpr std::string_view kKey = "// @each-adds:";
  Adds adds;
  for (const std::string& line : lines) {
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
      problems.push_back(std::format("{} names no kind in '{}'", design, line));
    }
  }
  return adds;
}

// What a design's file says its size counts: bits where a `// @counted-in:
// bits` line says so, and repetitions otherwise. A line naming anything else is
// reported into `problems`, since it would read as repetitions.
auto CountedOf(
    std::span<const std::string> lines, std::string_view design,
    std::vector<std::string>& problems) -> Counted {
  constexpr std::string_view kKey = "// @counted-in:";
  Counted counted = Counted::kRepetitions;
  for (const std::string& line : lines) {
    if (!line.starts_with(kKey)) {
      continue;
    }
    if (Trimmed(std::string_view(line).substr(kKey.size())) == "bits") {
      counted = Counted::kBits;
    } else {
      problems.push_back(
          std::format(
              "{} counts in something other than bits in '{}'", design, line));
    }
  }
  return counted;
}

auto LinesOf(const std::filesystem::path& file) -> std::vector<std::string> {
  std::vector<std::string> lines;
  std::ifstream source(file);
  for (std::string line; std::getline(source, line);) {
    lines.push_back(std::move(line));
  }
  return lines;
}

// Every design under `designs_root`, at the size the file beside them gives
// it, with what its own file says the size counts and a step adds, and how each
// path's record under `paths_root` says it departs from that. A design given no
// size, and a size or a record naming no design, are reported into `problems`.
auto LoadGrowthCases(
    const std::filesystem::path& designs_root,
    const std::filesystem::path& paths_root, std::vector<std::string>& problems)
    -> std::vector<GrowthCase> {
  std::map<std::string, GrowthCase> cases;
  for (const auto& entry : std::filesystem::directory_iterator(designs_root)) {
    if (entry.path().extension() == ".sv") {
      const std::string name = entry.path().stem().string();
      const std::string file = entry.path().filename().string();
      const std::vector<std::string> lines = LinesOf(entry.path());
      cases[name] = GrowthCase{
          .name = name,
          .design = entry.path(),
          .size = 0,
          .counted = CountedOf(lines, file, problems),
          .each_adds = EachAddsOf(lines, file, problems),
          .recorded = {{Path::kCpp, {}}, {Path::kLlvm, {}}}};
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
  for (const Path path : kPaths) {
    const std::string file = std::format("{}.growth.yaml", NameOf(path));
    for (const auto& [name, outgrows] :
         LoadByName<std::vector<std::string>>(paths_root / file)) {
      if (const auto found = cases.find(name); found != cases.end()) {
        found->second.recorded.at(path).insert(
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

// One design along one path. The test framework finds how to print a parameter
// by the parameter's own namespace, which is why this one has a name.
struct GrowthSubject {
  GrowthCase growth;
  Path path;

  friend auto PrintTo(const GrowthSubject& subject, std::ostream* out) -> void {
    *out << subject.growth.name << " along " << NameOf(subject.path);
  }
};

}  // namespace lyra::test

namespace {

using lyra::test::GrowthSubject;

auto Subjects() -> std::vector<GrowthSubject> {
  std::vector<GrowthSubject> subjects;
  for (const GrowthCase& growth : LoadCorpus().cases) {
    for (const Path path : kPaths) {
      subjects.push_back(GrowthSubject{.growth = growth, .path = path});
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
  if (auto failure = CheckGrowth(lyra, GetParam().growth, GetParam().path)) {
    ADD_FAILURE() << *failure;
  }
}

INSTANTIATE_TEST_SUITE_P(
    Designs, Growth, testing::ValuesIn(Subjects()),
    [](const testing::TestParamInfo<GrowthSubject>& subject) {
      return std::format(
          "{}_{}", NameOf(subject.param.path), subject.param.growth.name);
    });

}  // namespace
