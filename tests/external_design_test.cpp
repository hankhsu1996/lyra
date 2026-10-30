// Whether Lyra builds and runs the designs other simulators are measured on.
// The conformance corpus cannot say: each of its cases is one small unit, and
// what stops a real design is a hierarchy, a size, or a way of writing that no
// clause-by-clause case thinks to write.
//
// A case is built in the directory its declaration is written to, with the
// command a failure names. What it is recorded as stopping on is held exactly:
// a case that stops somewhere else, says something else, or stops no longer
// fails until the record says so, which makes the record the account of how
// far each design gets.
//
// A text the standard does not settle is refused, and so is one it forbids; a
// reader may ask for either by name. A design written in such a text is built
// with the front end's options listed for it, the way someone who cannot edit
// it would build it, so what stands behind that text is still reached. The list
// is held the way the record is, so an option the design no longer needs fails
// until it goes.

#include "tests/framework/external_design.hpp"

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
#include <iterator>
#include <memory>
#include <optional>
#include <ostream>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <variant>
#include <vector>

#include "conformance/framework/process.hpp"
#include "tests/framework/cli_fixture.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;
using conformance::framework::ExitCodeOf;
using conformance::framework::MakeScratchDir;
using conformance::framework::ProcessOutcome;
using conformance::framework::RunShellScript;
using conformance::framework::TestTimedOut;
using conformance::framework::TimedOut;
using lyra::test::DesignCase;
using lyra::test::DesignCorpus;
using lyra::test::DesignStep;
using lyra::test::DesignStop;
using lyra::test::LayOutDesign;
using lyra::test::NameOf;
using namespace std::chrono_literals;

namespace {

// What a step may address, in kibibytes. A build's memory is the compiler's
// own and the host compiler's it starts, and nothing else bounds either: one
// design that asks for more than the machine has takes the machine with it,
// and every case after it. Past this a design stops at the step that asked,
// which is the finding. It is set under what the smallest machine these run on
// holds, so a case ends the same way wherever it is run.
constexpr std::uint64_t kStepAddressLimitKib = 12ULL << 20;

// How many of a design's units a build lowers and compiles at once. A build is
// told this by whoever runs it, so like the limit above it is the same on every
// machine. Its peak memory is that many units' at once, all within the limit.
constexpr int kBuildWidth = 4;

// How much of what a step said is shown. A design that stops can say thousands
// of lines; the first name what a build refused, and the last say how a run
// ended, the time it had reached included when it was asked to end.
constexpr std::size_t kFirstLinesShown = 30;
constexpr std::size_t kLastLinesShown = 10;

// How long a step may take; a design past it stops at that step, which is the
// finding, and how long each case took is said beside it.
//
// A build is bounded well past the slowest one measured, so that which side of
// the limit a design falls on does not turn on the machine: what it catches is
// a build that will not end. A run is bounded by what it should cost. Each
// case is the suite's quick check, which Verilator's own published figures put
// under a second for most designs and at about thirty for the largest, so ten
// times the largest is the limit. A run past it is either stuck or slower than
// that. Waiting longer does not say which, and the time it says it had reached
// when asked to end does.
auto TimeLimitOf(DesignStep step) -> std::chrono::seconds {
  switch (step) {
    case DesignStep::kBuild:
      return 1800s;
    case DesignStep::kRun:
    case DesignStep::kCheck:
      return 300s;
  }
  std::unreachable();
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
    DesignStep step, const std::filesystem::path& dir,
    const std::string& command) -> ProcessOutcome {
  return RunShellScript(
      std::format(
          "ulimit -v {} && cd {} && {}", kStepAddressLimitKib,
          ShellQuoted(dir.string()), command),
      {}, TimeLimitOf(step));
}

auto Said(DesignStep step, const ProcessOutcome& outcome) -> std::string {
  if (std::holds_alternative<TimedOut>(outcome.end)) {
    return std::format(
        "did not end within {} s\n{}{}", TimeLimitOf(step).count(),
        outcome.stdout_text, outcome.stderr_text);
  }
  if (std::holds_alternative<TestTimedOut>(outcome.end)) {
    return std::format(
        "was ended because the test ran out of time\n{}{}", outcome.stdout_text,
        outcome.stderr_text);
  }
  return outcome.stdout_text + outcome.stderr_text;
}

auto ReadWhole(const std::filesystem::path& path) -> std::string {
  const std::ifstream in(path);
  std::ostringstream text;
  text << in.rdbuf();
  return text.str();
}

auto LyraCommand(
    const std::filesystem::path& lyra, std::string_view command,
    const std::vector<std::string>& options) -> std::string {
  std::string line = std::format("{} {}", ShellQuoted(lyra.string()), command);
  for (const std::string& option : options) {
    line += ' ';
    line += ShellQuoted(option);
  }
  return line;
}

// The build of a case, from the directory its declaration is written to. A
// design of this size draws warnings by the thousand, and what stops it is an
// error, so only errors are asked for.
auto BuildCommand(
    const std::filesystem::path& lyra, const std::vector<std::string>& options)
    -> std::string {
  return LyraCommand(
      lyra,
      std::format(
          "build --backend llvm --progress=none -Wnone -j {} --cache-dir cache "
          "-o sim",
          kBuildWidth),
      options);
}

// Builds the case in `work` with the front end's `options`, and runs it there
// as the suite would, ending at the first step that fails. The run's output is
// kept where the suite's check reads it.
auto Carry(
    const std::filesystem::path& lyra, const DesignCase& design,
    const std::vector<std::string>& options, const std::filesystem::path& work)
    -> std::optional<DesignStop> {
  const std::filesystem::path run = LayOutDesign(design, work);
  const auto built =
      RunFrom(DesignStep::kBuild, work, BuildCommand(lyra, options));
  if (ExitCodeOf(built) != 0) {
    return DesignStop{
        .at = DesignStep::kBuild, .said = Said(DesignStep::kBuild, built)};
  }

  // The program is the step's process itself, so that asking the step to end
  // asks the program, and the step ends when the program does.
  std::string command = "exec ../sim";
  for (const std::string& argument : design.arguments) {
    command += ' ';
    command += ShellQuoted(argument);
  }
  const auto ran =
      RunFrom(DesignStep::kRun, run, command + " > _execute/stdout.log 2>&1");
  if (ExitCodeOf(ran) != 0) {
    return DesignStop{
        .at = DesignStep::kRun,
        .said = Said(DesignStep::kRun, ran) +
                ReadWhole(run / "_execute" / "stdout.log")};
  }

  if (design.check) {
    // The suite runs a check as a program of its own, so each is written in
    // whatever its first line names.
    const auto checked =
        RunFrom(DesignStep::kCheck, run, ShellQuoted(design.check->string()));
    if (ExitCodeOf(checked) != 0) {
      return DesignStop{
          .at = DesignStep::kCheck, .said = Said(DesignStep::kCheck, checked)};
    }
  }
  return std::nullopt;
}

// Of the front end's `options` the case in `work` was built with, each the
// front end reads it without all the same. Whether an option is needed is a
// question for the front end alone, so asking costs no lowering.
auto NotNeeded(
    const std::filesystem::path& lyra, const std::vector<std::string>& options,
    const std::filesystem::path& work) -> std::vector<std::string> {
  std::vector<std::string> not_needed;
  for (const std::string& option : options) {
    std::vector<std::string> others;
    std::ranges::copy_if(
        options, std::back_inserter(others),
        [&](const std::string& other) { return other != option; });
    const auto checked = RunFrom(
        DesignStep::kBuild, work, LyraCommand(lyra, "check -Wnone", others));
    if (ExitCodeOf(checked) == 0) {
      not_needed.push_back(option);
    }
  }
  return not_needed;
}

// Where the first `count` lines of `text` end.
auto EndOfFirstLines(std::string_view text, std::size_t count) -> std::size_t {
  std::size_t end = 0;
  for (std::size_t line = 0; line < count && end < text.size(); ++line) {
    const std::size_t next = text.find('\n', end);
    end = next == std::string_view::npos ? text.size() : next + 1;
  }
  return end;
}

// Where the last `count` lines of `text` begin.
auto StartOfLastLines(std::string_view text, std::size_t count) -> std::size_t {
  std::size_t start = text.ends_with('\n') ? text.size() - 1 : text.size();
  for (std::size_t line = 0; line < count && start > 0; ++line) {
    const std::size_t previous = text.rfind('\n', start - 1);
    start = previous == std::string_view::npos ? 0 : previous;
  }
  return start == 0 ? 0 : start + 1;
}

auto LastLines(std::string_view text) -> std::string {
  return std::string(text.substr(StartOfLastLines(text, kLastLinesShown)));
}

// What a step said, its first and its last lines where it said more.
auto Shown(std::string_view text) -> std::string {
  const std::size_t first = EndOfFirstLines(text, kFirstLinesShown);
  const std::size_t last = StartOfLastLines(text, kLastLinesShown);
  if (last <= first) {
    return std::string(text);
  }
  return std::format("{}...\n{}", text.substr(0, first), text.substr(last));
}

// Holds what a case did against what is recorded of it. Nothing where they
// agree; otherwise what differs and what the record would have to say.
auto Differs(
    const std::optional<DesignStop>& stop,
    const std::optional<DesignStop>& recorded) -> std::optional<std::string> {
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
      was, Shown(stop->said));
}

auto LoadCorpus() -> DesignCorpus {
  std::string error;
  const std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&error)};
  if (!runfiles) {
    DesignCorpus corpus;
    corpus.problems.push_back(
        std::format("failed to create runfiles: {}", error));
    return corpus;
  }
  return lyra::test::LoadDesignCorpus(*runfiles);
}

}  // namespace

namespace lyra::test {

// One case with what is recorded of it and the front end's options its design
// is built with. The test framework finds how to print a parameter by the
// parameter's own namespace, which is why this one has a name.
struct DesignSubject {
  DesignCase design;
  std::optional<DesignStop> recorded;
  std::vector<std::string> options;

  friend auto PrintTo(const DesignSubject& subject, std::ostream* out) -> void {
    *out << subject.design.name;
  }
};

}  // namespace lyra::test

namespace {

using lyra::test::DesignSubject;

auto Subjects() -> std::vector<DesignSubject> {
  const DesignCorpus corpus = LoadCorpus();
  std::vector<DesignSubject> subjects;
  for (const DesignCase& design : corpus.cases) {
    const auto recorded = corpus.recorded.find(design.name);
    const auto options = corpus.options.find(design.design_name);
    subjects.push_back(
        DesignSubject{
            .design = design,
            .recorded = recorded == corpus.recorded.end()
                            ? std::nullopt
                            : std::optional{recorded->second},
            .options = options == corpus.options.end()
                           ? std::vector<std::string>{}
                           : options->second});
  }
  return subjects;
}

// A record naming a case the suite does not have, or options listed for a
// design it does not have, would hold nothing, and every other test here would
// still pass.
TEST(DesignRecord, NamesOnlyWhatTheSuiteHas) {
  const DesignCorpus corpus = LoadCorpus();
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
  const std::optional<DesignStop> stop =
      Carry(lyra, subject.design, subject.options, *work);
  const auto took = std::chrono::duration_cast<std::chrono::seconds>(
      std::chrono::steady_clock::now() - began);
  // Written as each case ends, so a run that is cut short has still said how
  // far the cases before it got. A case that stops is said with where it was
  // built, since one that stops as recorded is looked into from there as much
  // as one that does not, and with the last lines its step wrote, which say how
  // it ended.
  std::cout << std::format(
                   "{}: {} after {} s{}", subject.design.name,
                   stop ? std::format("stops at its {}", NameOf(stop->at))
                        : std::string("builds, runs and passes its check"),
                   took.count(),
                   stop ? std::format(
                              ", built in\n{}\n{}", work->string(),
                              LastLines(stop->said))
                        : std::string())
            << std::endl;

  if (const auto differs = Differs(stop, subject.recorded)) {
    ADD_FAILURE() << subject.design.name << " " << *differs
                  << (stop
                          ? std::format(
                                "\nIt is built from the directory above by\n{}",
                                BuildCommand(lyra, subject.options))
                          : std::string());
  }
  for (const std::string& option : NotNeeded(lyra, subject.options, *work)) {
    ADD_FAILURE() << subject.design.design_name << " is built with " << option
                  << " and no longer needs it, so it goes.";
  }
  // A design's objects and program are gigabytes, and nobody looks into one
  // that got all the way.
  if (!stop) {
    std::error_code left;
    std::filesystem::remove_all(*work, left);
    EXPECT_FALSE(left) << work->string()
                       << " was not removed: " << left.message();
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
