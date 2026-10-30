#include "conformance/framework/observation.hpp"

#include <algorithm>
#include <chrono>
#include <csignal>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <optional>
#include <span>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "conformance/framework/case.hpp"
#include "conformance/framework/overloaded.hpp"
#include "conformance/framework/process.hpp"

namespace conformance::framework {

namespace {

// What a signal usually means for the program it ended, for the signals a
// crash or a broken tool gives, so a reader need not know their numbers.
auto SignalMeaning(int signal) -> std::string_view {
  switch (signal) {
    case SIGSEGV:
    case SIGBUS:
      return " (an invalid memory access)";
    case SIGABRT:
      return " (the program aborted)";
    case SIGFPE:
      return " (an arithmetic error)";
    case SIGILL:
      return " (an illegal instruction)";
    case SIGPIPE:
      return " (it wrote to a pipe nobody was reading)";
    default:
      return "";
  }
}

}  // namespace

auto MissingInput(const Case& test_case) -> std::optional<HarnessFailed> {
  std::vector<std::filesystem::path> inputs = test_case.supporting_sources;
  inputs.push_back(test_case.entry);
  inputs.insert(
      inputs.end(), test_case.link_sources.begin(),
      test_case.link_sources.end());
  if (test_case.library_map) {
    inputs.push_back(*test_case.library_map);
  }
  for (const std::filesystem::path& input : inputs) {
    if (!input.is_absolute() || !std::filesystem::exists(input)) {
      return HarnessFailed{std::format(
          "'{}' is not a file a tool started elsewhere can read",
          input.string())};
    }
  }
  return std::nullopt;
}

auto EndOf(
    const ProcessOutcome& outcome,
    std::span<const std::string_view> out_of_memory) -> RunEnd {
  const bool exhausted =
      std::ranges::any_of(out_of_memory, [&](std::string_view words) {
        return outcome.stdout_text.find(words) != std::string::npos ||
               outcome.stderr_text.find(words) != std::string::npos;
      });
  const RunEnd ended = std::visit(
      Overloaded{
          [](const CouldNotRun& failed) -> RunEnd {
            return HarnessFailed{failed.reason};
          },
          [](const Exited& exited) -> RunEnd {
            return Ran{.exit_code = exited.code};
          },
          // A process is not killed outright by anything of its own: the
          // kernel does it to one past its memory, and a person or a
          // supervisor to one they stopped. Which of those it was, the signal
          // does not say.
          [](const Signalled& signalled) -> RunEnd {
            if (signalled.signal == SIGKILL) {
              return StoppedAtALimit{
                  "something outside it, for a reason that is not known"};
            }
            return ToolFailed{std::format(
                "a program the tool started was ended by signal {}{}",
                signalled.signal, SignalMeaning(signalled.signal))};
          },
          [](const TimedOut&) -> RunEnd {
            return StoppedAtALimit{"its time limit"};
          },
          [](const TestTimedOut&) -> RunEnd {
            return StoppedAtALimit{"its time limit"};
          }},
      outcome.end);
  if (exhausted && !std::holds_alternative<HarnessFailed>(ended)) {
    return StoppedAtALimit{"its memory limit"};
  }
  return ended;
}

auto RunStep(
    Observation& seen, const std::filesystem::path& exe,
    std::span<const std::string> args, const std::filesystem::path& directory,
    std::optional<std::chrono::steady_clock::time_point> deadline)
    -> ProcessOutcome {
  seen.steps.push_back(
      Step{
          .program = exe,
          .arguments = {args.begin(), args.end()},
          .directory = directory,
          .outcome =
              RunChildProcess(exe, args, TimeLeft(deadline), directory)});
  return seen.steps.back().outcome;
}

auto WrittenBy(const Observation& seen) -> std::string {
  std::string written;
  for (const Step& step : seen.steps) {
    written += step.outcome.stdout_text;
    written += step.outcome.stderr_text;
  }
  return written;
}

auto Evidence(const Observation& seen) -> std::string {
  std::string evidence;
  for (const Step& step : seen.steps) {
    evidence += Transcript(step);
  }
  return evidence;
}

void ReadOutputs(
    const Case& test_case, const std::filesystem::path& directory,
    Observation& seen) {
  for (const ExpectedOutput& expected : test_case.expected_outputs) {
    std::ifstream in(directory / expected.name, std::ios::binary);
    if (!in) {
      seen.outputs.push_back(
          WrittenOutput{.name = expected.name, .text = std::nullopt});
      continue;
    }
    std::ostringstream text;
    text << in.rdbuf();
    seen.outputs.push_back(
        WrittenOutput{.name = expected.name, .text = std::move(text).str()});
  }
}

void ReadMarkers(std::string_view program_output, Observation& seen) {
  seen.checks_passed =
      program_output.find(kAllChecksPassed) != std::string_view::npos;
  seen.ready_for_the_report =
      program_output.find(kReadyForTheReport) != std::string_view::npos;
}

}  // namespace conformance::framework
