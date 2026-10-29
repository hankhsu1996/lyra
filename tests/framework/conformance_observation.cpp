#include "conformance_observation.hpp"

#include <algorithm>
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

#include "conformance_case.hpp"
#include "lyra/base/overloaded.hpp"
#include "process.hpp"

namespace lyra::test {

auto MissingInput(const ConformanceCase& test_case)
    -> std::optional<HarnessFailed> {
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
                  "a kill from outside it, whose cause is not known"};
            }
            return ToolFailed{
                std::format("ended by signal {}", signalled.signal)};
          },
          [](const TimedOut&) -> RunEnd {
            return StoppedAtALimit{"the time it was given"};
          },
          [](const TestTimedOut&) -> RunEnd {
            return StoppedAtALimit{"the time it was given"};
          }},
      outcome.end);
  if (exhausted && !std::holds_alternative<HarnessFailed>(ended)) {
    return StoppedAtALimit{"the memory it was given"};
  }
  return ended;
}

void RecordCommand(
    Observation& seen, const std::filesystem::path& exe,
    std::span<const std::string> args, const ProcessOutcome& outcome) {
  seen.evidence += Transcript(exe, args, outcome);
  seen.written += outcome.stdout_text;
  seen.written += outcome.stderr_text;
}

void ReadOutputs(
    const ConformanceCase& test_case, const std::filesystem::path& directory,
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

}  // namespace lyra::test
