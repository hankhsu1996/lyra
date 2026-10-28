#include "conformance_observation.hpp"

#include <filesystem>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
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

auto EndOf(const ProcessOutcome& outcome) -> RunEnd {
  return std::visit(
      Overloaded{
          [](const CouldNotRun& failed) -> RunEnd {
            return HarnessFailed{failed.reason};
          },
          [](const Exited& exited) -> RunEnd {
            return Ran{.exit_code = exited.code};
          },
          [](const Signalled& signalled) -> RunEnd {
            return ToolFailed{
                std::format("ended by signal {}", signalled.signal)};
          },
          [](const TimedOut&) -> RunEnd { return DidNotFinish{}; },
          [](const TestTimedOut&) -> RunEnd { return DidNotFinish{}; }},
      outcome.end);
}

void RecordCommand(
    Observation& seen, const std::filesystem::path& exe,
    std::span<const std::string> args, const ProcessOutcome& outcome) {
  seen.evidence += Transcript(exe, args, outcome);
  seen.written += outcome.stdout_text;
  seen.written += outcome.stderr_text;
}

void ReadMarkers(std::string_view program_output, Observation& seen) {
  seen.checks_passed =
      program_output.find(kAllChecksPassed) != std::string_view::npos;
  seen.ready_for_the_report =
      program_output.find(kReadyForTheReport) != std::string_view::npos;
}

}  // namespace lyra::test
