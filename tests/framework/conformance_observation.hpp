#pragma once

#include <cstdint>
#include <filesystem>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "conformance_case.hpp"
#include "process.hpp"

namespace lyra::test {

// A report as one tool's reader placed it: what the standard fixes of a report,
// read off whatever that tool writes -- each where the tool gave it -- and the
// text it was read from. The message is the program's own, cut out of the
// tool's wording at boundaries the reader knows, since a claim naming one is
// met only by that text whole; a reader that cannot tell where it ends gives
// none, and so does one that knows the report carries no message of the
// program's.
struct ObservedReport {
  ReportKind kind = ReportKind::kInfo;
  std::optional<std::uint64_t> line;
  std::optional<std::uint64_t> time;
  std::optional<std::string> scope;
  std::optional<std::string> message;
  std::string written;
};

// How far a run of one case got. The first two say nothing of the program:
// the harness could not start the tool, or the tool has no use model the case
// needs. The rest are the tool's answer to the program.
struct HarnessFailed {
  std::string why;
};

// A use model the standard requires every tool to offer and this one does not,
// with the clause requiring it.
struct ToolLacks {
  std::string use_model;
  std::string clause;
};

struct Rejected {};

// The tool ended in a way no program asks for: a crash, a report of a bug of
// its own, a refusal of a construct it does not carry out, a build of its own
// output that failed.
struct ToolFailed {
  std::string how;
};

struct DidNotFinish {};

struct Ran {
  int exit_code = 0;
};

using RunEnd = std::variant<
    HarnessFailed, ToolLacks, Rejected, ToolFailed, DidNotFinish, Ran>;

// Why the files a case is made of cannot be handed to a tool, where they
// cannot. A tool asked to read a file that is not there refuses the command,
// which reads exactly like refusing the program, so a reader checks first and
// never lets the harness's failure stand as the tool's answer.
auto MissingInput(const ConformanceCase& test_case)
    -> std::optional<HarnessFailed>;

// How a command a tool's reader ran ended, before its output is read: a child
// that never started is the harness's failure, one a signal or the time limit
// ended is not the tool answering, and an exit is.
auto EndOf(const ProcessOutcome& outcome) -> RunEnd;

// What a run of one case on one tool showed, in terms no tool owns. Lines the
// reader recognises as its tool's own report yet cannot place are kept apart
// from the program's output, since a claim about reports cannot be decided
// while one of them might be the report it asks about. What the tool and the
// program wrote is kept apart from the commands that were run, which with it
// are the evidence a verdict is read from without running the case again.
struct Observation {
  RunEnd end;
  bool checks_passed = false;
  bool ready_for_the_report = false;
  std::vector<ObservedReport> reports;
  std::vector<std::string> unplaced;
  std::string written;
  std::string evidence;
};

// Adds a command a tool's reader ran, and what it wrote, to the observation.
void RecordCommand(
    Observation& seen, const std::filesystem::path& exe,
    std::span<const std::string> args, const ProcessOutcome& outcome);

// Whether the program printed the case's two markers. They are text the case
// itself writes to standard output, so no tool's reader reads them its own way.
void ReadMarkers(std::string_view program_output, Observation& seen);

}  // namespace lyra::test
