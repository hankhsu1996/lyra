#pragma once

#include <chrono>
#include <filesystem>
#include <optional>
#include <span>
#include <string>

namespace lyra::test {

enum class TerminationKind {
  kExitedNormally,
  kExitedNonZero,
  kSignaled,
  kTimedOut,
  kSpawnFailed,
  kWaitFailed,
};

struct ProcessOutcome {
  TerminationKind termination = TerminationKind::kSpawnFailed;
  int exit_code = 0;
  int signal_number = 0;
  std::string stdout_text;
  std::string stderr_text;
};

// Spawns `exe` with `args` and collects both streams. A child that outlives
// `timeout` is asked to end, killed if it has not shortly after, and reported
// as timed out, so a hung run fails rather than hanging the suite. It starts in
// `working_directory` where one is given, and where this process is otherwise.
//
// Whether it timed out is decided here, by the clock, and nothing the child
// does once asked changes it, an exit of 0 included. What it writes when asked
// is kept as evidence: a program Lyra built says the simulation time its run
// had reached, which only the program knows, and a kill with no request first
// would leave the record saying only that the limit passed.
auto RunChildProcess(
    const std::filesystem::path& exe, std::span<const std::string> args,
    std::chrono::seconds timeout = std::chrono::seconds{30},
    const std::optional<std::filesystem::path>& working_directory =
        std::nullopt) -> ProcessOutcome;

// Runs `program` with `args` in a shell that has first limited its own address
// space to `kibibytes`, so a program that asks for more is refused by its host.
auto RunUnderAddressLimit(
    const std::filesystem::path& program, std::span<const std::string> args,
    int kibibytes, std::chrono::seconds timeout = std::chrono::seconds{60})
    -> ProcessOutcome;

}  // namespace lyra::test
