#pragma once

#include <chrono>
#include <filesystem>
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
// `timeout` is killed and reported as such, so a hung run fails as a timeout
// rather than hanging the suite.
auto RunChildProcess(
    const std::filesystem::path& exe, std::span<const std::string> args,
    std::chrono::seconds timeout = std::chrono::seconds{30}) -> ProcessOutcome;

// Runs `program` with `args` in a shell that has first limited its own address
// space to `kibibytes`, so a program that asks for more is refused by its host.
auto RunUnderAddressLimit(
    const std::filesystem::path& program, std::span<const std::string> args,
    int kibibytes, std::chrono::seconds timeout = std::chrono::seconds{60})
    -> ProcessOutcome;

}  // namespace lyra::test
