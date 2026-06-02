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

// Spawns `exe` with `args`. When `cwd` is set the child runs from that
// directory (used by expect.files cases so $fopen("foo.txt") writes inside
// the per-case sandbox rather than the test runner's CWD).
auto RunChildProcess(
    const std::filesystem::path& exe, std::span<const std::string> args,
    std::chrono::seconds timeout = std::chrono::seconds{30},
    std::optional<std::filesystem::path> cwd = std::nullopt) -> ProcessOutcome;

}  // namespace lyra::test
