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

auto RunChildProcess(
    const std::filesystem::path& exe, std::span<const std::string> args,
    std::chrono::seconds timeout = std::chrono::seconds{30}) -> ProcessOutcome;

}  // namespace lyra::test
