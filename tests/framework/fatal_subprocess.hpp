#pragma once

// POSIX-only subprocess helper for testing runtime-fatal contracts.
// Uses fork/pipe/waitpid. Not available on non-POSIX platforms.

#include <functional>
#include <string>

namespace lyra::test {

// Whether the helper itself ran successfully.
enum class FatalSubprocessRunStatus {
  kRan,
  kPipeFailed,
  kForkFailed,
  kWaitFailed,
};

// How the child process terminated.
enum class ChildTerminationKind {
  kExitedNormally,  // exit(0)
  kExitedNonZero,   // exit(N) where N != 0
  kSignaled,        // killed by signal (e.g., SIGABRT from abort())
};

struct FatalSubprocessResult {
  FatalSubprocessRunStatus run_status = FatalSubprocessRunStatus::kRan;
  ChildTerminationKind termination = ChildTerminationKind::kExitedNormally;
  int exit_code = 0;
  int signal_number = 0;
  std::string captured_stderr;
};

// Run `action` in a forked child process and capture its stderr.
// Returns structured result with execution status and termination details.
//
// This is a small, focused POSIX-only helper for testing runtime fatal
// contracts (e.g., missing DPI scope). It is not a general crash-test
// framework.
auto RunExpectingFatal(std::function<void()> action) -> FatalSubprocessResult;

}  // namespace lyra::test
